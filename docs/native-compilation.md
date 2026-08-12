# Native compilation

Shen/Scheme can compile Shen sources ahead of time to Chez object files. This
is an explicit workflow: normal REPL evaluation, `script`, `eval`, and Shen
`load` continue to use the ordinary dynamic path.

Files ending in `.so` in this guide are Chez compiled objects. They are not
operating-system shared libraries, and application objects are not standalone
executables. Load them into a matching Shen/Scheme process.

Commands in this guide assume a source checkout. In a binary release, skip
`make` and use `./bin/shen-scheme` wherever the examples use
`./_build/bin/shen-scheme`.

## Choose a workflow

| Goal | Command | Result |
| --- | --- | --- |
| Compile one source file with ordinary rebinding semantics | `compile` | One loadable object in `compatible` mode |
| Compile one source unit with locally bound calls | `compile --mode sealed` | One loadable object with faster intra-unit calls |
| Give a unit a name, exports, metadata, and dependencies | `compile-module` | One standalone module object described by `.shenmod` |
| Load a compiled module and its compiled dependencies | `load-module` | The dependency graph loaded in dependency order |
| Bundle an ordered group of raw source files | `build-app` | One loadable application object |
| Bundle a closed `.shenmod` graph | `build-module-app` | One loadable application object with static module boundaries |

Use ordinary Shen `load` while developing unless ahead-of-time compilation is
part of what you want to test. Start with `compatible` when code relies on
redefinition. Use `sealed` or an app builder when the compiled boundary is
deliberate and performance matters.

The complete command surface is:

```text
shen-scheme compile SOURCE -o OBJECT
  [--emit-scheme SCHEME] [--mode compatible|sealed]
  [--profile release|debug|wpo|unsafe]
shen-scheme load-compiled OBJECT
shen-scheme compile-module DECLARATION -o OBJECT
  [--emit-scheme SCHEME] [--module-dir DIR]
shen-scheme load-module DECLARATION
  --module-dir MODULE_DIR --object-dir OBJECT_DIR
shen-scheme build-app MAIN [--module SOURCE ...] -o OBJECT
  [--wpo] [--profile release|debug|wpo|unsafe]
shen-scheme build-module-app DECLARATION --module-dir DIR -o OBJECT
  [--wpo] [--profile release|debug|wpo|unsafe]
```

Run `shen-scheme COMMAND --help` for the corresponding launcher help.

## Quick start

Build Shen/Scheme, compile the single-file example, then load and call it in
one process:

```sh
make
mkdir -p _build/native-examples
./_build/bin/shen-scheme compile examples/native/single-file.shen \
  -o _build/native-examples/single-file.so
./_build/bin/shen-scheme eval \
  -e '(shen-scheme.load-compiled "_build/native-examples/single-file.so")' \
  -e '(answer 5)'
```

The final expression returns `26`.

The standalone command below loads the object and then exits, so it is useful
for checking initialization but not for calling a definition afterward:

```sh
./_build/bin/shen-scheme load-compiled _build/native-examples/single-file.so
```

The complete runnable examples cover:

- [single-file compilation](../examples/native/single-file.shen);
- [compatible and sealed binding](../examples/native/binding.shen);
- [packages and top-level effects](../examples/native/package-effects.shen);
- [module dependencies and app builds](../examples/native/modules/native-example.app.shenmod);
- WPO and full-to-Petite deployment.

Follow [the examples README](../examples/native/README.md) from the repository
root, or run its examples as automated checks with:

```sh
make test-native-examples
```

## Direct source compilation

The general command is:

```text
shen-scheme compile SOURCE -o OBJECT
  [--emit-scheme SCHEME]
  [--mode compatible|sealed]
  [--profile release|debug|wpo|unsafe]
```

Generated Scheme is normally compiled directly. `--emit-scheme` also writes it
to the requested path for inspection:

```sh
./_build/bin/shen-scheme compile examples/native/single-file.shen \
  -o _build/native-examples/single-file.so \
  --emit-scheme _build/native-examples/single-file.scm
```

### Compatible and sealed modes

`compatible` is the default. Definitions use normal top-level bindings, so a
compiled caller observes a helper that is redefined later:

```sh
./_build/bin/shen-scheme compile examples/native/binding.shen \
  -o _build/native-examples/binding-compatible.so
./_build/bin/shen-scheme eval \
  -e '(shen-scheme.load-compiled "_build/native-examples/binding-compatible.so")' \
  -l examples/native/binding-update.shen \
  -e '(call-helper 1)'
```

The compatible result is `101`. Compiling the same source with `--mode sealed`
keeps the original helper binding, so the result after the update is `2`.

`sealed` gives definitions in the same compilation unit local compiled
bindings. Redefining the top-level helper later does not change calls already
compiled into that unit:

```sh
./_build/bin/shen-scheme compile examples/native/binding.shen \
  -o _build/native-examples/binding-sealed.so --mode sealed
```

Both modes install the unit's public definitions at Shen top level. Sealing
changes how definitions inside that unit call one another; it is not an access
control mechanism by itself.

## Shen packages and native modules

They solve different problems:

- A Shen `package` form is part of the Shen language. It qualifies internal
  symbols and records the package's external and internal names. Native
  compilation supports package forms and preserves package registration.
- A `.shenmod` file is portable module metadata. Its core names a compilation
  unit and lists source files, feature requirements, and module dependencies.
  Namespaced extensions carry settings used by a particular implementation.
  It does not qualify Shen symbols and does not replace `package`.

A source listed by a module can contain one or more packages. The Shen/Scheme
extension's `exports` are the native boundary; package externals are the Shen
namespace boundary. Keep both when both concepts are useful.

See [package-effects.shen](../examples/native/package-effects.shen) for a
package whose effects are restored when its compiled object is loaded.

## Module declarations

A declaration is one raw Shen form. It is read without macro expansion:

```shen
(shen.module
  (version 1)
  (name my.math)
  (requires my.core)
  (requires-features shen/scheme)
  (sources tc- "src/math.shen")
  (extension shen/scheme
    (mode sealed)
    (exports my.add my.sum)
    (metadata runtime compiletime source-kl)
    (profile release)))
```

Portable fields are order-independent. `extension` may occur once per
extension name; Shen/Scheme preserves extensions it does not recognize.

| Core field | Required/default | Meaning |
| --- | --- | --- |
| `version` | Required, exactly `1` | Portable descriptor format version |
| `name` | Required | Symbolic module name used by `requires` and file lookup |
| `requires` | Empty | Symbolic module dependencies |
| `requires-features` | Empty | Port or library features needed by the module |
| `sources` | Required, one or more paths | Ordered source paths, each following a `tc+` or `tc-` marker |
| `extension` | Optional, repeatable | Namespaced implementation settings |

The `shen/scheme` extension has these fields:

| Extension field | Default | Meaning |
| --- | --- | --- |
| `mode` | `compatible` | `compatible` or `sealed` for standalone `compile-module` |
| `exports` | `infer-all` | Exported function names, or every definition |
| `metadata` | `runtime compiletime` | Any of `runtime`, `compiletime`, and `source-kl` |
| `profile` | `release` | `release`, `debug`, `wpo`, or `unsafe` for standalone compilation |

Source paths must be relative and are resolved from the descriptor's
directory. The listed files are compiled in order, so macros,
declarations, datatypes, synonyms, and arities established by one source are
available to the sources that follow it. Put a definition before sources that
need its arity. If a function is defined repeatedly, the final definition is
compiled.

`tc+` and `tc-` are stateful markers within `sources`; each applies until the
next marker. A `tc+` source is typechecked and its inline function signatures
are included in compile-time metadata. A `tc-` source is compiled without
typechecking and does not contribute inline signatures. Explicit `declare`
forms remain effective in either mode.

An explicit Shen/Scheme export list requires `sealed` mode for a standalone module.
`compatible` standalone modules use `infer-all`, because dynamically resolved
top-level calls do not provide a private native boundary.

### Compile and load a graph

The example descriptors are:

- [native-example.core.shenmod](../examples/native/modules/native-example.core.shenmod)
- [native-example.app.shenmod](../examples/native/modules/native-example.app.shenmod)

Keep declarations beside their sources, compile each object under the name used
by the resolver, then load the root with separate module and object roots:

```sh
mkdir -p _build/native-examples/objects
./_build/bin/shen-scheme compile-module \
  examples/native/modules/native-example.core.shenmod \
  -o _build/native-examples/objects/native-example.core.so
./_build/bin/shen-scheme compile-module \
  examples/native/modules/native-example.app.shenmod \
  --module-dir examples/native/modules \
  -o _build/native-examples/objects/native-example.app.so
./_build/bin/shen-scheme eval \
  -e '(shen-scheme.load-module "examples/native/modules/native-example.app.shenmod" "examples/native/modules" "_build/native-examples/objects")' \
  -e '(run-example 32)' \
  -e '(module-events)'
```

The last two expressions return `42` and `[42]`.

For a required module named `my.core`, the two roots resolve:

```text
MODULE_DIR/my.core.shenmod
OBJECT_DIR/my.core.so
```

`compile-module` analyzes dependency declarations and source files in
topological order. It does not require or load dependency objects, install
ordinary dependency definitions, or run dependency initializers. Analysis is
transient: private dependency arities and the compiler state do not leak after
the build. Exported arities, packages, macros, datatypes, and synonyms are made
available while compiling dependants.

`load-module` does require every `.so`. It loads dependencies before their
dependants and returns a list of the loaded module names. Cycles and missing
declarations or objects are errors. If standalone direct requirements export
the same function, the later direct requirement takes precedence.

## Application objects

Application builders create one object for code that is deployed together.
The result still requires the matching Shen/Scheme runtime and must be loaded
with `load-compiled`; it is not an executable.

### Ordered raw sources

`build-app` takes a main source and zero or more module sources:

```sh
shen-scheme build-app src/main.shen \
  --module src/core.shen \
  --module src/math.shen \
  -o _build/app.so
```

Module sources are compiled in command-line order, followed by the main
source. Later sources can statically call definitions from earlier sources.
Compile-time forms are available while building, but a raw-source app does not
replay them when the object is loaded.

### Closed module graph

`build-module-app` starts from a root descriptor and traverses its complete
dependency graph:

```sh
./_build/bin/shen-scheme build-module-app \
  examples/native/modules/native-example.app.shenmod \
  --module-dir examples/native/modules \
  -o _build/native-examples/module-app.so
```

The graph is analyzed and initialized in dependency order. Descriptor exports
are static cross-module boundaries, so private dependency functions do not
become visible. Overlapping exports from direct requirements are rejected
because their static meaning would be ambiguous.

Module-app linkage is static regardless of descriptor `mode`. The app command's
profile controls the build; descriptor `mode` and `profile` control standalone
`compile-module` builds. Descriptor metadata is replayed according to each
module's `metadata` field.

To run work automatically when an app object is loaded, make that work an
explicit top-level effect. Otherwise the object simply installs its definitions
and can be called by the loading process.

## Definitions, effects, and metadata

Native sources support:

- ordinary and typed `define`;
- `defprolog`;
- `declare`, `defmacro`, `datatype`, and `synonyms`;
- `package`;
- other top-level Shen expressions used for effects.

Definitions are hoisted before initializers, including across all sources in a
single descriptor. Top-level effects do not run while compiling. When the
object is loaded, definitions and selected metadata are installed first, then
effects run in their original relative source order. The same rule applies to
effects inside packages.

Descriptor metadata controls what is restored in the loading Shen image:

- `runtime` records exported arities and installs the needed exported
  top-level bindings for sealed modules and module apps.
- `compiletime` replays declarations, macros, datatypes, and synonyms.
- `source-kl` records KLambda for `ps` and user-definition introspection.

Package external/internal registration is preserved independently of that
list. Direct source compilation uses runtime and compile-time metadata.

Ordinary foreign Scheme expressions can be top-level effects. A foreign escape
that produces a Scheme definition-context form such as `define`, `import`,
`module`, or `library` cannot be used as a native initializer.

## Profiles and WPO

| Profile | Chez settings | Intended use |
| --- | --- | --- |
| `release` | Optimize level 2 | Safe optimized default |
| `debug` | Optimize level 2, debug level 2, inspector and source information | Debugging compiled code |
| `wpo` | Release settings plus WPO sidecar generation | Whole-program app builds |
| `unsafe` | Optimize level 3 | Trusted, well-tested code only |

Chez optimize level 3 can omit checks based on compiler assumptions. Incorrect
type or bounds assumptions can crash the process or corrupt memory.

For app builders, either `--wpo` or `--profile wpo` runs Chez whole-program
optimization over the generated application libraries:

```sh
shen-scheme build-module-app modules/my.app.shenmod \
  --module-dir modules -o _build/app.so --wpo
```

The Shen/Scheme runtime remains external and is not folded into the app. WPO on
a direct or standalone module compile produces Chez WPO information, but only
an app builder has the closed generated program needed to produce the final
whole-program object.

## Runtime deployment

A normal installation contains:

```text
bin/shen-scheme
lib/shen-scheme/petite.boot
lib/shen-scheme/scheme.boot
lib/shen-scheme/shen-scheme/runtime.so
```

The boot files are unmodified Chez files. `runtime.so` is one composite Chez
object containing the R6RS library `(shen-scheme runtime)` followed by the
Shen/Scheme launcher program. Chez maps that library name to
`shen-scheme/runtime.so`, which is why the object is nested one level below the
boot files.

At startup the C executable:

1. finds its home at `../lib/shen-scheme`, or uses `SHEN_SCHEME_HOME`;
2. registers `petite.boot` and, in full mode, `scheme.boot`;
3. loads `shen-scheme/runtime.so` and starts the Shen launcher.

### Migrating from `shen.boot`

Older releases generated a custom `shen.boot`. That file and
`SHEN_SCHEME_BOOT` are no longer used. Install or select the complete new home
instead:

```sh
SHEN_SCHEME_HOME=/opt/shen-scheme/lib/shen-scheme shen-scheme --version
```

Keep the stock boots and composite runtime from the same Shen/Scheme build
together. Do not combine runtime artifacts from different releases or Chez
builds.

### Compiler-free Petite deployment

The default `SHEN_SCHEME_RUNTIME=full` registers both boot files and supports
native compilation. `petite` registers only `petite.boot`:

```sh
SHEN_SCHEME_RUNTIME=petite shen-scheme script app.shen
```

Petite can run Shen and load native objects built by a matching full
installation. It cannot run the native compilation commands because Chez's
compiler is absent. A Petite-only deployment may omit `scheme.boot` if every
launch sets `SHEN_SCHEME_RUNTIME=petite`; it still needs the executable,
`petite.boot`, and `shen-scheme/runtime.so` in the layout above.

A typical deployment flow is:

1. build the application object with the full runtime;
2. copy the object alongside an otherwise matching Petite deployment;
3. start with `SHEN_SCHEME_RUNTIME=petite` and load the object.

`make test-external-runtime` exercises both full and Petite operation.

## Shen API

The CLI is backed by the following supported `shen-scheme` functions. `S`,
`F`, `M`, `O`, `Scm`, and `Dir` below are source, descriptor, main-source,
object, generated-Scheme, and directory paths. `P` is a profile symbol and
`Mode` is `compatible` or `sealed`.

### Source files

```shen
(shen-scheme.compile-file S O)
(shen-scheme.compile-file/mode S O Mode)
(shen-scheme.compile-file/profile S O P)
(shen-scheme.compile-file/profile/mode S O P Mode)

(shen-scheme.compile-file/emit S O Scm)
(shen-scheme.compile-file/emit/mode S O Scm Mode)
(shen-scheme.compile-file/emit/profile S O Scm P)
(shen-scheme.compile-file/emit/profile/mode S O Scm P Mode)
```

Each returns `O`.

### Modules and loading

```shen
(shen-scheme.compile-module F O)
(shen-scheme.compile-module/in-dir F O Dir)
(shen-scheme.compile-module/emit F O Scm)
(shen-scheme.compile-module/emit/in-dir F O Scm Dir)
(shen-scheme.load-compiled O)
(shen-scheme.load-module F ModuleDir ObjectDir)
```

The compilation functions and `load-compiled` return `O`. `load-module`
returns the loaded module-name list; dependencies are initialized before
dependants. `ModuleDir` contains declarations and `ObjectDir` contains compiled
module objects.

### Applications

```shen
(shen-scheme.build-app M ModuleSources O)
(shen-scheme.build-app/wpo M ModuleSources O)
(shen-scheme.build-app/profile M ModuleSources O P)
(shen-scheme.build-app/wpo/profile M ModuleSources O P)

(shen-scheme.build-module-app F Dir O)
(shen-scheme.build-module-app/wpo F Dir O)
(shen-scheme.build-module-app/profile F Dir O P)
(shen-scheme.build-module-app/wpo/profile F Dir O P)
```

`ModuleSources` is an ordered list of source paths. An app builder returns:

```shen
[O RemainingLibraries]
```

`RemainingLibraries` is `[]` without WPO. For WPO it is Chez's list of
libraries that remain external to the whole-program object, normally including
`[shen-scheme runtime]`.

Functions ending in `/options`, including `compile-file/options`,
`compile-file/emit/options`, `build-app/emit/options`, and
`build-module-app/emit/options`, expose the compiler's internal option vector
and scratch-directory controls. They are advanced, lower-level interfaces;
prefer the mode/profile facade above in application code.

## Embedding as R6RS libraries

The source generator can produce a public `(shen)` R6RS library instead of the
launcher program. From a Shen/Scheme checkout:

```sh
mkdir -p _build/embed/shen-scheme
./_build/bin/shen-scheme eval \
  -e '(load "scripts/build.shen")' \
  -e '(set *runtime-library-file* "_build/embed/shen-scheme/runtime.ss")' \
  -e '(build library "_build/embed/shen.ss")'
```

This creates two sources:

- `_build/embed/shen-scheme/runtime.ss`, the private
  `(shen-scheme runtime)` library;
- `_build/embed/shen.ss`, the public `(shen)` library.

Compile the runtime first, with `_build/embed` as both the source and object
library root, then compile `shen.ss`. In Chez this is equivalent to:

```scheme
(parameterize ([library-directories
                (cons '("_build/embed" . "_build/embed")
                      (library-directories))])
  (compile-file "_build/embed/shen-scheme/runtime.ss"
                "_build/embed/shen-scheme/runtime.so")
  (compile-file "_build/embed/shen.ss"
                "_build/embed/shen.so"))
```

An embedding program imports `(shen)`, calls `initialize-shen` once, and then
uses its exported `kl:` bindings. Hosts that use native compilation must also
register the `get_shen_scheme_home_path` callback used to locate runtime
artifacts. The optional CFFI conversion helpers additionally require
`scm_make_utf8_string` and `scm_make_bytevector`. The supplied `shen-scheme`
launcher registers all three.

## Benchmarks

The port and realistic suites compare five execution modes:

```text
dynamic compatible sealed app app-wpo
```

Run every mode:

```sh
make bench-port
make bench-realistic
```

Pass runner arguments through the corresponding make variable:

```sh
make bench-port PORT_BENCH_ARGS='sealed app-wpo --offset 2 --samples 5'
make bench-realistic REALISTIC_BENCH_ARGS='compatible sealed --samples 3'
```

Recognized arguments are:

- any subset of the five mode names; all modes run if none is given;
- `--samples N` for repeated samples;
- `--offset N` to reduce each benchmark's base-10 run exponent by `N`;
- `--quick` (or `quick`) for the suite's smoke-test offset.

`make bench-port-smoke` and `make bench-realistic-smoke` select quick mode.
After comment lines beginning with `#`, results are pipe-delimited:

```text
mode|sample|tag|description|runs_power|seconds|result
```

Compilation chatter is suppressed so the data rows can be consumed directly.
The primitive workloads are vendored unchanged from `shen-sources`; see their
[provenance and cross-port notes](https://github.com/tizoc/shen-scheme/blob/master/benchmarks/port/README.md).

## Limitations and artifact compatibility

- A dependency macro must be self-contained or use helpers already present in
  the compiler. Ordinary helpers defined only in dependency source are not
  installed during dependency analysis.
- Macro transformer names share the live compiler namespace and must not
  collide with existing bindings.
- Unusual compile-time rewrites that depend on another module's `declare`
  forms must be arranged by the caller.
- Foreign Scheme definition-context forms cannot be native initializers.
- A raw-source app does not replay compile-time forms when loaded.

Native outputs are build artifacts. Rebuild them when source files, profile,
Shen/Scheme, Chez, operating system, or architecture changes. They are not a
portable module format and are expected to be loaded only by the matching
runtime that produced them.
