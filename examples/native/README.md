# Native compilation examples

Run these examples from the repository root after building Shen/Scheme:

```sh
make
mkdir -p _build/native-examples
```

In a binary release, skip `make` and use `./bin/shen-scheme` in place of
`./_build/bin/shen-scheme`.

The `.so` files below are Chez compiled objects, not operating-system shared
libraries.

## Compile one source file

Compile a source file and load the result in the same process that calls it:

```sh
./_build/bin/shen-scheme compile examples/native/single-file.shen \
  -o _build/native-examples/single-file.so
./_build/bin/shen-scheme eval \
  -e '(shen-scheme.load-compiled "_build/native-examples/single-file.so")' \
  -e '(answer 5)'
```

The last expression evaluates to `26`. `load-compiled` as a launcher command
loads an object and exits; use `eval` when later expressions need its functions.

## Compatible and sealed calls

Compatible mode is the default. A compiled caller observes a later top-level
redefinition of its helper:

```sh
./_build/bin/shen-scheme compile examples/native/binding.shen \
  -o _build/native-examples/compatible.so
./_build/bin/shen-scheme eval \
  -e '(shen-scheme.load-compiled "_build/native-examples/compatible.so")' \
  -e '(load "examples/native/binding-update.shen")' \
  -e '(call-helper 1)'
```

This evaluates to `101`. Compile the same source in sealed mode and the caller
keeps its statically bound helper, so the final expression evaluates to `2`:

```sh
./_build/bin/shen-scheme compile examples/native/binding.shen --mode sealed \
  -o _build/native-examples/sealed.so
./_build/bin/shen-scheme eval \
  -e '(shen-scheme.load-compiled "_build/native-examples/sealed.so")' \
  -e '(load "examples/native/binding-update.shen")' \
  -e '(call-helper 1)'
```

## Packages and top-level effects

Native compilation accepts normal `package` forms and delays top-level effects
until the object is loaded. Definitions are installed first, so an initializer
can call `record` before its textual definition:

```sh
./_build/bin/shen-scheme compile examples/native/package-effects.shen \
  -o _build/native-examples/package-effects.so
./_build/bin/shen-scheme eval \
  -e '(shen-scheme.load-compiled "_build/native-examples/package-effects.so")' \
  -e '(effect-events)'
```

The two string values print as `[inside-before-definition after-definition]`,
preserving the source order of the effects.

## Compile and load a module graph

A Shen package controls source-level qualification. A `.shenmod` declaration
instead describes native compilation, dependencies, exports, and metadata. The
example modules use both mechanisms.

`load-module` resolves declarations and compiled objects by module name from
separate roots. Relative sources are resolved from each declaration's
directory, so declarations stay beside their sources while generated objects
go under `_build`:

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

The last two expressions evaluate to `42` and `[42]`. The core module is loaded
before the app module, and only its declared export is available for native
cross-module calls.

## Build one application object

`build-module-app` bundles the closed descriptor graph into one object:

```sh
./_build/bin/shen-scheme build-module-app \
  examples/native/modules/native-example.app.shenmod \
  --module-dir examples/native/modules \
  -o _build/native-examples/app.so
./_build/bin/shen-scheme eval \
  -e '(shen-scheme.load-compiled "_build/native-examples/app.so")' \
  -e '(run-example 32)'
```

Add `--wpo` to the build command to run Chez whole-program optimization over
the application graph.

## Load a full-runtime artifact with Petite

Compile with the default full runtime, then select the compiler-free runtime
when loading the object:

```sh
./_build/bin/shen-scheme compile examples/native/single-file.shen \
  -o _build/native-examples/full-to-petite.so
SHEN_SCHEME_RUNTIME=petite ./_build/bin/shen-scheme eval \
  -e '(shen-scheme.load-compiled "_build/native-examples/full-to-petite.so")' \
  -e '(answer 5)'
```

`make test-native-examples` validates the examples through the module-app build.
`make test-external-runtime` verifies full-runtime compilation, Petite loading,
and the expected absence of the compiler in Petite mode.
