# Shen/Scheme

[![Shen Version](https://img.shields.io/badge/shen-41.2-blue.svg)](https://github.com/Shen-Language)
[![build](https://github.com/tizoc/shen-scheme/workflows/build/badge.svg)](https://github.com/tizoc/shen-scheme/actions?query=workflow%3Abuild)

* [Shen](https://shen-language.github.io/)
* [chez-scheme](https://cisco.github.io/ChezScheme)
* [shen-scheme](https://github.com/tizoc/shen-scheme)

Shen/Scheme is a port of the Shen language that runs on Scheme. The currently
supported backend is [Chez Scheme](https://cisco.github.io/ChezScheme).

Shen is a portable functional programming language by
[Mark Tarver](http://marktarver.com) that offers:

- pattern matching,
- lambda calculus consistency,
- macros,
- optional lazy evaluation,
- static type checking,
- an integrated fully functional Prolog,
- and an inbuilt compiler-compiler.

Older Shen/Scheme releases supported additional Scheme implementations. Since
version 0.16, the maintained backend is Chez Scheme.

* [chibi-scheme](http://synthcode.com/wiki/chibi-scheme)
* [gauche](http://practical-scheme.net/gauche/)

Binaries
--------

Starting with version 0.18, binaries are provided for Windows, Linux, and macOS.
See [releases](https://github.com/tizoc/shen-scheme/releases).

Nix users can use the community-maintained package at
https://github.com/hakujin/nix-shen-scheme. With Nix installed, you can drop
into a working Shen/Scheme via:

```sh
nix run github:hakujin/nix-shen-scheme/master#shen-scheme
```

Building
--------

### Building from the source distribution

> **IMPORTANT:** Download the release asset named `shen-scheme-<version>-src.tar.gz` from
> the [Releases](https://github.com/tizoc/shen-scheme/releases) page. Do **not** use the
> GitHub-generated "Source code" tarball/zip — it is missing the pre-generated
> `.scm` files and `shen-scheme-runtime.ss`.

Running `make` should do the job. It downloads and compiles Chez under the
`_build` directory, then builds the `shen-scheme` binary and a Shen/Scheme
runtime object. Shen/Scheme starts from the stock Chez boot files and loads its
runtime separately; it does not generate a custom boot file.

```sh
make prefix=/opt/shen-scheme # optional prefix, defaults to /usr/local
```

The Makefile verifies downloaded Chez, Shen kernel, and bootstrap archives
against pinned SHA-256 checksums. When selecting a different `csversion`,
`shenversion`, or `prebuilt_version`, also provide its trusted checksum as
`chez_sha256`, `kernel_sha256`, or `prebuilt_sha256`; the build will not extract
an unverified archive.

Runtime generation can be configured with
`SHEN_SCHEME_OPTIMIZE_LEVEL`, `SHEN_SCHEME_DEBUG_LEVEL`,
`SHEN_SCHEME_INSPECTOR`, and `SHEN_SCHEME_SOURCE_INFO`.
`make` treats these values as build inputs and rebuilds the runtime when
they change.

Then install with:

```sh
make install
```

This installs the binary to `$(prefix)/bin/shen-scheme` and this runtime under
`$(prefix)/lib/shen-scheme`:

- `petite.boot` and `scheme.boot`: the stock Chez boot files.
- `shen-scheme/runtime.so`: one Chez object containing both the Shen/Scheme
  runtime library and launcher.

The nested object path is intentional: it is the path Chez derives for the
R6RS library `(shen-scheme runtime)`. The build compiles the library and
launcher as separate units, then combines them for installation.

To build on Windows, an environment with curl, 7zip, make, and Visual Studio
2017 is needed. These can be installed with [Chocolatey](https://chocolatey.org/).

### Building from scratch

This step is only necessary if cloning from this repository. The release tarballs
include pre-generated `.scm` files.

To build from source, obtain a
[copy of the Shen kernel distribution](https://github.com/Shen-Language/shen-sources/releases)
and copy the `.kl` files to the `kl/` directory of Shen/Scheme. Then with a
working Shen implementation do:

```shen
(load "scripts/build.shen")
(build program "shen-scheme.scm")
```

This produces `.scm` files in the `compiled/` directory plus
`shen-scheme.scm` and `shen-scheme-runtime.ss` in the current directory.

After doing this the procedure is the same as building from the source distribution.

Running
-------

`shen-scheme` starts the Shen REPL. The explicit `repl` command does the same
thing.

```sh
shen-scheme
shen-scheme repl
```

Run a script with:

```sh
shen-scheme script path/to/script.shen
```

Evaluate expressions and files with:

```sh
shen-scheme eval -e "(+ 1 2)"
shen-scheme eval -l path/to/file.shen -e "(main)"
```

Use `--help` for the full launcher and native compilation command list:

```sh
shen-scheme --help
```

Native Compilation
------------------

Native compilation is explicit and does not change normal REPL, `script`,
`eval`, or Shen `load` behavior. Its `.so` outputs are Chez object files, not
operating-system shared libraries or standalone executables.

The commands below assume a source checkout. In a binary release, skip `make`
and use `./bin/shen-scheme` in place of `./_build/bin/shen-scheme`.

| Goal | Use |
| --- | --- |
| Compile one source file and preserve normal redefinition behavior | `compile` (the default `compatible` mode) |
| Bind calls within one source unit for speed | `compile --mode sealed` |
| Describe exports, metadata, and dependencies | `compile-module` and `load-module` with `.shenmod` files |
| Bundle an ordered set of source files | `build-app` |
| Bundle a closed `.shenmod` dependency graph | `build-module-app` |

A runnable first example from the repository root is:

```sh
mkdir -p _build/native-examples
./_build/bin/shen-scheme compile examples/native/single-file.shen \
  -o _build/native-examples/single-file.so
./_build/bin/shen-scheme eval \
  -e '(shen-scheme.load-compiled "_build/native-examples/single-file.so")' \
  -e '(answer 5)'
```

The final expression returns `26`.

See the [native examples](examples/native/README.md) for compatible versus
sealed binding, packages and effects, module dependencies, bundled apps, WPO,
and full-to-Petite deployment. The
[native compilation guide](docs/native-compilation.md) documents the complete
CLI, `.shenmod` format, Shen API, metadata behavior, limitations, embedding,
deployment, and benchmarks.

Home and runtime selection
--------------------------

Shen/Scheme will use as its *home directory* a path relative to the executable:
`../lib/shen-scheme`.
For example, if the executable is `/usr/local/bin/shen-bin` then the
*home directory* will be `/usr/local/lib/shen-scheme`.
This can be overridden by the `SHEN_SCHEME_HOME` environment variable.

By default, the launcher registers `<shen-scheme-home>/petite.boot` and
`<shen-scheme-home>/scheme.boot`, then loads the composite
`<shen-scheme-home>/shen-scheme/runtime.so`. This full runtime supports the
native compilation commands.

`SHEN_SCHEME_RUNTIME` accepts `full` (the default) or `petite`. Petite mode
registers only `petite.boot`; it still loads the Shen/Scheme runtime and
can run Shen and load precompiled native artifacts, but native compilation
commands are unavailable.

`SHEN_SCHEME_BOOT` is no longer used. Set `SHEN_SCHEME_HOME` when selecting a
different, internally consistent set of boot and runtime artifacts. See
[Runtime deployment](docs/native-compilation.md#runtime-deployment) for the
layout, migration notes, and a compiler-free Petite deployment.

Calling Scheme from Shen
------------------------

Scheme functions live under the `scm` namespace (`scm.` prefix), and names need
to be wrapped with the `foreign` form in calls. For example,
`((foreign scm.write) [1 2 3 4])` invokes Scheme's `write` function with a list
as an argument.

Because Scheme functions can have variable numbers of arguments and the code
passed to `scm.` is not preprocessed, any imported function that is intended to
support partial application has to be wrapped with a `defun`:

```shen
(0-) (defun my-for-each (F L) ((foreign scm.for-each) F L))
my-for-each

(1-) (my-for-each (/. X (do (print (+ X X)) (nl))) [1 2 3 4 5])
2
4
6
8
10
0

(2-) (my-for-each (function print))
#<procedure>
```

Literal Scheme Code
-------------------

Scheme code can be compiled as-is with the `scm.` special form that takes a
string with Scheme code as an argument.

Example:

```shen
(0-) ((foreign scm.) "(+ 1 2)")
3

(1-) ((foreign scm.) "(begin (display c#34;testc#34;) (newline))")
test
#<void>

(2-) ((foreign scm.) "(list #t #f (quote symbol) 'symbol)")
[true false symbol symbol]
```

Importing Bindings from Scheme Modules
--------------------------------------

[Import expressions](https://cisco.github.io/ChezScheme/csug9.5/libraries.html#./libraries:h4)
are supported through the `scm.` prefix. Names will be imported under the
`scm.` namespace.

Example:

```shen
(1-) ((foreign scm.import) (rename (rnrs) (+ add-numbers)))
#<void>

(2-) ((foreign scm.add-numbers) 1 2 3 4)
10
```

License
-------

- Shen, Copyright © 2010-2022 Mark Tarver - [License](http://www.shenlanguage.org/license.pdf).
- shen-scheme, Copyright © 2012-2023 Bruno Deferrari under [BSD 3-Clause License](http://opensource.org/licenses/BSD-3-Clause).
