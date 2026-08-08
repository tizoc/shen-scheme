# Port benchmark sources

The five `.shen` workload files in this directory are copied byte-for-byte from
the [`Shen-Language/shen-sources`](https://github.com/Shen-Language/shen-sources)
benchmark suite at commit `93ed67eb2ba12558a165147142fb356483578cce`.

Keep these files in sync with the upstream sources rather than editing them
locally. Shen/Scheme-specific compilation, registration, and reporting belong
in the runner, harness, and module descriptors outside the copied files.

The copies intentionally retain upstream behavior. In particular, `vector-read`
and `vector-write` recurse through the abstract-vector helpers after their first
iteration, and the input labelled `shen.pvar? (true)` is not a tagged Prolog
variable in Shen/Scheme. Changes to those benchmarks should be made upstream
first and then synced here.
