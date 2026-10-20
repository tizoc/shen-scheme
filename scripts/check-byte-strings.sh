#!/bin/sh
# Strings are byte strings (src/chez-prelude.scm): what a program prints must
# be exactly the bytes of its string literals, as the kernel's pr writes them.
set -u
# An absolute launcher, so checks can run inside the temporary directory.
exe=$(cd "$(dirname "$1")" && pwd)/$(basename "$1")
status=0
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

check() {
  label="$1"; expected="$2"; actual="$3"
  if cmp -s "$expected" "$actual"; then
    echo "[OK]    $label"
  else
    echo "[FAIL]  $label"; od -An -tx1 "$actual"; status=1
  fi
}

# eval and load also print the value of what they ran; compare the bytes pr
# wrote, which come first.
prefix() {
  head -c "$(wc -c < "$tmp/expected")" "$1" > "$1.prefix"
  echo "$1.prefix"
}

printf 'λ雪 café' > "$tmp/expected"

printf '(pr "λ雪 café" (stoutput))\n' > "$tmp/pr.shen"
"$exe" script "$tmp/pr.shen" > "$tmp/script.out"
check "script: pr writes a literal's bytes to stdout" "$tmp/expected" "$tmp/script.out"

"$exe" eval -e '(pr "λ雪 café" (stoutput))' > "$tmp/eval.out"
check "eval -e: an argument's bytes reach stdout unchanged" "$tmp/expected" "$(prefix "$tmp/eval.out")"

printf '(pr (n->string 233) (stoutput))\n' > "$tmp/byte.shen"
"$exe" script "$tmp/byte.shen" > "$tmp/byte.out"
printf '\351' > "$tmp/byte.expected"
check "a character below 256 is one byte" "$tmp/byte.expected" "$tmp/byte.out"

printf '(pr "λ雪 café" (stoutput))\n' > "$tmp/naïve λ.shen"
"$exe" script "$tmp/naïve λ.shen" > "$tmp/named.out"
check "a script whose path is not ASCII loads" "$tmp/expected" "$tmp/named.out"

# Paths written inside Shen source are relative: on Windows the shell's
# /tmp/... form is translated for arguments, never inside a file.
printf '(load "naïve λ.shen")\n' > "$tmp/load.shen"
(cd "$tmp" && "$exe" script load.shen) > "$tmp/load.out"
check "load of a non-ASCII path named by a literal" "$tmp/expected" "$(prefix "$tmp/load.out")"

printf '(write-to-file "naïve λ.txt" "x")\n' > "$tmp/name.shen"
(cd "$tmp" && "$exe" script name.shen) > /dev/null
if [ -f "$tmp/naïve λ.txt" ]; then
  echo "[OK]    a file named by a literal gets that UTF-8 name on disk"
else
  echo "[FAIL]  a file named by a literal gets that UTF-8 name on disk"; ls "$tmp"; status=1
fi

"$exe" script "$tmp/missing-λ.shen" > /dev/null 2> "$tmp/err.out"
if grep -q 'missing-λ.shen' "$tmp/err.out"; then
  echo "[OK]    an error naming a non-ASCII path prints it as UTF-8"
else
  echo "[FAIL]  an error naming a non-ASCII path prints it as UTF-8"; od -c "$tmp/err.out" | head; status=1
fi

exit $status
