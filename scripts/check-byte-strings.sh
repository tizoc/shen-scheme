#!/bin/sh
set -eu

exe=$(cd "$(dirname "$1")" && pwd)/$(basename "$1")
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

check() {
  if ! cmp -s "$tmp/expected" "$tmp/actual"; then
    echo "[FAIL]  $1"
    od -An -tx1 "$tmp/actual"
    exit 1
  fi
  echo "[OK]    $1"
}

printf 'λ雪 café' > "$tmp/expected"
mkdir "$tmp/λ"
printf '(pr "λ雪 café" (stoutput))\n' > "$tmp/λ/naïve.shen"
"$exe" script "$tmp/λ/naïve.shen" > "$tmp/actual"
check "script paths and source literals preserve UTF-8"
(cd "$tmp/λ" && "$exe" script naïve.shen) > "$tmp/actual"
check "relative paths work from a Unicode current directory"

printf '(write-to-file "naïve λ.txt" "λ雪 café")\n' > "$tmp/λ/write.shen"
(cd "$tmp/λ" && "$exe" script write.shen) > /dev/null
cp "$tmp/λ/naïve λ.txt" "$tmp/actual"
check "filenames in source literals get their UTF-8 names on disk"

printf '(define byte-cli-answer -> "λ雪 café")\n' > "$tmp/λ/naïve-value.shen"
printf '(set *hush* true)\n(load "naïve-value.shen")\n(set *hush* false)\n(pr (byte-cli-answer) (stoutput))\n' > "$tmp/λ/load.shen"
(cd "$tmp/λ" && "$exe" script load.shen) > "$tmp/actual"
check "load accepts a filename written as a source literal"

printf '(pr (hd (tl (value *argv*))) (stoutput))\n' > "$tmp/args.shen"
"$exe" script "$tmp/args.shen" 'λ雪 café' > "$tmp/actual"
check "script arguments enter Shen as bytes"

printf '(pr "λ雪 café" (sterror))\n' > "$tmp/stderr.shen"
"$exe" script "$tmp/stderr.shen" 2> "$tmp/actual"
check "stderr receives the source literal's UTF-8 bytes"

printf 'λ雪 caféλ雪 café\n' > "$tmp/expected"
"$exe" eval -e '(pr "λ雪 café" (stoutput))' > "$tmp/actual"
check "eval -e uses the same string representation as a source file"

printf 'éλéλ\n' > "$tmp/expected"
"$exe" eval -e '(trap-error (simple-error "éλ") (/. E (pr (error-to-string E) (stoutput))))' > "$tmp/actual"
check "caught Shen errors preserve UTF-8 message bytes"

if "$exe" script "$tmp/missing-éλ.shen" > /dev/null 2> "$tmp/actual"; then
  echo "[FAIL]  a missing script must fail"
  exit 1
fi
grep -Fq 'missing-éλ.shen' "$tmp/actual"
echo "[OK]    uncaught filesystem errors display Unicode filenames"

"$exe" eval -e '(trap-error (open "missing-éλ.shen" in) (/. E (pr (error-to-string E) (stoutput))))' > "$tmp/actual"
grep -Fq 'missing-éλ.shen' "$tmp/actual"
echo "[OK]    caught filesystem errors preserve UTF-8 filenames"

if "$exe" eval -e '(simple-error "éλ")' > /dev/null 2> "$tmp/actual"; then
  echo "[FAIL]  an uncaught Shen error must fail"
  exit 1
fi
grep -Fq 'éλ' "$tmp/actual"
echo "[OK]    uncaught Shen errors display Unicode messages"
