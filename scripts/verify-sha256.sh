#!/bin/sh

set -eu

expected=${1-}
file=${2-}

if [ -z "$file" ]; then
  echo "usage: verify-sha256.sh EXPECTED FILE" >&2
  exit 2
fi

if [ -z "$expected" ]; then
  echo "error: no SHA-256 checksum is configured for $file" >&2
  exit 2
fi

case "$expected" in
  *[!0123456789abcdefABCDEF]*)
    echo "error: invalid SHA-256 checksum configured for $file" >&2
    exit 2
    ;;
esac

if [ "${#expected}" -ne 64 ]; then
  echo "error: invalid SHA-256 checksum configured for $file" >&2
  exit 2
fi

if [ ! -f "$file" ]; then
  echo "error: cannot verify missing file $file" >&2
  exit 2
fi

if command -v sha256sum >/dev/null 2>&1; then
  actual=$(sha256sum "$file")
  actual=${actual%% *}
elif command -v shasum >/dev/null 2>&1; then
  actual=$(shasum -a 256 "$file")
  actual=${actual%% *}
elif command -v certutil >/dev/null 2>&1; then
  actual=$(certutil -hashfile "$file" SHA256 | sed -n '2{s/[[:space:]]//g;p;}')
else
  echo "error: SHA-256 verification requires sha256sum, shasum, or certutil" >&2
  exit 2
fi

expected=$(printf '%s' "$expected" | tr '[:upper:]' '[:lower:]')
actual=$(printf '%s' "$actual" | tr '[:upper:]' '[:lower:]')

if [ "$actual" != "$expected" ]; then
  echo "error: SHA-256 checksum mismatch for $file" >&2
  echo "expected: $expected" >&2
  echo "actual:   $actual" >&2
  exit 1
fi

echo "$file: SHA-256 OK"
