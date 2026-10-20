\* Copyright (c) 2012-2026 Bruno Deferrari.  All rights reserved.    *\
\* BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

\* Strings are byte strings: the kernel's reader builds them from the bytes of
   a file and its pr writes them back byte by byte (src/chez-prelude.scm). Run
   from the repository root; scripts/check-byte-strings.sh checks the bytes
   that reach stdout. *\

(define assert-bytes-equal
  Name Expected Expected -> (output "[OK]    ~A~%" Name)
  Name Expected Actual -> (error "~A: expected ~R, got ~R~%" Name Expected Actual))

(define bytes-of
  "" -> []
  S -> [(string->n (pos S 0)) | (bytes-of (tlstr S))])

(assert-bytes-equal
  "a UTF-8 literal is read as its bytes"
  [206 187 233 155 170 32 99 97 102 195 169]
  (bytes-of "λ雪 café"))

(assert-bytes-equal
  "n->string of a byte rebuilds the literal"
  true
  (= "λ" (cn (n->string 206) (n->string 187))))

(assert-bytes-equal
  "write-to-file writes the bytes, not their UTF-8 encoding"
  [206 187 233 155 170 32 99 97 102 195 169 32 233]
  (let File "_build/byte-strings-write.txt"
       Written (write-to-file File (cn "λ雪 café " (n->string 233)))
       Bytes (read-file-as-bytelist File)
       Deleted (delete-file File)
    Bytes))

(assert-bytes-equal
  "read-file-as-string returns the byte string the reader would build"
  true
  (let File "_build/byte-strings-read.txt"
       Written (write-to-file File "λ雪 café")
       String (read-file-as-string File)
       Deleted (delete-file File)
    (= String "λ雪 café")))

(assert-bytes-equal
  "write-byte and read-byte round-trip every byte"
  [0 127 128 233 255]
  (let File "_build/byte-strings-bytes.bin"
       Out (open File out)
       W (map (/. B (write-byte B Out)) [0 127 128 233 255])
       C (close Out)
       In (open File in)
       Read [(read-byte In) (read-byte In) (read-byte In) (read-byte In) (read-byte In)]
       C2 (close In)
       Deleted (delete-file File)
    Read))

(assert-bytes-equal
  "a file named with a UTF-8 literal can be written and read back"
  [110 97 195 175 118 101]
  (let File "_build/byte-strings-naïve λ.txt"
       Written (write-to-file File "naïve")
       Back (read-file-as-bytelist File)
       Deleted (delete-file File)
    Back))

(assert-bytes-equal
  "a character at or above 256 is written as its UTF-8 encoding"
  [206 187]
  (let File "_build/byte-strings-wide.txt"
       Written (write-to-file File (n->string 955))
       Bytes (read-file-as-bytelist File)
       Deleted (delete-file File)
    Bytes))
