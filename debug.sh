#!/bin/bash

rm debug/*.ml
sed -e 's/\(^open .*\)/(* \1 *)/' eval.ml > debug/eval.ml
sed -e 's/\(^open .*\)/(* \1 *)/' extendn.ml > debug/extendn.ml
sed -e 's/\(^open .*\)/(* \1 *)/' types.ml > debug/types.ml
sed -e 's/\(^open .*\)/(* \1 *)/' p4.ml > debug/p4.ml
cd debug/
ocaml -init ../.ocamlinit

