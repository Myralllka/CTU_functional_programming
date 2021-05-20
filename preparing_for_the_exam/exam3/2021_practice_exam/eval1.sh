#!/bin/bash

PUB1="(display (equal? (create_cholesky '((4 12 -16) (12 37 -43) (-16 -43 98))) '((2 0 0) (6 1 0) (-8 5 3))))"

rm tmp.ss
(
  #echo "#lang r5rs" # will be "racket" or "scheme" in 2021
  cat task1_correct.ss
  echo
  echo -e "$PUB1"
) > tmp.ss

echo "Result for public instance:" 
echo "$PUB1"
echo $(racket tmp.ss)

