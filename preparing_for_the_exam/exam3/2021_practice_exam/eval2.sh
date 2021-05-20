#!/bin/bash


(
  echo "#lang r5rs"
  echo ""
  cat task2_correct.ss
  echo ""
  cat eval2_public_test.ss
) > tmp.ss

echo "Public test results"
echo $(racket tmp.ss)


