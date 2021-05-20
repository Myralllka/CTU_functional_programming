#!/bin/bash



(
cat task3_correct.hs
echo ""
cat eval3_public_test.hs
) > tmp.hs

echo "Public test results:"
ghc tmp.hs
echo $(./tmp)
