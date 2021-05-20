#!/bin/bash


PUB1="2\n(2,2)\n(3,1)\n1"

cat task4_correct.hs > tmp.hs

echo "Result for public instance" 
echo -e "$PUB1" | runghc tmp.hs 
