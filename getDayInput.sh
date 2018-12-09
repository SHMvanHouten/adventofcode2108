#!/bin/bash

if [[ -z "$1" ]]
then
echo "please enter a the number of the day you want to pull"
exit 0
fi

mkdir src/AOC/Day$1
mkdir test/AOC/Day$1

curl --cookie "_ga=GA1.2.743125386.1514929847; session=53616c7465645f5fa0a331122b94b4753c18f51d05eea9a3e275a4230cea277c34f64c70f1af9ad5b22457c8eb3316f9; _gid=GA1.2.1419791182.1543748024" https://adventofcode.com/2018/day/$1/input -o resources/input-day$1.txt -s

curl https://adventofcode.com/2018/day/$1 -o src/AOC/Day$1/challenge.html -s
