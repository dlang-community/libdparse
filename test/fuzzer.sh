#!/bin/bash

echo "Compiling fuzzer"
dmd fuzzer.d -I../src/ ../src/std/d/*.d ../src/std/*.d || exit 1
echo "Done"
echo "Compiling parser"
dmd -O -inline -g tester.d ../src/std/*.d ../src/std/d/*.d  -I../src/ || exit 1
echo "Done"
for i in $(seq 0 100000); do
	./fuzzer > tokens.txt
	./tester tokens.txt > output.txt 2>&1
	if [ $? -eq 139 ]; then echo "Segfaulted..."; sed -e "s/ /\\n/g" tokens.txt > tokens-newlines.txt; exit 1; fi
	grep ception output.txt && sed -e "s/ /\\n/g" tokens.txt > tokens-newlines.txt && exit 1;
	echo "$i"
done
