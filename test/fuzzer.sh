TOTALRUNS=10000

echo "Compiling fuzzer"
dmd fuzzer.d -I../src/ ../src/std/d/*.d ../src/std/*.d || exit 1
echo "Done"
echo "Compiling parser"
dmd -O -inline -g tester.d ../src/std/*.d ../src/std/d/*.d  -I../src/ || exit 1
echo "Done"
printf "           "
for i in $(seq $TOTALRUNS); do
	printf "\b\b\b\b\b\b\b\b\b\b\b"
	printf "%5d/%5d" $i $TOTALRUNS
	./fuzzer > tokens.txt
	./tester tokens.txt > output.txt 2>&1
	if [ $? -eq 139 ]; then echo "Segfaulted..."; sed -e "s/ /\\n/g" tokens.txt > tokens-newlines.txt; exit 1; fi
	grep ception output.txt && sed -e "s/ /\\n/g" tokens.txt > tokens-newlines.txt && exit 1;
done
printf "\n"
