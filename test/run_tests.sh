#!/bin/bash

PASS_FILES=$(find pass_files -name "*.d")
FAIL_FILES=$(find fail_files -name "*.d")
PASS_COUNT=0
FAIL_COUNT=0
NORMAL="\033[01;0m"
GREEN="\033[32m"
RED="\033[31m"
CYAN="\033[36m"
DMD=${DMD:=dmd}
ALLOCATOR_SOURCE=$(find ../experimental_allocator/src/std/experimental -name "*.d")

echo -en "Compiling tester... "
${DMD} tester.d ../src/std/experimental/*.d ../src/dparse/*.d ${ALLOCATOR_SOURCE} -g -I../src/ || exit 1
echo -e "${GREEN}DONE${NORMAL}"

for i in $PASS_FILES; do
	echo -en "Parsing" $i
	echo -en "... "
	./tester $i 2>/dev/null 1>/dev/null;
	if [ $? -eq 0 ]; then
		echo -e "${GREEN}PASS${NORMAL}"
		let PASS_COUNT=PASS_COUNT+1
	else
		echo -e "${RED}FAIL${NORMAL}"
		let FAIL_COUNT=FAIL_COUNT+1
	fi
done

for i in $FAIL_FILES; do
	echo -en "Parsing" $i
	echo -en "... "
	./tester $i 2>/dev/null 1>/dev/null;
	if [ $? -eq 0 ]; then
		echo -e "${RED}FAIL${NORMAL}"
		let FAIL_COUNT=FAIL_COUNT+1
	else
		echo -e "${GREEN}PASS${NORMAL}"
		let PASS_COUNT=PASS_COUNT+1
	fi
done

if [ $FAIL_COUNT -eq 0 ]; then
	echo -e "${GREEN}${PASS_COUNT} tests passed and ${FAIL_COUNT} failed.${NORMAL}"
else
	echo -e "${RED}${PASS_COUNT} tests passed and ${FAIL_COUNT} failed.${NORMAL}"
	exit 1
fi

find . -name "*.lst" | xargs rm -f
echo -en "Generating coverage reports... "
${DMD} tester.d -cov ../src/std/experimental/*.d ../src/dparse/*.d  ${ALLOCATOR_SOURCE} -I../src/ || exit 1
./tester $PASS_FILES $FAIL_FILES 2>/dev/null 1>/dev/null
rm -rf coverage/
mkdir coverage/
for i in $(find . -name "*.lst"); do
	mv $i coverage/$(echo $i | sed -e "s/\\.\\.\\-//");
done
echo -e "${GREEN}DONE${NORMAL}"
for i in coverage/*.lst; do
	echo $(tail $i -n1)
done

rm -f tester tester.o
