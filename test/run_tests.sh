#!/bin/bash

set -euo pipefail

PASS_FILES=$(find pass_files -name "*.d")
FAIL_FILES=$(find fail_files -name "*.d")
PASS_COUNT=0
FAIL_COUNT=0
NORMAL="\033[01;0m"
GREEN="\033[32m"
RED="\033[31m"
YELLOW="\033[33m"
DMD=${DMD:=dmd}
SOURCE_FILES="../src/std/experimental/*.d ../src/dparse/*.d "
STDX_ALLOC_FILES=$(find ../stdx-allocator/source -name "*.d" )
IMPORT_PATHS="-I../src/ -I../stdx-allocator/source"

${DMD} $STDX_ALLOC_FILES $IMPORT_PATHS -of"stdxalloc.a" -lib

echo -en "Compiling parse tester... "
${DMD} tester.d $SOURCE_FILES -g "stdxalloc.a" $IMPORT_PATHS
echo -e "${GREEN}DONE${NORMAL}"

for i in $PASS_FILES; do
	echo -en "Parsing $i..."
	if ./tester "$i" 2>/dev/null 1>/dev/null; then
		echo -e "${GREEN}PASS${NORMAL}"
		((PASS_COUNT=PASS_COUNT+1))
	else
		echo -e "${RED}FAIL${NORMAL}"
		((FAIL_COUNT=FAIL_COUNT+1))
	fi
done

for i in $FAIL_FILES; do
	echo -en "Parsing $i..."
	if ./tester "$i" 2>/dev/null 1>/dev/null; then
		echo -e "${RED}FAIL${NORMAL}"
		((FAIL_COUNT=FAIL_COUNT+1))
	else
		echo -e "${GREEN}PASS${NORMAL}"
		((PASS_COUNT=PASS_COUNT+1))
	fi
done

echo
if [ "$FAIL_COUNT" -eq 0 ]; then
	echo -e "${GREEN}${PASS_COUNT} parse test(s) passed and ${FAIL_COUNT} failed.${NORMAL}"
else
	echo -e "${RED}${PASS_COUNT} parse test(s) passed and ${FAIL_COUNT} failed.${NORMAL}"
	exit 1
fi

if [[ ${BUILDKITE:-} != "true" ]]; then
	PASS_COUNT=0
	FAIL_COUNT=0
	echo
	for file in ast_checks/*.d; do
		echo -en "Running AST match tests on ${file}..."
		# The query file has the same base name as its corresponding D file, but
		# with a txt extension. It contains XPath expressions, one per line, that
		# must match nodes in the generated AST.
		queryFile=ast_checks/$(basename "$file" .d).txt
		checkCount=1
		currentPasses=0
		currentFailures=0
		while read -r line; do
			if ./tester --ast "$file" | xmllint --xpath "${line}" - 2>/dev/null > /dev/null; then
				((currentPasses=currentPasses+1))
			else
				echo
				echo -e "    ${RED}Check on line $checkCount of $queryFile failed.${NORMAL}"
				((currentFailures=currentFailures+1))
			fi
			((checkCount=checkCount+1))
		done < "$queryFile"
		if [[ $currentFailures -gt 0 ]]; then
			echo -e "    ${RED}${currentPasses} check(s) passed and ${currentFailures} check(s) failed${NORMAL}"
			((FAIL_COUNT=FAIL_COUNT+1))
		else
			echo -e " ${GREEN}${currentPasses} check(s) passed and ${currentFailures} check(s) failed${NORMAL}"
			((PASS_COUNT=PASS_COUNT+1))
		fi
	done
	echo
	if [ "$FAIL_COUNT" -eq 0 ]; then
		echo -e "${GREEN}${PASS_COUNT} AST test(s) passed and ${FAIL_COUNT} failed.${NORMAL}"
	else
		echo -e "${RED}${PASS_COUNT} AST test(s) passed and ${FAIL_COUNT} failed.${NORMAL}"
		exit 1
	fi
else
	echo
	echo -e "${YELLOW}Skipping AST checks in Buildkite CI${NORMAL}"
fi

if [[ ${DMD} == "gdmd" ]]
then
	echo "GDC / GDMD does not support -cov"
	exit 0;
fi

echo
find . -name "*.lst" -exec rm -f {} \;
echo -en "Generating coverage reports... "
${DMD} tester.d -cov -unittest $SOURCE_FILES "stdxalloc.a" $IMPORT_PATHS
./tester --ast --DRT-testmode=run-main $PASS_FILES $FAIL_FILES &> /dev/null || true
rm -rf coverage/
mkdir coverage/
find . -name "*.lst" | while read -r i; do
	dest=$(echo "$i" | sed -e "s/\\.\\.\\-//")
	mv "$i" "coverage/$dest";
done
echo -e "${GREEN}DONE${NORMAL}"
for i in coverage/*.lst; do
	tail "$i" -n1
done

rm -f tester.o
