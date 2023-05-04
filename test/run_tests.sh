#!/bin/bash

set -euo pipefail

PASS_FILES=$(find pass_files -name "*.d" | sort)
FAIL_FILES=$(find fail_files -name "*.d" | sort)
PASS_COUNT=0
FAIL_COUNT=0
NORMAL="\033[01;0m"
GREEN="\033[32m"
RED="\033[31m"
YELLOW="\033[33m"
DMD=${DMD:=dmd}
SOURCE_FILES="../src/std/experimental/*.d ../src/dparse/*.d "
IMPORT_PATHS="-I../src/"

echo -en "Compiling parse tester... "
${DMD} tester.d -debug $SOURCE_FILES -g $IMPORT_PATHS
echo -e "${GREEN}DONE${NORMAL}"

test_fail() {
	set +e
	./tester "$@"
	test_fail_status=$?
	set -e
	if [ $test_fail_status -eq 1 ]; then
		return 0
	else
		return 1
	fi
}

# if tester segfaults, it's most likely due to a stack overflow
# check the maxStackSize variable in tester.d in that case
# (increasing it should be avoided if it's possible to implement tail recursion or other stack saving techniques)

for i in $PASS_FILES; do
	echo -en "Parsing $i..."
	if ./tester "$i" 2>/dev/null 1>/dev/null; then
		echo -e "\t${GREEN}PASS${NORMAL}"
		((PASS_COUNT=PASS_COUNT+1))
	else
		echo -e "\t${RED}FAIL${NORMAL}"
		((FAIL_COUNT=FAIL_COUNT+1))
		./tester "$i"
	fi
done

for i in $FAIL_FILES; do
	echo -en "Parsing $i..."
	if test_fail "$i" 2>/dev/null 1>/dev/null; then
		echo -e "\t${GREEN}PASS${NORMAL}"
		((PASS_COUNT=PASS_COUNT+1))
	else
		echo -e "\t${RED}FAIL${NORMAL}"
		((FAIL_COUNT=FAIL_COUNT+1))
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
		lineCount=1
		currentPasses=0
		currentFailures=0
		expectParseFailure=0
		set +e
		AST="$(./tester --ast "$file" 2>/dev/null)"
		test_fail_status=$?
		set -e
		while read -r line || [ -n "$line" ]; do
			if [[ "$line" == "INCLUDES_PARSE_ERROR" ]]; then
				expectParseFailure=1
			elif echo "$AST" | xmllint --xpath "${line}" - 2>/dev/null > /dev/null; then
				((currentPasses=currentPasses+1))
			else
				echo
				echo -e "    ${RED}Check on line $lineCount of $queryFile failed.${NORMAL}"
				((currentFailures=currentFailures+1))
			fi
			((lineCount=lineCount+1))
		done < "$queryFile"

		if [[ $expectParseFailure -eq 0 ]]; then
			if [[ $test_fail_status -ne 0 ]]; then
				echo -e "    ${RED}D parsing of $queryFile failed in general.${NORMAL}"
				./tester --ast "$file" >/dev/null
				((currentFailures=currentFailures+1))
			fi
		fi

		if [[ $currentFailures -gt 0 ]]; then
			echo -e "    ${RED}${currentPasses} check(s) passed and ${currentFailures} check(s) failed${NORMAL}"
			((FAIL_COUNT=FAIL_COUNT+1))

			if [ -z "${VERBOSE:-}" ]; then
				echo -e "    Run with VERBOSE=1 to print AST XML"
			else
				echo
				./tester --ast "$file" | xmllint --format -
				echo
			fi
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
${DMD} tester.d -debug -cov -unittest $SOURCE_FILES $IMPORT_PATHS
# if tester segfaults, it's most likely due to a stack overflow
# check the maxStackSize variable in tester.d in that case
# (increasing it should be avoided if it's possible to implement tail recursion or other stack saving techniques)
./tester --ast --DRT-testmode=run-main $PASS_FILES $FAIL_FILES ast_checks/*.d > /dev/null
rm -rf coverage/
mkdir coverage/
find . -name "*.lst" | while read -r i; do
	dest=$(echo "$i" | sed -e "s/\\.\\.\\-//")
	mv "$i" "coverage/$dest";
done
echo -e "${GREEN}DONE${NORMAL}"
for i in coverage/*.lst; do
	tail -n1 "$i"
done

rm -f tester.o
