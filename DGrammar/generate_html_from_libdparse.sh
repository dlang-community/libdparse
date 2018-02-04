#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
LIBDPARSE_PATH=${DIR}/..
cd $DIR

dmd -D ${LIBDPARSE_PATH}/src/dparse/parser.d\
	-I${LIBDPARSE_PATH}/src\
	${LIBDPARSE_PATH}/macros.ddoc -c

cat begin.txt > grammar.html
xmllint --html --xpath "//pre[@class='grammar']" parser.html >> grammar.html
cat end.txt >> grammar.html
