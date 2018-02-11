#!/bin/bash

set -euo pipefail

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
LIBDPARSE_PATH=${DIR}/..
cd $DIR

dmd -D ${LIBDPARSE_PATH}/src/dparse/parser.d\
	-I${LIBDPARSE_PATH}/src\
	-I${LIBDPARSE_PATH}/stdx-allocator/source\
	${LIBDPARSE_PATH}/macros.ddoc -c -o-

# install pup (if not existent)
pup=pup
if [[ $(command -v "$pup" 2>&1 > /dev/null || echo 131) -eq 131 ]] ; then
    if [ -f $DIR/pup ] ; then
        pup="./pup"
    else
        version="v0.4.0"
        shasum="c1706d13aa04f3665b4a3a73958870268b41c672"
        out="pup_${version}_linux_amd64.zip"
        wget "https://github.com/ericchiang/pup/releases/download/${version}/pup_${version}_linux_amd64.zip" -O "${out}"
        echo "${shasum}  ${out}" | sha1sum -c -
        unzip pup_${version}_linux_amd64.zip
        pup="./pup"
    fi
fi

cat begin.txt > grammar.html
cat parser.html | "$pup" 'pre[class="grammar"]' --pre >> grammar.html
cat end.txt >> grammar.html
