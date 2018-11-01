#!/bin/bash

set -uexo pipefail

wget https://dlang.org/install.sh
# Required for scod - see https://github.com/MartinNowak/scod/pull/15
. $(bash install.sh dmd-2.083.0 -a)

# just to be save
git submodule update --init --recursive

dub upgrade # to fix to the newer dependencies
dub build -b ddox -v
./DGrammar/generate_html_from_libdparse.sh
cp ./DGrammar/grammar.html docs/grammar.html
