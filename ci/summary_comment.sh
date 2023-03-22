#!/usr/bin/env bash

set -u

# Output from this script is piped to a file by CI, being run from before a
# change has been made and after a change has been made. Then both outputs are
# compared using summary_comment_diff.sh

# cd to git folder, just in case this is manually run:
ROOT_DIR="$( cd "$(dirname "${BASH_SOURCE[0]}")/../" && pwd )"
cd ${ROOT_DIR}

dub --version
ldc2 --version

# fetch missing packages before timing
dub upgrade --missing-only

start=`date +%s`
dub build --build=release --force 2>&1 || echo "LIB BUILD FAILED"
end=`date +%s`
build_time=$( echo "$end - $start" | bc -l )

strip libdparse.a

echo "STAT:------ libdparse statistics ------"
echo "STAT:"
echo "STAT:statistics (-before, +after)"
echo "STAT:library size=$(wc -c libdparse.a)"
echo "STAT:rough build time=${build_time}s"
echo "STAT:"

cleanup() {
	rm -rf "$ROOT_DIR/DCD"
}
trap cleanup EXIT

git clone https://github.com/dlang-community/DCD.git
cd DCD
echo "STAT:"
echo "STAT:------ DCD statistics ------"
echo "STAT:"

sed -E -i 's/"libdparse":\s*"[^"]+"/"libdparse": {"path":".."}/' dub.json
cat dub.json
sed -E -i 's/"libdparse":\s*"[^"]+"/"libdparse": {"path":".."}/' dub.selections.json
cat dub.selections.json

./ci/summary_comment.sh 2>&1 | grep -E "^STAT:|DCD BUILD FAILED"
