#! /usr/bin/env bash

set -euo pipefail

# switch to project root directory if we're not already there
script_directory=`dirname "${BASH_SOURCE[0]}"`
cd "$script_directory/.."
project_root=$(pwd)

# make sure everything is installed

if [ -z `which clojure` ]; then
    cat <<EOF
Error: The Clojure CLI is not installed.

See https://clojure.org/guides/install_clojure for installation instructions.

EOF
    exit -1
fi

if [ -z `which clj-kondo` ]; then
    cat <<EOF
Error: clj-kondo is not installed.

See https://github.com/clj-kondo/clj-kondo/blob/master/doc/install.md for installation instructions.

EOF
    exit -1
fi

if [ -z `which codespell` ]; then
    cat <<EOF
Error: codespell is not installed.

Install it with

pip install codespell

See https://github.com/codespell-project/codespell#installation for more information.
EOF
    exit -1
fi

# from here on out print everything we run.
set -x

# now run the linters and tests.

clj-kondo --parallel --lint src test

clojure -X:dev:test

codespell

clojure -M:check

clojure -T:whitespace-linter
