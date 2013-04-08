#!/bin/sh -e

trap 'echo "let version = None"' INT TERM EXIT

if [ -e .git/logs/HEAD ]; then
    describe=`git rev-parse HEAD`
    echo let version = Some \"${describe}\"
    trap - INT TERM EXIT
else
    echo "let version = None"
fi
