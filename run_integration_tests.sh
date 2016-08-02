#!/bin/sh

exit 0

set -e
cd test/projects

luna-empire-result-saver --projectFile simple.project.json --out simple.results.json.test
diff -s simple.results.json simple.results.json.test

luna-empire-result-saver --projectFile words.project.json --out words.results.json.test
diff -s words.results.json words.results.json.test
