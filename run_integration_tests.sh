#!/bin/sh
set -e
cd test/projects

luna-empire-result-saver --projectFile simple.project.json --out simple.results.json.test
diff -s simple.results.json simple.results.json.test
