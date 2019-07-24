#!/usr/bin/env bash

set -o errexit
jsonnet transpose.jsonnet | diff -w transpose.golden -
