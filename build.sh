#!/usr/bin/env bash
set -euo pipefail

script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
cd "$script_dir"

echo 'Creating required directories'
mkdir -p doc/img

echo 'Building image generator and creating images'
pushd imagegen
cabal build
cabal run
popd

echo 'Running asciidoc'
asciidoctor \
  --backend html5 \
  --doctype book \
  --destination-dir doc \
  --out-file index.html \
  gi2d.adoc

echo 'Output: open doc/index.html'
