#!/usr/bin/env bash
set -euo pipefail

script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
cd "$script_dir"

echo 'Creating required directories'
mkdir -p doc/img

echo 'Copying static JavaScript content'
cp -r js doc/.

echo 'Building image generator and creating images'
pushd imagegen
cabal build
cabal run
popd

echo 'Converting images'
pushd doc/img
for file in *.pdf; do
  base="${file%.*}"
  cropped="${base}-crop.pdf"
  svg="${base}.svg"
  pdfcrop "$base"
  pdf2svg "$cropped" "$svg"
  rm "$file"
  rm "$cropped"
done
popd

echo 'Running asciidoc'
asciidoctor \
  --backend html5 \
  --doctype book \
  --destination-dir doc \
  --out-file index.html \
  gi2d.adoc

echo 'Output: open doc/index.html'
