#!/usr/bin/env bash
set -euo pipefail

asciidoctor \
  --backend html5 \
  --doctype book \
  --destination-dir doc \
  --out-file index.html \
  gi2d.adoc
