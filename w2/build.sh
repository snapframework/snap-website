#!/bin/sh

build="./builder env.conf"

set -x

ghc --make ./builder.hs || exit 1
rsync -auP --delete src/ site/

$build src/index.html > site/index.html
$build src/media/css/main.css > site/media/css/main.css
