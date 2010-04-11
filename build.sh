#!/bin/sh

build="./builder"

set -x
rsync -auP w2/ build/

$build w2/index.html > build/index.html
$build w2/media/css/main.css > build/media/css/main.css
