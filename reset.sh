#!/bin/sh

find ~/.emacs.d/.local -mindepth 1 \
    -not -path "*/cache" -not -path "*/cache/*" \
    -not -path "*/etc" -not -path "*/etc/*" \
    -prune -exec rm -rf "{}" \;
find ~/.emacs.d/ -mindepth 1 -name *.elc -exec rm -f "{}" \;
yes | ~/.emacs.d/bin/doom install
