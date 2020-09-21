#!/bin/sh

find ~/.emacs.d/.local -mindepth 1 \
    -not -path "*/cache" -not -path "*/cache/*" \
    -not -path "*/etc" -not -path "*/etc/*" \
    -prune -exec rm -rf "{}" \;
yes | ~/.emacs.d/bin/doom install
