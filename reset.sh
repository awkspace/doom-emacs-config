#!/bin/sh

find ~/.emacs.d/.local -mindepth 1 -not -path "*/cache" -not -path "*/cache/*" -prune -exec rm -rf "{}" \;
yes | ~/.emacs.d/bin/doom install
