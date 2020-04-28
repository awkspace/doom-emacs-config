#!/bin/sh

find ~/.emacs.d/.local -not -path "*/cache" -not -path "*/cache/*" -exec rm -rf "{}" \;
yes | ~/.emacs.d/bin/doom install
