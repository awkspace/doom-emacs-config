#!/bin/sh

rm -rf ~/.emacs.d/.local/{autoloads.*,elpa,straight,etc}
yes | ~/.emacs.d/bin/doom refresh
