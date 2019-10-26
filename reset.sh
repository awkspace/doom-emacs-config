#!/bin/sh

rm -rf ~/.emacs.d/.local/{autoloads.*,elpa/*,straight/*}
yes | ~/.emacs.d/bin/doom refresh
yes | ~/.emacs.d/bin/doom autoloads
