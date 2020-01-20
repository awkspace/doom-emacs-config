#!/usr/bin/bash

if [ -d ~/.doom.d ]
then
  git -C ~/.doom.d pull --rebase
else
  ssh -o ConnectTimeout=3 -T git@github.com </dev/null 2>&1 | grep "^Hi awkspace!" > /dev/null
  if [[ $? -eq 0 ]]
  then
    git clone ssh://git@github.com/awkspace/doom-emacs-config ~/.doom.d -o github
  else
    git clone https://github.com/awkspace/doom-emacs-config ~/.doom.d -o github
  fi
fi

if [ -d ~/.emacs.d ]
then
  git -C ~/.emacs.d pull --rebase
  yes | ~/.emacs.d/bin/doom upgrade
else
  git clone https://github.com/hlissner/doom-emacs ~/.emacs.d -b develop -o github
fi

yes | ~/.emacs.d/bin/doom upgrade
yes | ~/.emacs.d/bin/doom refresh
