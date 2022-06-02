#!/bin/sh

if [ -d "$HOME/.doom.d" ]
then
  git -C "$HOME/.doom.d" pull --rebase
else
  if ssh -o ConnectTimeout=3 -T git@github.com </dev/null 2>&1 | grep "^Hi awkspace!" > /dev/null
  then
    git clone ssh://git@github.com/awkspace/doom-emacs-config "$HOME/.doom.d" -o github
  else
    git clone https://github.com/awkspace/doom-emacs-config "$HOME/.doom.d" -o github
  fi
fi

if [ -d "$HOME/.emacs.d" ] && [ ! -d "$HOME/.emacs.d/.git" ]
then
  mv "$HOME/.emacs.d" "$HOME/.emacs.d.bak"
fi

if [ -d "$HOME/.emacs.d" ]
then
  git -C "$HOME/.emacs.d" pull --rebase
  yes | "$HOME/.emacs.d/bin/doom" upgrade
else
  git clone https://github.com/hlissner/doom-emacs "$HOME/.emacs.d" -o github
fi

yes | "$HOME/.emacs.d/bin/doom" sync
yes | "$HOME/.emacs.d/bin/doom" upgrade
