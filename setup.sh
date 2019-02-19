#!/usr/bin/bash

if [ ! -d ~/.doom.d ]
then
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
    rm -rf ~/.emacs.d/.local/packages.bak
    mv ~/.emacs.d/.local/packages ~/.emacs.d/.local/packages.bak
else
    git clone https://github.com/hlissner/doom-emacs ~/.emacs.d -b develop -o github
fi

cd ~/.emacs.d && yes | make quickstart
cd
