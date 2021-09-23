# awk's Emacs runfile

```sh
run_set EMACS_VERSION "27.2"
```

## install

Install awk's Doom Emacs configuration.
```sh
wget -O- https://raw.githubusercontent.com/awkspace/doom-emacs-config/master/setup.sh | bash
```

## reset

Forcibly reinstall Doom Emacs.
```sh
(cd "$HOME/.doom.d" && ./reset.sh)
```

## debian_install_emacs
```sh
mkdir -p "$HOME/.local/src"
if [ ! -d "$HOME/.local/src/emacs-$EMACS_VERSION" ]
then
    cd "$HOME/.local/src"
    if [ ! -f "$HOME/.local/src/emacs-$EMACS_VERSION.tar.gz" ]
    then
        wget "https://ftp.gnu.org/gnu/emacs/emacs-$EMACS_VERSION.tar.gz"
    fi
    tar xapf "$HOME/.local/src/emacs-$EMACS_VERSION.tar.gz"
fi
cd "$HOME/.local/src/emacs-$EMACS_VERSION"
sudo apt update
. /etc/os-release
if [ "$VERSION_CODENAME" = "bullseye" ]
then
    sudo apt build-dep emacs -y
else
    sudo apt build-dep emacs25 -y
fi
./configure
make -j $(nproc)
sudo make install
```

## debian_install_doom
```
requires:
  - install
```

## debian_install_extras
```sh
sudo apt-get install -y \
    fonts-go \
    fonts-noto-mono \
    pandoc \
    markdown
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.38.0/install.sh | bash
if which npm; then npm install -g marked; fi
if which go; then GO111MODULE=on go get golang.org/x/tools/gopls@latest; fi
```

## macos_install_emacs

```sh
brew install emacs
```

## macos_install_extras

```sh
brew install homebrew/cask-fonts/font-{go,noto}-mono
```
