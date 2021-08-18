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
sudo apt build-dep emacs25 -y
./configure
make -j $(nproc)
sudo make install
```

## debian_install_extras

```sh
sudo apt-get install -y \
    fonts-go \
    fonts-noto-mono \
    pandoc \
    markdown
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.38.0/install.sh | bash
npm install -g marked
```
