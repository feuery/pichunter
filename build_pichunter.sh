#!/bin/env bash
set -euo pipefail

apt-get update
apt-get install sbcl curl libssl-dev -q -y

curl https://beta.quicklisp.org/quicklisp.lisp -o ~/quicklisp.lisp
sbcl --load ~/quicklisp.lisp --load ./install_ql.lisp

current_dir=$(pwd)

mkdir ~/common-lisp
pushd ~/common-lisp
ln -s $current_dir
popd


if [ ! -d ~/common-lisp ]; then
    current_dir=$(pwd)
    
    mkdir ~/common-lisp
    pushd ~/common-lisp
    ln -s $current_dir
    popd
fi

sbcl --load ./build_pichunter.lisp

mv pichunter_server ..

apt-get remove sbcl -q -y
