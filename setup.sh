#!/bin/bash

cd $HOME
rm -rf .bashrc .emacs
ln -s env/.bashrc .bashrc
ln -s env/.emacs .emacs 

cd $HOME/.config
rm -rf xfce4
ln -s $HOME/env/xfce4 xfce4
