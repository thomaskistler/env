#!/bin/bash

cd $HOME
rm -rf .bashrc .emacs
ln -s env/.bashrc .bashrc
ln -s env/.emacs .emacs 

