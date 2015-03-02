#!/bin/bash

cd $HOME
rm -rf .bashrc .emacs
ln -s env/.bashrc .bash_profile
ln -s env/.bashrc .bashrc
ln -s env/.emacs .emacs 
ln -s env/.tmux.conf .tmux.conf
