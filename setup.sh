#!/bin/bash

cd $HOME
rm -rf .bashrc .emacs .tmux.conf .zshrc .oh-my-zsh .bash_profile .p10k.zsh
ln -s env/.bashrc .bash_profile
ln -s env/.bashrc .bashrc
ln -s env/.emacs .emacs 
ln -s env/.tmux.conf .tmux.conf
ln -s env/.zshrc .zshrc
ln -s env/.oh-my-zsh .oh-my-zsh
ln -s env/.p10k.zsh .p10k.zsh

cd env/
git submodule init
git submodule update
