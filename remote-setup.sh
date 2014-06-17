#!/bin/bash

if [[ -z "$1" ]]; then
    echo "Please provide host name"
    exit 1
fi

ssh $1 mkdir -p ~/env
scp -r ~/env $1:~/
ssh $1 ~/env/setup.sh

scp -r ~/.ssh $1:~/

