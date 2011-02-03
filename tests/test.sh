#!/bin/bash

shopt -s nullglob

for sign in *.elf; do
    echo "> init $sign"
    ../gasp init --repo $sign.gasp $sign &&
    for commit in `basename $sign .elf`-*.tm; do
	echo "> commit $commit" &&
	../gasp commit --repo $sign.gasp $commit &&
	echo "> show" &&
	../gasp --repo $sign.gasp show
    done
    if [ $? != 0 ]; then 
	exit 1; 
    fi
done
