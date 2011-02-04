#!/bin/bash

GASP=../gasp

shopt -s nullglob

for sign in *.elf; do
    echo "> init $sign" &&
    $GASP init --repo $sign.gasp $sign &&
    echo "> check" &&
    $GASP --repo $sign.gasp check &&
    for commit in `basename $sign .elf`-*.tm; do
	echo "> commit $commit" &&
	$GASP commit --repo $sign.gasp $commit &&
	echo "> check" &&
	$GASP --repo $sign.gasp check &&
	echo "> show" &&
	$GASP --repo $sign.gasp show
    done
    if [ $? != 0 ]; then 
	exit 1; 
    fi
done
