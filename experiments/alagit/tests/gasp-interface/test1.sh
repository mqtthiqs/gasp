#!/usr/bin/env bash

source config.sh
echo '* Create a repository.'
$GASP --do init
echo '* Require a "just-save" integration of "foo.module" in the repository.'
$GASP --do commit foo foo.module save 
echo '* Again, just save the "foo2.module" in the repository.'
$GASP --do commit foo2 foo2.module save
echo '* Now, we check out this saved module, again, just as raw syntax.'
$GASP --do checkout foo2 foo3.raw-fragment
