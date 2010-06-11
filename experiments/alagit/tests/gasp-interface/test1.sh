#!/usr/bin/env bash

source config.sh
# Create a repository
$GASP --do init                    
# Require an "just-save" integration of "foo.module" in the repository
$GASP --do commit foo foo.module save 
# Again, just save the "foo2.module" in the repository
$GASP --do commit foo2 foo2.module save
