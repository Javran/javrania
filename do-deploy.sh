#!/bin/bash

SOURCE='site-generated/'
#TARGET='javran.github.io/'
TARGET='xxx/'

echo '>>> Updating submodule ...'

rsync -cvvr --delete  --exclude=.git/  "${SOURCE}" "${TARGET}"

