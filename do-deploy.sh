#!/bin/bash

SOURCE='site-generated/'
TARGET='javran.github.io/'

TIMESTAMP=`date`

echo '>>>> Updating submodule ...'

rsync -cvr --delete  --exclude=.git/ "${SOURCE}" "${TARGET}" \
    || { echo 'rsync failed' ; exit 1; }

echo '>>>> Committing changes ...'

cd "${TARGET}"

git add . --all \
    || { echo 'git add failed' ; exit 1; }

git commit -m "Updated: ${TIMESTAMP}" \
    || { echo 'git commit failed' ; exit 1; }

echo '>>>> Pushing changes ...'

git push origin master \
    || { echo 'git push failed'; exit 1; } 

echo '>>>> Done'
