#!/usr/bin/env bash

set -e

echo "[BUILD]"
./bin/build-gh-pages.sh

echo "[DEPLOY]"

deploy_branch=gh-pages
deploy_dir=.dist
root_dir=.build/elm-calculator

git worktree add $deploy_dir $deploy_branch

rm -rf $deploy_dir/*
cp -r $root_dir/* $deploy_dir

git -C $deploy_dir add .

hash="$(git log -n 1 --format='%h' master)"
message="Site updated to $hash"

if git -C $deploy_dir commit -m "$message"; then
  git -C $deploy_dir push -u origin HEAD
fi

git worktree remove --force $deploy_dir

echo "[DEPLOYED]"
