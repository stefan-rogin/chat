#!/bin/bash

TAG="$1"
echo "https://github.com/stefan-rogin/chat/releases/tag/$TAG" > github.txt
zip share.zip github.txt README.md
rm github.txt
