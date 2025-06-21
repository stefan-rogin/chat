#!/bin/bash

set -euo pipefail
EC2_HOST="$1"

rebar3 release
tar -czf "chat_release.tar.gz" -C "_build/default/rel" chat
scp -i $SERVICE_CHAT_AWS_KEY_PATH "chat_release.tar.gz" ubuntu@"$EC2_HOST":/home/ubuntu/
rm chat_release.tar.gz 

ssh -i $SERVICE_CHAT_AWS_KEY_PATH ubuntu@"$EC2_HOST" bash -s <<EOF
  set -e
  mkdir -p /home/ubuntu/chat
  tar -xzf chat_release.tar.gz -C /home/ubuntu
  rm -f chat_release.tar.gz
  cd /home/ubuntu/chat/bin
  ./chat stop
  ./chat daemon
EOF

echo "Done."