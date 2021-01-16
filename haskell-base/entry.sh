#!/bin/bash
# Adapted from https://denibertovic.com/posts/handling-permissions-with-docker-volumes/
# Add local user
# Either use the LOCAL_USER_ID if passed in at runtime or
# fallback

USER_ID=${LOCAL_USER_ID:-9001}

echo "Starting with UID : $USER_ID"
usermod -u $USER_ID user
export HOME=/home/user

exec su user -c "$*"
