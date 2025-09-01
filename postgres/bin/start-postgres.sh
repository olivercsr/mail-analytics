#!/usr/bin/env sh

DOCKER_CMD=
if [ -x "$(command -v podman)" ]; then
  DOCKER_CMD="$(command -v podman)"
elif [ -x "$(command -v docker)" ]; then
  DOCKER_CMD="$(command -v docker)"
else
  echo "ERROR: could not find either docker or podman" >&2
  exit 1
fi

${DOCKER_CMD} run -it --rm \
  --name=postgres \
  -p 5432:5432 \
  -v dmarc-app-db:/var/lib/postgresql/data \
  -e POSTGRES_USER=dmarc \
  -e POSTGRES_PASSWORD=dmarc \
  -e POSTGRES_DB=dmarc \
  docker.io/postgres:17-alpine

