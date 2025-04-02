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

EXISTDB_NAME=${EXISTDB_NAME:-existdb-dev}

${DOCKER_CMD} run -it --rm \
  --name "${EXISTDB_NAME}" \
  --hostname "${EXISTDB_NAME}.local" \
  -m 768m \
  -p 8080:8080 \
  -p 8443:8443 \
  docker.io/existdb/existdb:release
