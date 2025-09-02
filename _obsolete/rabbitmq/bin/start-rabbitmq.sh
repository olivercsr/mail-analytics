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

RABBITMQ_NAME=${RABBITMQ_NAME:-rabbitmq-dev}

${DOCKER_CMD} run -it --rm \
  --name "${RABBITMQ_NAME}" \
  --hostname "${RABBITMQ_NAME}.local" \
  -m 512m \
  -p 5672:5672 \
  -v ./99-dev.conf:/etc/rabbitmq/conf.d/99-dev.conf:ro \
  docker.io/rabbitmq:3-alpine
