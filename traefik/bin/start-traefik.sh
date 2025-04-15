#!/usr/bin/env sh

podman run -it --rm \
  -p 8080:8080 \
  -v $(pwd)/traefik.yml:/etc/traefik/traefik.yml:ro \
  -v $(pwd)/file-provider.yml:/etc/traefik/file-provider.yml:ro \
  docker.io/traefik:v3

