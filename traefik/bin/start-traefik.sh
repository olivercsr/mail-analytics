#!/usr/bin/env sh

podman run -it --rm \
  -p 8080:80 \
  -p 8443:443 \
  -p 8082:8080 \
  -v $(pwd)/traefik.yml:/etc/traefik/traefik.yml:ro \
  -v $(pwd)/file-provider.yml:/etc/traefik/file-provider.yml:ro \
  docker.io/traefik:v3

