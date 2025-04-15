#!/usr/bin/env sh

podman run -it --rm \
  -p 9091:9091 \
  -v $(pwd)/configuration.yml:/config/configuration.yml:ro \
  -v $(pwd)/users_database.yml:/config/users_database.yml:ro \
  docker.io/authelia/authelia:4

