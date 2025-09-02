#!/usr/bin/env bash

podman run -it --rm \
  -e LLDAP_KEY_SEED=a12345678901234567890 \
  -e LLDAP_JWT_SECRET=a12345678901234567890 \
  -e LLDAP_LDAP_BASE_DN=dc=csr-informatik,dc=de \
  -e LLDAP_LDAP_USER_PASS=foobar123 \
  -e TZ=Europe/Berlin \
  -p17170:17170 -p3890:3890 \
  -v lldap_data:/data:Z \
  docker.io/lldap/lldap:latest

