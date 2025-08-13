FROM docker.io/alpine:3

VOLUME /exports
EXPOSE 111 2049 20048

RUN apk update && apk add nfs-utils

COPY entrypoint.sh /entrypoint.sh
COPY exports /etc/exports

ENTRYPOINT ["/entrypoint.sh"]
