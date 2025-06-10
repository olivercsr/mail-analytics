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
  --name existdb \
  --hostname "${EXISTDB_NAME}.local" \
  -p 8080:8080 \
  -p 8443:8443 \
  -p 3333:3333 \
  -e JAVA_TOOL_OPTIONS="-Dfile.encoding=UTF8 -Dsun.jnu.encoding=UTF-8 -Djava.awt.headless=true -Dorg.exist.db-connection.cacheSize=256M -Dorg.exist.db-connection.pool.max=20 -Dlog4j.configurationFile=/exist/etc/log4j2.xml -Dexist.home=/exist -Dexist.configurationFile=/exist/etc/conf.xml -Djetty.home=/exist -Dexist.jetty.config=/exist/etc/jetty/standard.enabled-jetty-configs -XX:+UseG1GC -XX:+UseStringDeduplication -XX:+UseContainerSupport -XX:MaxRAMPercentage=75.0 -XX:+ExitOnOutOfMemoryError -Dcom.sun.management.jmxremote -Dcom.sun.management.jmxremote.port=3333 -Dcom.sun.management.jmxremote.rmi.port=3333 -Dcom.sun.management.jmxremote.local.only=false -Dcom.sun.management.jmxremote.authenticate=false -Dcom.sun.management.jmxremote.ssl=false" \
  -v existdb-data:/exist/data:Z \
  docker.io/existdb/existdb:release
