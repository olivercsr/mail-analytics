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

KAFKA_NAME=${KAFKA_NAME:-kafka-dev}

# in order to run with camel, you need to download
#  https://repo.maven.apache.org/maven2/org/apache/camel/kafkaconnector/camel-timer-source-kafka-connector/4.8.3/camel-timer-source-kafka-connector-4.8.3-package.tar.gz
# and extract that archive into ./camel/ :
${DOCKER_CMD} run -it --rm \
  --name "${KAFKA_NAME}" \
  --hostname "${KAFKA_NAME}" \
  -m 512m \
  -e KAFKA_CFG_NODE_ID=0 \
  -e KAFKA_CFG_PROCESS_ROLES=controller,broker \
  -e KAFKA_CFG_LISTENERS=PLAINTEXT://:9092,CONTROLLER://:9093 \
  -e KAFKA_CFG_LISTENER_SECURITY_PROTOCOL_MAP=CONTROLLER:PLAINTEXT,PLAINTEXT:PLAINTEXT \
  -e KAFKA_CFG_CONTROLLER_QUORUM_VOTERS=0@"${KAFKA_NAME}":9093 \
  -e KAFKA_CFG_CONTROLLER_LISTENER_NAMES=CONTROLLER \
  -e KAFKA_CFG_AUTO_CREATE_TOPICS_ENABLE=true \
  -e KAFKA_CFG_PLUGIN_PATH=/plugins \
  -p 9092:9092 \
  -p 9093:9093 \
  -v ./config/connect-standalone.properties:/opt/bitnami/kafka/config/connect-standalone.properties:ro \
  -v ./camel:/kafka-plugins/:ro \
  docker.io/bitnami/kafka
