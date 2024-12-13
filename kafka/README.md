# Kafka

## Run locally as standalone Docker container

``` bash
podman run -it --rm --name mykafka --hostname mykafka -m 256m \
    -e KAFKA_CFG_NODE_ID=0 \
    -e KAFKA_CFG_PROCESS_ROLES=controller,broker \
    -e KAFKA_CFG_LISTENERS=PLAINTEXT://:9092,CONTROLLER://:9093 \
    -e KAFKA_CFG_LISTENER_SECURITY_PROTOCOL_MAP=CONTROLLER:PLAINTEXT,PLAINTEXT:PLAINTEXT \
    -e KAFKA_CFG_CONTROLLER_QUORUM_VOTERS=0@mykafka:9093 \
    -e KAFKA_CFG_CONTROLLER_LISTENER_NAMES=CONTROLLER \
    -p 9092:9092 \
    -p 9093:9093 \
    docker.io/bitnami/kafka
```

In this repository, you will find a script `./bin/start-kafka.sh` that will do just that.
