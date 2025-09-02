# Kafka

## Download Apache Camel

- Download [Apache Camel timer source connector](https://repo.maven.apache.org/maven2/org/apache/camel/kafkaconnector/camel-timer-source-kafka-connector/4.8.3/camel-timer-source-kafka-connector-4.8.3-package.tar.gz)
- Extract that archive into `/camel` subfolder
- Edit `./camel/docs/examples/CamelTimersourceSourceConnector.properties`:
  - Set `topics`
  - Set `camel.kamelet.timer-source.message`

## Run Kafka locally as standalone Docker container

In this repository, you will find a script `./bin/start-kafka.sh` that
will start Kafka.

## Test Apache Camel

### Start producer / connect source

- `podman exec -it kafka-dev /bin/bash`
- `/opt/bitnami/kafka/bin/kafka-topics.sh --bootstrap-server localhost:9092 --create --replication-factor 1 --partitions 1 --topic mytopic`
- `/opt/bitnami/kafka/bin/connect-standalone.sh /opt/bitnami/kafka/config/connect-standalone.properties /kafka-plugins/docs/examples/CamelTimersourceSourceConnector.properties`

### Start consumer

- `podman exec -it kafka-dev /bin/bash`
- `/opt/bitnami/kafka/bin/kafka-console-consumer.sh --bootstrap-server localhost:9092 --topic mytopic`

