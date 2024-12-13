# dmarc-importer



# Usage

Run from sources:

    make run
    # aka sbcl --load run.lisp

choose your lisp:

    LISP=ccl make run

or build and run the binary:

```
$ make build
$ ./dmarc-importer [name]
Hello [name] from dmarc-importer
```

## Roswell integration

Roswell is an implementation manager and [script launcher](https://github.com/roswell/roswell/wiki/Roswell-as-a-Scripting-Environment).

A POC script is in the roswell/ directory.

Your users can install the script with `owegner/dmarc-importer`.

# Dev

Tests are defined with [Fiveam](https://common-lisp.net/project/fiveam/docs/).

Run them from the terminal with `make test`. You should see a failing test.

```bash
$ make test
Running test suite TESTMAIN
 Running test TEST1 f
 Did 1 check.
    Pass: 0 ( 0%)
    Skip: 0 ( 0%)
    Fail: 1 (100%)

 Failure Details:
 --------------------------------
 TEST1 in TESTMAIN []:

3

 evaluated to

3

 which is not

=

 to

2

Makefile:15: recipe for target 'test' failed

$ echo $?
2
```

On Slime, load the test package and run `run!`.

---

Licence: BSD



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
