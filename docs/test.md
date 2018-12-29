# Test

## Set Environment Variables

In terms of the environment variables, please read the reference,
[docs/environment_variables.md](docs/environment_variables.md).

If "bin/faasshell.env" is created referring to [Development Environment](docs/development_environment.md), type the following command.

```sh
$ source bin/faasshell.env
```

Otherwise, type the following commands manually.

```sh
$ # arn:aws:lambda:{aws_region}:{aws_account_id}:function:hello
$ export aws_region=XX-XXXX-X
$ export aws_account_id=XXXXXXXXXXXX
$ export AWS_ACCESS_KEY_ID=XXXXXXXXXXXXXXXXXXXX
$ export AWS_SECRET_ACCESS_KEY=XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

$ # https://{azure_webapp_name}.azurewebsites.net/api/hello?code={AZURE_HOSTKEY}
$ export azure_webapp_name=XXXXXXXXXXXXXXXXXXXXXX
$ export AZURE_HOSTKEY=XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
$ export AZURE_TENANT_ID=XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX
$ export AZURE_CLIENT_ID=XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX
$ export AZURE_CLIENT_SECRET=XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

$ # https://{gcp_location_id}-{gcp_project_id}.cloudfunctions.net/hello
$ export gcp_location_id=us-central1
$ export gcp_project_id=XXXXXXX-XXXXXXX-XXXXXX 
$ export GOOGLE_APPLICATION_CREDENTIALS="/home/your_id/credential.json"

$ export WSK_APIHOST=https://IP_OR_FQDN
$ export WSK_AUTH=XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX:XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

$ export IFTTT_KEY=XXXXXXXXXXXXXXXXXXXXX
```

## Unit Test

```sh
$ docker run -d -p 5984:5984 apache/couchdb

$ make
```

## Functional Test

```sh
$ docker run -d -p 5984:5984 apache/couchdb

$ make -e docker_image_prefix=YOUR_PREFIX run

$ make functional_test
```

## Tests with Kafka

### Start kafka

```sh
$ docker-compose -f docs/kafka-docker-compose-single.yml up
```

### Start faasshell

```sh
$ export FAASSHELL_MQ=kafka

$ proj=/home/your_id/your_working_directory/faasshell/lib

$ export CLASSPATH=${proj}/kafka-clients-0.11.0.3.jar:${proj}/slf4j-api-1.7.25.jar:${proj}/slf4j-log4j12-1.7.25.jar:${proj}/log4j-1.2.17.jar

$ export _JAVA_OPTIONS="-Dconfig.location=file -Dlog4j.configuration=file://${proj}/log4j.properties"

$ docker run -d -p 5984:5984 apache/couchdb

$ make -e docker_image_prefix=YOUR_PREFIX run
```

### Run tests

```sh
$ swipl -q -l tests/unit/test_faasshell_run.pl -g kafka_api:debug_kafka -g 'run_tests(activity_task)' -g 'run_tests(event_state)' -t halt

$ swipl -q -l tests/functional/test_activity.pl -g run_tests -t halt

$ swipl -q -l tests/functional/test_trigger.pl -g run_tests -t halt
```
