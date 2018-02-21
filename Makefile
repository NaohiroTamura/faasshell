# Copyright 2017 FUJITSU LIMITED
#
# Licensed under the Apache License, Version 2.0 (the "License"); you may
# not use this file except in compliance with the License. You may obtain
# a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations
# under the License.
#

docker_image_prefix =
docker_image_tag = :latest

unit_test_files := $(wildcard tests/unit/test_*.pl)
functional_test_files := $(wildcard tests/functional/test_*.pl)

JAR = /opt/kafka/libs
CLASSPATH=$(JAR)/kafka-clients-0.11.0.2.jar:$(JAR)/slf4j-api-1.7.25.jar:$(JAR)/slf4j-log4j12-1.7.25.jar:$(JAR)/log4j-1.2.17.jar
_JAVA_OPTIONS="-Dconfig.location=file -Dlog4j.configuration=file://$(PWD)/lib/log4j.properties"

all: unit_test

unit_test:
	@echo "unit  test"
	for case in $(unit_test_files); do \
		echo $$case; \
		swipl -q -l $$case -g run_tests -t halt; \
	done

functional_test:
	@echo "functional  test"
	swipl -q -l src/asl_svc.pl -g main -t halt &
	swipl -q -l tests/unit/unit_test_utils.pl -g functional_test_setup -t halt
	sleep 3
	for case in $(functional_test_files); do \
		echo $$case; \
		swipl -q -l $$case -g run_tests -t halt; \
	done
	pkill -HUP swipl

test_with_kafka:
	@echo "unit and functional tests with kafka"
	zookeeper-server-start.sh /opt/kafka/config/zookeeper.properties > /dev/null &
	sleep 3
	kafka-server-start.sh /opt/kafka/config/server.properties > /dev/null &
	sleep 3
	CLASSPATH=$(CLASSPATH) _JAVA_OPTIONS=$(_JAVA_OPTIONS) \
	swipl -q -l tests/unit/test_asl_run.pl -g kafka_api:debug_kafka -g 'run_tests(activity_task)' -t halt
	sleep 1
	CLASSPATH=$(CLASSPATH) _JAVA_OPTIONS=$(_JAVA_OPTIONS) \
	swipl -q -l src/asl_svc.pl -g main -t halt &
	sleep 10
	CLASSPATH=$(CLASSPATH) _JAVA_OPTIONS=$(_JAVA_OPTIONS) \
	swipl -q -l tests/functional/test_activity.pl -g run_tests -t halt
	pkill -HUP swipl
	pkill -KILL java

build_image:
	@echo "create build image"
	docker build -t s2i-swipl .

# make -e docker_image_prefix=myprefix/ -e docker_image_tag=:0.1 app_image
app_image:
	@echo "create application image"
	s2i build src s2i-swipl $(docker_image_prefix)faasshell$(docker_image_tag)

# make -e docker_image_prefix=myprefix/ -e docker_image_tag=:0.1 deploy
deploy:
	@echo "deploy app_image to kubernetes"
	docker push  $(docker_image_prefix)faasshell$(docker_image_tag)
	envsubst < faasshell.yml | kubectl apply -f -

undeploy:
	@echo "undeploy app_image from kubernetes"
	kubectl delete -f faasshell.yml

# make -e docker_image_prefix=myprefix/ -e docker_image_tag=:0.1 run
run:
	@echo "run app_image in docker"
	docker run -d \
	           -e $$(grep APIHOST ~/.wskprops) \
	           -e DB_AUTH=whisk_admin:some_passw0rd \
	           -e DB_APIHOST=172.17.0.1:5984 \
	           -p 8080:8080 -v /tmp:/logs \
	           $(docker_image_prefix)faasshell$(docker_image_tag)

debug:
	@echo "run asl_svc for debugging"
	swipl -q -l src/asl_svc.pl -g main -t halt

