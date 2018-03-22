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
SHELL:=/bin/bash

# set default value
docker_image_tag = :latest
GOOGLE_APPLICATION_CREDENTIALS ?= /dev/null

unit_test_files := $(wildcard tests/unit/test_*.pl)
functional_test_files := $(wildcard tests/functional/test_*.pl)

JAR = /opt/kafka/libs
CLASSPATH=$(JAR)/kafka-clients-0.11.0.2.jar:$(JAR)/slf4j-api-1.7.25.jar:$(JAR)/slf4j-log4j12-1.7.25.jar:$(JAR)/log4j-1.2.17.jar
_JAVA_OPTIONS="-Dconfig.location=file -Dlog4j.configuration=file://$(PWD)/lib/log4j.properties"

all: unit_test

cert:
	@echo generate self signed certificate
	openssl req -x509 -newkey rsa:2048 \
		-keyout etc/server/server-key.pem \
		-out etc/server/server-cert.pem \
		-nodes -subj "/CN=127.0.0.1" -days 365
	chmod 0400 etc/server/server-*.pem

unit_test:
	@echo "unit  test"
	for case in $(unit_test_files); do \
		echo $$case; \
		swipl -q -l $$case -g run_tests -t halt; \
	done

functional_test:
	@echo "functional  test"
	swipl -q -l src/faasshell_svc.pl -g main -t halt &
	swipl -q -l tests/unit/unit_test_utils.pl -g faas_test_setup -t halt
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
	swipl -q -l tests/unit/test_faasshell_run.pl -g kafka_api:debug_kafka -g 'run_tests(activity_task)' -t halt
	sleep 1
	CLASSPATH=$(CLASSPATH) _JAVA_OPTIONS=$(_JAVA_OPTIONS) \
	swipl -q -l src/faasshell_svc.pl -g main -t halt &
	sleep 10
	CLASSPATH=$(CLASSPATH) _JAVA_OPTIONS=$(_JAVA_OPTIONS) \
	swipl -q -l tests/functional/test_activity.pl -g run_tests -t halt
	pkill -HUP swipl
	pkill -KILL java

build_image:
	@echo "create build image"
	if [ -v $(HTTP_PROXY) -a -v $(HTTPS_PROXY) ]; \
	then \
		docker build -t s2i-swipl . ; \
	else \
		docker build -t s2i-swipl . \
			 --build-arg HTTP_PROXY=$(HTTP_PROXY) \
			 --build-arg http_proxy=$(HTTP_PROXY) \
			 --build-arg HTTPS_PROXY=$(HTTPS_PROXY) \
			 --build-arg https_proxy=$(HTTPS_PROXY) ; \
	fi

# make -e docker_image_prefix=myprefix/ -e docker_image_tag=:0.1 app_image
app_image:
	@echo "create application image"
	rm src/faasshell_version.pl
	git checkout src/faasshell_version.pl
	s2i build . s2i-swipl $(docker_image_prefix)faasshell$(docker_image_tag) -c

# make -e docker_image_prefix=myprefix/ -e docker_image_tag=:0.1 deploy
ifdef HTTP_PROXY
    ifdef HTTPS_PROXY
        faasshell_deployment = faasshell-proxy.yml
    endif
else
    faasshell_deployment = faasshell.yml
endif
deploy:
	@echo "deploy app_image to kubernetes"
	docker push  $(docker_image_prefix)faasshell$(docker_image_tag)
	kubectl -n faasshell create secret generic faasshell \
		--from-literal=faasshell_db_auth=$(FAASSHELL_DB_AUTH) \
		--from-literal=faasshell_db_apihost=$(FAASSHELL_DB_APIHOST) \
		--from-literal=aws_access_key_id=$(AWS_ACCESS_KEY_ID) \
		--from-literal=aws_secret_access_key=$(AWS_SECRET_ACCESS_KEY) \
		--from-file=gcp_app_cred=$(GOOGLE_APPLICATION_CREDENTIALS) \
		--from-literal=azure_hostkey=$(AZURE_HOSTKEY) \
		--from-literal=azure_tenant_id=$(AZURE_TENANT_ID) \
		--from-literal=azure_client_id=$(AZURE_CLIENT_ID) \
		--from-literal=azure_client_secret=$(AZURE_CLIENT_SECRET) \
		--from-literal=wsk_auth=$(WSK_AUTH) \
		--from-literal=wsk_apihost=$(WSK_APIHOST)
	envsubst < $(faasshell_deployment) | kubectl apply -f -

undeploy:
	@echo "undeploy app_image from kubernetes"
	kubectl delete -f faasshell.yml

# make -e docker_image_prefix=myprefix/ -e docker_image_tag=:0.1 run
run:
	@echo "run app_image in docker"
	if [ -v $(HTTP_PROXY) -a -v $(HTTPS_PROXY) ]; \
	then \
		docker run -d \
	           --net=host -v /tmp:/logs \
		   -e FAASSHELL_DB_AUTH=$(FAASSHELL_DB_AUTH) \
		   -e FAASSHELL_DB_APIHOST=$(FAASSHELL_DB_APIHOST) \
	           -e AWS_ACCESS_KEY_ID=$(AWS_ACCESS_KEY_ID) \
	           -e AWS_SECRET_ACCESS_KEY=$(AWS_SECRET_ACCESS_KEY) \
		   -e GCP_APP_CRED='$(shell cat $(GOOGLE_APPLICATION_CREDENTIALS))' \
	           -e AZURE_HOSTKEY=$(AZURE_HOSTKEY) \
		   -e AZURE_TENANT_ID=$(AZURE_TENANT_ID) \
		   -e AZURE_CLIENT_ID=$(AZURE_CLIENT_ID) \
		   -e AZURE_CLIENT_SECRET=$(AZURE_CLIENT_SECRET) \
	           -e WSK_AUTH=$(WSK_AUTH) -e WSK_APIHOST=$(WSK_APIHOST) \
	           $(docker_image_prefix)faasshell$(docker_image_tag) ; \
	else \
		docker run -d \
	           --net=host -v /tmp:/logs \
		   -e FAASSHELL_DB_AUTH=$(FAASSHELL_DB_AUTH) \
		   -e FAASSHELL_DB_APIHOST=$(FAASSHELL_DB_APIHOST) \
	           -e AWS_ACCESS_KEY_ID=$(AWS_ACCESS_KEY_ID) \
	           -e AWS_SECRET_ACCESS_KEY=$(AWS_SECRET_ACCESS_KEY) \
		   -e GCP_APP_CRED='$(shell cat $(GOOGLE_APPLICATION_CREDENTIALS))' \
	           -e AZURE_HOSTKEY=$(AZURE_HOSTKEY) \
		   -e AZURE_TENANT_ID=$(AZURE_TENANT_ID) \
		   -e AZURE_CLIENT_ID=$(AZURE_CLIENT_ID) \
		   -e AZURE_CLIENT_SECRET=$(AZURE_CLIENT_SECRET) \
	           -e WSK_AUTH=$(WSK_AUTH) -e WSK_APIHOST=$(WSK_APIHOST) \
		   -e HTTP_PROXY=$(HTTP_PROXY) \
		   -e HTTPS_PROXY=$(HTTPS_PROXY) \
		   -e NO_PROXY=$(NO_PROXY) \
	           $(docker_image_prefix)faasshell$(docker_image_tag) ; \
	fi

debug:
	@echo "run faasshell_svc for debugging"
	swipl -q -l src/faasshell_svc.pl -g main -t halt
