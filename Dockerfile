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

#
# $ docker build -t s2i-swipl .
#
# JPL enabled SWI Prolog 7.5.15
# https://github.com/NaohiroTamura/docker-swipl/tree/swi-7.5.15-jpl
FROM nao16t/swipl7jpl

LABEL io.k8s.description="Platform for building and running SWI Prolog apps" \
      io.k8s.display-name="swipl" \
      io.openshift.expose-services="8080:http" \
      io.openshift.tags="builder,swipl" \
      # io.openshift.s2i.destination="/opt/app" \
      io.openshift.s2i.scripts-url="image:///opt/s2i"

RUN apt-get update -y && \ 
    apt-get install -y zip unzip ca-certificates \
                       procps curl iputils-ping iproute2 traceroute dnsutils \
                       less vim-tiny

RUN mkdir /opt/faasshell /opt/s2i

COPY ./s2i/bin/ /opt/s2i

RUN chown -R 1001:1001 /opt/faasshell

USER 1001

EXPOSE 8080

CMD ["/opt/s2i/usage"]
