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
# $ docker run -it --rm -v /home:/home -u $(id -u):$(id -g) -w $PWD \
#              -e EMACS=t -e INFERIOR=yes \
#              -e $(grep AUTH ~/.wskprops) -e $(grep APIHOST ~/.wskprops) \
#              -p 8080:8080 -v /tmp:/logs swipl:7.5.15 $@
#
FROM swipl:7.5.15

RUN apt-get update -y && \ 
    apt-get install -y procps curl iputils-ping iproute2 traceroute dnsutils \
                       less vim-tiny

ADD asl*.pl wsk*.pl /opt/cloudshell/

WORKDIR /opt/cloudshell

EXPOSE 8080

CMD ["swipl", "-q", "-l", "asl_svc.pl", "-g", "main", "-t", "halt"]
