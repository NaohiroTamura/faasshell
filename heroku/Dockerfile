# Copyright 2018 FUJITSU LIMITED
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

FROM nao16t/faasshell

USER 0:0

WORKDIR /opt/faasshell

RUN rm /bin/sh && ln -s /bin/bash /bin/sh && \
    mkdir /logs && chmod 0777 /logs && \
    ln -s /dev/stderr /logs/httpd.log

CMD if [ -e etc/server ]; then rm -rf etc/server ; fi && \
    FAASSHELL_SVC_PORT=$PORT swipl -q -l src/faasshell_svc.pl -g main -t halt
