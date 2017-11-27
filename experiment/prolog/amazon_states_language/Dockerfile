#
# $Id$
#
FROM swipl:7.5.15

RUN apt-get update -y && \ 
    apt-get install -y procps curl iputils-ping iproute2 traceroute dnsutils \
                       less vim-tiny

ADD asl*.pl wsk*.pl /opt/cloudshell/

WORKDIR /opt/cloudshell

EXPOSE 8080

CMD ["swipl", "-q", "-l", "asl_svc.pl", "-g", "main", "-t", "halt"]
