#
# $Id$
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
