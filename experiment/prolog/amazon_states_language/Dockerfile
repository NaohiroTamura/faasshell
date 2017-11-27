#
# $Id$
#
FROM swipl:7.5.15

ADD asl*.pl wsk*.pl /opt/cloudshell/

WORKDIR /opt/cloudshell

EXPOSE 8080

CMD ["swipl", "-q", "-l", "asl_svc.pl", "-g", "main", "-t", "halt"]
