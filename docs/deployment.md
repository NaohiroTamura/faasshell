# Deployment

## Server certificate

The faasshell service runs in https if there are "etc/server/server-cert.pem" and "etc/server/server-key.pem". Otherwise it runs in http.

Self signed certificate is checked in the git repository".
You can print the contents as following.

```sh
$ openssl x509 -text -noout -in etc/server/server-cert.pem
```
In order to regenerate the self signed certificate, type the following command.

```sh
$ make cert
```

Otherwise follow the steps described in [Generating Self Signed Certificate](../etc).

## Docker

```sh
$ docker run -d -p 5984:5984 apache/couchdb

$ make -e docker_image_prefix=YOUR_PREFIX run

$ FAASSHELL_APIHOST=https://127.0.0.1:8443

$ DEMO=ec29e90c-188d-11e8-bb72-00163ec1cd01:0b82fe63b6bd450519ade02c3cb8f77ee581f25a810db28f3910e6cdd9d041bf

$ curl -ksLX GET ${FAASSHELL_APIHOST}/ -u $DEMO
{"version":"$Id rev.YYYY-MM-DD.COMMITID $"}
```

## Kubernetes

```sh
$ kubectl create namespace faasshell

$ kubectl -n faasshell run couchdb --image=apache/couchdb

$ kubectl -n faasshell expose deployment couchdb --port=5984
```

  > In case of proxy environment:
  >
  > ```sh
  > $ kubectl -n faasshell get service | grep couchdb | awk '{print $3}'
  > 10.101.62.31
  >
  > $ export NO_PROXY=$NO_PROXY,10.101.62.31
  > ```

```sh
$ make -e docker_image_prefix=YOUR_PREFIX deploy

$ kubectl -n faasshell describe service faasshell | grep https | grep NodePort| awk '{print $3}' | cut -d'/' -f1
30954

$ FAASSHELL_APIHOST=https://${cluster_address}:30954

$ DEMO=ec29e90c-188d-11e8-bb72-00163ec1cd01:0b82fe63b6bd450519ade02c3cb8f77ee581f25a810db28f3910e6cdd9d041bf

$ curl -ksLX GET ${FAASSHELL_APIHOST}/ -u $DEMO
{"version":"$Id rev.YYYY-MM-DD.COMMITID $"}
```

${cluster_address}  depends on clusters environment, it is opened at https://$(minikube ip):30954 in case of Minikube.

The port number is changed each service deployment, 30954 is just an example.
