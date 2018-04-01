# Debug

## CouchDB

In order to browse the Faas Shell DB, type the following command.

```sh
$ firefox http://localhost:5984/_utils/
```

For more details, see [the couchdb offical document][1].

[1]: https://couchdb.apache.org/fauxton-visual-guide/index.html "Fauxton Visual Guide"

## Switching to HTTP

The faasshell service runs in https in default if there are "etc/server/server-cert.pem" and "etc/server/server-key.pem".

You can switch to plain http for debugging by restarting the faasshell service after removing the two files, "etc/server/server-cert.pem" and "etc/server/server-key.pem".

## Logging

Currently faasshell logs to "/logs/httpd.log" so that stdout can be used for
debugging during development.

Later faasshell will log to stdout and stderr for Docker and Kubernetes.

## Docker

"bin/swipl" script starts docker with '-v /tmp:/logs' option.

```sh
$ tail -f /tmp/httpd.log
```

## Kubernetes

```sh
$ kubectl -n faasshell get pods | grep faasshell | awk '{print $1}'
faasshell-5f656d447d-pc7cd

$ kubectl -n faasshell exec -it faasshell-5f656d447d-pc7cd /bin/bash

I have no name!@faasshell-5f656d447d-pc7cd:/$ tail -f /logs/httpd.log
```
