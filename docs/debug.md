# Debug

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
