# Getting started with OpenWhisk

## Prepare OpenWhisk Environment

Please setup OpenWhisk environment in either Docker or Kubernetes by
refereeing to the steps described at the following sites.

- [OpenWhisk](https://github.com/apache/incubator-openwhisk)
- [OpenWhisk Deployment for Kubernetes](https://github.com/apache/incubator-openwhisk-deploy-kube)

If the environment is ready, the echo action should work like below:
```sh
$ wsk action invoke /whisk.system/utils/echo -p message hello -r -i
 {
        "message": "hello"
 }
```

## Build Image

- Install [Source-To-Image (S2I)](https://github.com/openshift/source-to-image/releases)
- Build builder image
  ```sh
  ubuntu@trusty:~/faasshell[master]$ make build_image
  ```
- Build application image
  ```sh
  ubuntu@trusty:~/faasshell[master]$ make -e docker_image_prefix=my_prefix/ app_image
  ```

## Deploy Image

### In case of Docker
```sh
ubuntu@trusty:~/faasshell[master]$ make -e docker_image_prefix=my_prefix/ run

ubuntu@trusty:~/faasshell[master]$ docker ps | grep faasshell
4858b533d7c4  my_prefix/faasshell:latest  "/opt/s2i/run" 1 minutes ago  Up 9 minutes  0.0.0.0:8080->8080/tcp  suspicious_brown

```

Endpoint is opened at http://localhost:8080/

### In case of Kubernetes

docker_image_prefix has to be your Docker Hub image prefix.

```sh
ubuntu@trusty:~/faasshell[master]$ make -e docker_image_prefix=my_prefix/ deploy

ubuntu@trusty:~/faasshell[master]$ kubectl get pod -n openwhisk | grep faasshell
faasshell-4218281672-bf9xm    1/1       Running   0          3m

ubuntu@trusty:~/faasshell[master]$ kubectl get svc -n openwhisk faasshell
NAME        CLUSTER-IP       EXTERNAL-IP   PORT(S)          AGE
faasshell   172.21.208.173   <nodes>       8080:30954/TCP   2m
```

Endpoint is opened at http://$(minikube ip):30954 in case of Minikube.

## Execute Hello World Example

### faas interface

```sh
ubuntu@trusty:~/faasshell[master]$ wsk action create hello samples/actions/hello.js -i
ok: created action hello

ubuntu@trusty:~/faasshell[master]$ wsk action create helloPython samples/actions/helloPython.py -i
ok: created action helloPython

ubuntu@trusty:~/faasshell[master]$ wsk action create job samples/actions/job.py -i
ok: created action job
```
```sh
ubuntu@trusty:~/faasshell[master]$ . ~/.wskprops
```
```sh
ubuntu@trusty:~/faasshell[master]$ curl -sLX GET http://172.17.0.1:8080/faas/ -u $AUTH
[
  {
    "annotations": [ {"key":"exec", "value":"python:2"} ],
    "name":"helloPython",
    "namespace":"guest",
    "publish":false,
    "version":"0.0.1"
  },
  {
    "annotations": [ {"key":"exec", "value":"python:2"} ],
    "name":"job",
    "namespace":"guest",
    "publish":false,
    "version":"0.0.1"
  },
  {
    "annotations": [ {"key":"exec", "value":"nodejs:6"} ],
    "name":"hello",
    "namespace":"guest",
    "publish":false,
    "version":"0.0.1"
  }
]
```

### statemachine interface
```sh
ubuntu@trusty:~/faasshell[master]$ curl -sX PUT http://172.17.0.1:8080/statemachine/hello_world_task_asl.json \
-H 'Content-Type: application/json' -d @samples/asl/hello_world_task_asl.json -u $AUTH
{
  "asl": {
    "Comment":"A Hello World example of the Amazon States Language using an AWS Lambda function",
    "StartAt":"HelloWorld",
    "States": {
      "HelloWorld": {
        "End":true,
        "HeartbeatSeconds":60,
        "Resource":"hello",
        "TimeoutSeconds":300,
        "Type":"Task"
      }
    }
  },
  "dsl":"asl([task('HelloWorld',\"hello\",[timeout_seconds(300),heartbeat_seconds(60)])])",
  "input": {"name":"FaaS Shell"},
  "name":"hello_world_task_asl.json",
  "output":"ok"
}
```
```sh
ubuntu@trusty:~/faasshell[master]$ curl -sLX GET http://172.17.0.1:8080/statemachine/hello_world_task_asl.json \
-u $AUTH
{
  "asl": {
    "Comment":"A Hello World example of the Amazon States Language using an AWS Lambda function",
    "StartAt":"HelloWorld",
    "States": {
      "HelloWorld": {
        "End":true,
        "HeartbeatSeconds":60,
        "Resource":"hello",
        "TimeoutSeconds":300,
        "Type":"Task"
      }
    }
  },
  "input": {"name":"FaaS Shell"},
  "name":"hello_world_task_asl.json"
}
```
```sh
ubuntu@trusty:~/faasshell[master]$ curl -sX POST http://172.17.0.1:8080/statemachine/hello_world_task_asl.json \
-u $AUTH
{
  "asl": {
    "Comment":"A Hello World example of the Amazon States Language using an AWS Lambda function",
    "StartAt":"HelloWorld",
    "States": {
      "HelloWorld": {
        "End":true,
        "HeartbeatSeconds":60,
        "Resource":"hello",
        "TimeoutSeconds":300,
        "Type":"Task"
      }
    }
  },
  "input": {"name":"FaaS Shell"},
  "name":"hello_world_task_asl.json",
  "output": {"name":"FaaS Shell", "payload":"Hello, FaaS Shell!"}
}
```
```sh
ubuntu@trusty:~/faasshell[master]$ curl -sX POST http://172.17.0.1:8080/statemachine/hello_world_task_asl.json \
-H 'Content-Type: application/json' -d '{"input": {"name": "Curl"}}' -u $AUTH
{
  "asl": {
    "Comment":"A Hello World example of the Amazon States Language using an AWS Lambda function",
    "StartAt":"HelloWorld",
    "States": {
      "HelloWorld": {
        "End":true,
        "HeartbeatSeconds":60,
        "Resource":"hello",
        "TimeoutSeconds":300,
        "Type":"Task"
      }
    }
  },
  "input": {"name":"FaaS Shell"},
  "name":"hello_world_task_asl.json",
  "output": {"name":"Curl", "payload":"Hello, Curl!"}
}
```
```sh
ubuntu@trusty:~/faasshell[master]$ curl -sX PATCH http://172.17.0.1:8080/statemachine/hello_world_task_asl.json \
-u $AUTH
digraph graph_name {
     "Start" -> "HelloWorld" ;
     "HelloWorld" -> "End" ;
}
```
```sh
ubuntu@trusty:~/faasshell[master]$ curl -sX DELETE http://172.17.0.1:8080/statemachine/hello_world_task_asl.json \
-u $AUTH
{"output":"ok"}

```

### shell interface
```sh
ubuntu@trusty:~/faasshell[master]$ curl -sX PUT http://172.17.0.1:8080/shell/hello_world_task.dsl \
-H 'Content-Type: text/plain' -d @samples/dsl/hello_world_task.dsl -u $AUTH
{
  "dsl":"asl([task('HelloWorld',\"hello\",[timeout_seconds(5),heartbeat_seconds(10)])]).",
  "output":"ok"
}
```
```sh
ubuntu@trusty:~/faasshell[master]$ curl -sLX GET http://172.17.0.1:8080/shell/hello_world_task.dsl \
-u $AUTH
{
  "dsl":"asl([task('HelloWorld',\"hello\",[timeout_seconds(5),heartbeat_seconds(10)])])",
  "output":"ok"
}
```
```sh
ubuntu@trusty:~/faasshell[master]$ curl -sX POST  http://172.17.0.1:8080/shell/hello_world_task.dsl \
-H 'Content-Type: application/json' -d '{"name":"Shell"}' -u $AUTH
{"output": {"name":"Shell", "payload":"Hello, Shell!"}}
```
```sh
ubuntu@trusty:~/faasshell[master]$ curl -sX DELETE http://172.17.0.1:8080/shell/hello_world_task.dsl \
-u $AUTH
{"output":"ok"}
```
