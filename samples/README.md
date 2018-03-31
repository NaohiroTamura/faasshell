# Getting Started

## Prepare FaaS Environment

Please create at least one FaaS account among the following four
provides. All of FaaS offers free plan.

- [AWS Lambda][1]
- [Google Cloud Functions][2]
- [Microsoft Azure Functions][3]
- [IBM Cloud Functions][4]

[1]: https://aws.amazon.com/lambda/ "AWS Lambda"
[2]: https://cloud.google.com/functions/ "Google Cloud Functions"
[3]: https://azure.microsoft.com/en-us/services/functions/ "Microsoft Azure Functions"
[4]: https://www.ibm.com/cloud/functions "IBM Cloud Functions"

Otherwise setup Apache OpenWhisk environment by yourself in either
Docker or Kubernetes by refereeing to the steps described at the
following sites.

- [Apache OpenWhisk](https://github.com/apache/incubator-openwhisk)
- [OpenWhisk Deployment for Kubernetes](https://github.com/apache/incubator-openwhisk-deploy-kube)


## Set Environment Variables

- AWS Lambda

  ```sh
  $ export AWS_ACCESS_KEY_ID=AAAAAAAAAAAAAAAAAAAA
  $ export AWS_SECRET_ACCESS_KEY=BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
  ```

- Google Cloud Functions

  ```sh
  $ export GOOGLE_APPLICATION_CREDENTIALS=/full/path/to_your_json_credential_file
  ```

- Microsoft Azure Functions

  ```sh
  $ export AZURE_HOSTKEY=CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  $ export AZURE_TENANT_ID=DDDDDDDD-DDDD-DDDD-DDDD-DDDDDDDDDDDD
  $ export AZURE_CLIENT_ID=EEEEEEEE-EEEE-EEEE-EEEE-EEEEEEEEEEEE
  $ export AZURE_CLIENT_SECRET=FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
  ```

- IBM Cloud Functions / Apache OpenWhisk

  ```sh
  $ export WSK_AUTH=GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG:HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH
  $ export WSK_APIHOST=FQDN_OR_IP
  ```

  WSK_AUTH and WSK_APIHOST should be set the values of AUTH and
  APIHOST in ~/.wskprops, so you can type the commands below in short.

  ```sh
  $ export WSK_$(grep AUTH ~/.wskprops)
  $ export WSK_$(grep APIHOST ~/.wskprops)
  ```

- For those who are behind Proxy Server

  DO NOT forget to set docker bridge or kubernetes endpoint into
  NO_PROXY.

  ```sh
  $ export HTTP_PROXY="http://id:pw@proxy.example.com:8080"
  $ export HTTPS_PROXY="https://id:pw@proxy.example.com:8433"
  $ export NO_PROXY="localhost,127.0.0.1,0.0.0.0,172.17.0.1,kubernetes_endpoint"
  ```

## Build Image

- Install [Source-To-Image (S2I)](https://github.com/openshift/source-to-image/releases)
- Build builder image
  ```sh
  ubuntu@trusty:~/faasshell[master]$ make build_image
  ```
- Build application image
  ```sh
  ubuntu@trusty:~/faasshell[master]$ make -e docker_image_prefix=YOUR_PREFIX app_image
  ```

## Deploy Image

### In case of Docker
```sh
ubuntu@trusty:~/faasshell[master]$ docker run -d -p 5984:5984 apache/couchdb

ubuntu@trusty:~/faasshell[master]$ make -e docker_image_prefix=YOUR_PREFIX run

ubuntu@trusty:~/faasshell[master]$ FAASSHELL_APIHOST=https://127.0.0.1:8443
```

### In case of Kubernetes

docker_image_prefix has to be your Docker Hub image prefix.

```sh
ubuntu@trusty:~/faasshell[master]$ kubectl create namespace faasshell

ubuntu@trusty:~/faasshell[master]$ kubectl -n faasshell run couchdb --image=apache/couchdb

ubuntu@trusty:~/faasshell[master]$ kubectl -n faasshell expose deployment couchdb --port=5984
```

  > In case of proxy environment:
  >
  > ```sh
  > ubuntu@trusty:~/faasshell[master]$ kubectl -n faasshell get service | grep couchdb | awk '{print $3}'
  > 10.101.62.31
  >
  > ubuntu@trusty:~/faasshell[master]$ export NO_PROXY=$NO_PROXY,10.101.62.31
  > ```

```sh
ubuntu@trusty:~/faasshell[master]$ make -e docker_image_prefix=YOUR_PREFIX deploy

ubuntu@trusty:~/faasshell[master]$ kubectl -n faasshell describe service faasshell | grep https | grep NodePort| awk '{print $3}' | cut -d'/' -f1
30954

ubuntu@trusty:~/faasshell[master]$ FAASSHELL_APIHOST=https://${cluster_address}:30954
```

${cluster_address}  depends on clusters environment, it is opened at https://$(minikube ip):30954 in case of Minikube.

The port number is changed each service deployment, 30954 is just an example.

## Execute Hello World Example

### Set DEMO key

```sh
ubuntu@trusty:~/faasshell[master]$ DEMO=ec29e90c-188d-11e8-bb72-00163ec1cd01:0b82fe63b6bd450519ade02c3cb8f77ee581f25a810db28f3910e6cdd9d041bf
```

### Add hello function

- AWS Lambda
  ```sh
  ubuntu@trusty:~/faasshell[master]$ (cd samples/aws/lambda; zip hello.zip hello.js)
   
  ubuntu@trusty:~/faasshell[master]$ aws lambda create-function --region us-east-2 --function-name hello \ 
    --zip-file fileb://samples/aws/lambda/hello.zip \
    --role arn:aws:iam::your_id:role/your_role \
    --handler hello.handler \
    --runtime nodejs6.10

  ```

- Google Cloud Functions
  ```sh
  ubuntu@trusty:~/faasshell[master]$ gcloud beta functions deploy hello --trigger-http \
    --source=samples/gcp/functions/hello --entry-point=helloWorld
  ```

- Microsoft Azure Functions
  ```sh
  ubuntu@trusty:~/faasshell[master]$ (cd samples/azure/functions; zip -r hello.zip .)
  
  ubuntu@trusty:~/faasshell[master]$ az functionapp deployment source config-zip \
    -g yourResourceGroup -n yourFunctionApp --src samples/azure/functions/hello.zip
  ```

- IBM Cloud Functions / Apache OpenWhisk
  ```sh
  ubuntu@trusty:~/faasshell[master]$ wsk action create hello samples/wsk/actions/hello.js -i
  ok: created action hello
  ```

### faas interface

```sh
ubuntu@trusty:~/faasshell[master]$ curl -ksLX GET ${FAASSHELL_APIHOST}/faas/ -u $DEMO
[
  {
    "annotations": [ {"key":"exec", "value":"nodejs:6"} ],
    "name":"hello",
    "namespace":"demo",
    "publish":false,
    "version":"0.0.1"
  }
]
```

### statemachine interface

- AWS Lambda

  ```sh
  ubuntu@trusty:~/faasshell[master]$ export aws_region=XX-YY-Z
  ubuntu@trusty:~/faasshell[master]$ export aws_account_id=000000000000
              
  ubuntu@trusty:~/faasshell[master]$ envsubst < samples/aws/asl/hello_world_task.json | \
  curl -ksX PUT ${FAASSHELL_APIHOST}/statemachine/hello_world_task.json?overwrite=true \
  -H 'Content-Type: application/json' -d @/dev/stdin -u $DEMO
  ```
- Google Cloud Functions

  ```sh
  ubuntu@trusty:~/faasshell[master]$ export gcp_location_id=XX-YYYYYYYY
  ubuntu@trusty:~/faasshell[master]$ export gcp_project_id=ZZZZZZZ
              
  ubuntu@trusty:~/faasshell[master]$ envsubst < samples/gcp/asl/hello_world_task.json | \
  curl -ksX PUT ${FAASSHELL_APIHOST}/statemachine/hello_world_task.json?overwrite=true \
  -H 'Content-Type: application/json' -d @/dev/stdin -u $DEMO
  ```

- Microsoft Azure Functions

  ```sh
  ubuntu@trusty:~/faasshell[master]$ export azure_webapp_name=ZZZZZZZ

  ubuntu@trusty:~/faasshell[master]$ envsubst < samples/azure/asl/hello_world_task.json | \
  curl -ksX PUT ${FAASSHELL_APIHOST}/statemachine/hello_world_task.json?overwrite=true \
  -H 'Content-Type: application/json' -d @/dev/stdin -u $DEMO
  ```

- IBM Cloud Functions / Apache OpenWhisk

  ```sh
  ubuntu@trusty:~/faasshell[master]$ curl -ksX PUT ${FAASSHELL_APIHOST}/statemachine/hello_world_task.json?overwrite=true \
  -H 'Content-Type: application/json' -d @samples/wsk/asl/hello_world_task.json -u $DEMO
  {
    "asl": {
      "Comment":"A Hello World example of the Amazon States Language using an AWS Lambda function",
      "StartAt":"HelloWorld",
      "States": {
        "HelloWorld": {
          "End":true,
          "Resource":"frn:wsk:functions:::function:hello",
          "TimeoutSeconds":5,
          "Type":"Task"
        }
      }
    },
    "dsl":"asl([task('HelloWorld',\"hello\",[timeout_seconds(5)])])",
    "name":"hello_world_task.json",
    "namespace":"demo",
    "output":"ok"
  }
  ```

```sh
ubuntu@trusty:~/faasshell[master]$ curl -ksLX GET ${FAASSHELL_APIHOST}/statemachine/hello_world_task.json \
-u $DEMO
{
  "asl": {
    "Comment":"A Hello World example of the Amazon States Language using an AWS Lambda function",
    "StartAt":"HelloWorld",
    "States": {
      "HelloWorld": {
        "End":true,
        "Resource":"frn:wsk:functions:::function:hello",
        "TimeoutSeconds":5,
        "Type":"Task"
      }
    }
  },
  "dsl":"asl([task('HelloWorld',\"frn:wsk:functions:::function:hello\",[timeout_seconds(5)])])",
  "name":"hello_world_task.json",
  "namespace":"demo",
  "output":"ok"
}
```
```sh
ubuntu@trusty:~/faasshell[master]$ curl -ksX POST ${FAASSHELL_APIHOST}/statemachine/hello_world_task.json?blocking=true \
-u $DEMO
{
  "asl": {
    "Comment":"A Hello World example of the Amazon States Language using an AWS Lambda function",
    "StartAt":"HelloWorld",
    "States": {
      "HelloWorld": {
        "End":true,
        "Resource":"frn:wsk:functions:::function:hello",
        "TimeoutSeconds":5,
        "Type":"Task"
      }
    }
  },
  "dsl":"asl([task('HelloWorld',\"frn:wsk:functions:::function:hello\",[timeout_seconds(5)])])",
  "input": {},
  "name":"hello_world_task.json",
  "namespace":"demo",
  "output": {"payload":"Hello, World!"}
}
```
```sh
ubuntu@trusty:~/faasshell[master]$ curl -ksX POST ${FAASSHELL_APIHOST}/statemachine/hello_world_task.json?blocking=true \
-H 'Content-Type: application/json' -d '{"input": {"name": "Curl"}}' -u $DEMO
{
  "asl": {
    "Comment":"A Hello World example of the Amazon States Language using an AWS Lambda function",
    "StartAt":"HelloWorld",
    "States": {
      "HelloWorld": {
        "End":true,
        "Resource":"frn:wsk:functions:::function:hello",
        "TimeoutSeconds":5,
        "Type":"Task"
      }
    }
  },
  "dsl":"asl([task('HelloWorld',\"frn:wsk:functions:::function:hello\",[timeout_seconds(5)])])",
  "input": {"name":"Curl"},
  "name":"hello_world_task.json",
  "namespace":"demo",
  "output": {"payload":"Hello, Curl!"}
}
```
```sh
ubuntu@trusty:~/faasshell[master]$ curl -ksX PATCH ${FAASSHELL_APIHOST}/statemachine/hello_world_task.json \
-u $DEMO
digraph graph_name {
     "Start" -> "HelloWorld" ;
     "HelloWorld" -> "End" ;
}
```
```sh
ubuntu@trusty:~/faasshell[master]$ curl -ksX DELETE ${FAASSHELL_APIHOST}/statemachine/hello_world_task.json \
-u $DEMO
{"output":"ok"}

```

### shell interface

- AWS Lambda

  ```sh
  ubuntu@trusty:~/faasshell[master]$ export aws_region=XX-YY-Z
  ubuntu@trusty:~/faasshell[master]$ export aws_account_id=000000000000
              
  ubuntu@trusty:~/faasshell[master]$ envsubst < samples/aws/dsl/hello_world_task.dsl | \
  curl -ksX PUT ${FAASSHELL_APIHOST}/shell/hello_world_task.dsl?overwrite=true \
  -H 'Content-Type: text/plain' -d @/dev/stdin -u $DEMO
  ```

- Google Cloud Functions

  ```sh
  ubuntu@trusty:~/faasshell[master]$ export gcp_location_id=XX-YYYYYYYY
  ubuntu@trusty:~/faasshell[master]$ export gcp_project_id=ZZZZZZZ
              
  ubuntu@trusty:~/faasshell[master]$ envsubst < samples/gcp/dsl/hello_world_task.dsl | \
  curl -ksX PUT ${FAASSHELL_APIHOST}/shell/hello_world_task.dsl?overwrite=true \
  -H 'Content-Type: text/plain' -d @/dev/stdin -u $DEMO
  ```

- Microsoft Azure Functions

  ```sh
  ubuntu@trusty:~/faasshell[master]$ export azure_webapp_name=ZZZZZZZ

  ubuntu@trusty:~/faasshell[master]$ envsubst < samples/azure/dsl/hello_world_task.dsl | \
  curl -ksX PUT ${FAASSHELL_APIHOST}/shell/hello_world_task.dsl?overwrite=true \
  -H 'Content-Type: text/plain' -d @/dev/stdin -u $DEMO
  ```

- IBM Cloud Functions / Apache OpenWhisk

  ```sh
  ubuntu@trusty:~/faasshell[master]$ curl -ksX PUT ${FAASSHELL_APIHOST}/shell/hello_world_task.dsl?overwrite=true \
  -H 'Content-Type: text/plain' -d @samples/wsk/dsl/hello_world_task.dsl -u $DEMO
  {
      "dsl":"asl([task('HelloWorld',\"frn:wsk:functions:::function:hello\",[timeout_seconds(5)])]).",
      "name":"hello_world_task.dsl",
      "namespace":"demo",
      "output":"ok"
  }
  ```

```sh
ubuntu@trusty:~/faasshell[master]$ curl -ksLX GET ${FAASSHELL_APIHOST}/shell/hello_world_task.dsl \
-u $DEMO
{
  "dsl":"asl([task('HelloWorld',\"frn:wsk:functions:::function:hello\",[timeout_seconds(5)])])",
  "name":"hello_world_task.dsl",
  "namespace":"demo",
  "output":"ok"
}
```
```sh
ubuntu@trusty:~/faasshell[master]$ curl -ksX POST  ${FAASSHELL_APIHOST}/shell/hello_world_task.dsl?blocking=true \
-H 'Content-Type: application/json' -d '{"input": {"name":"Shell"}}' -u $DEMO
{
  "dsl":"asl([task('HelloWorld',\"frn:wsk:functions:::function:hello\",[timeout_seconds(5),heartbeat_seconds(10)])]).",
  "input": {"name":"Shell"},
  "name":"hello_world_task.dsl",
  "namespace":"demo",
  "output": {"payload":"Hello, Shell!"}
}
```
```sh
ubuntu@trusty:~/faasshell[master]$ curl -ksX DELETE ${FAASSHELL_APIHOST}/shell/hello_world_task.dsl \
-u $DEMO
{"output":"ok"}
```
