# Demo

## IFTTT as FaaS

### Prepare Functions

* IFTTT "save_result" Applet

  Follow the steps in the document, [samples/ifttt/applet/save_result.md](./ifttt/applet/save_result.md).

* IBM Cloud Functions / Apache OpenWhisk "hello" Action

  ```sh
  $ wsk action create hello samples/wsk/actions/hello.js -i
  ```

### Set Environment Variables

```sh
$ export WSK_AUTH=XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX:YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY
$ export WSK_APIHOST=FQDN_OR_IP

$ export IFTTT_KEY=ZZZZZZZZZZZZZZZZZZZZZ
```

```sh
$ export HTTP_PROXY="http://id:pw@proxy.example.com:8080"
$ export HTTPS_PROXY="https://id:pw@proxy.example.com:8433"
$ export NO_PROXY="localhost,127.0.0.1,0.0.0.0,172.17.0.1"
```

### Start FaaS Shell in Docker

```sh
$ docker run -d -p 5984:5984 apache/couchdb

$ make -e docker_image_prefix=nao16t run

$ FAASSHELL_APIHOST=https://127.0.0.1:8443

$ DEMO=ec29e90c-188d-11e8-bb72-00163ec1cd01:0b82fe63b6bd450519ade02c3cb8f77ee581f25a810db28f3910e6cdd9d041bf
```

### Run IFTTT as FaaS

Notice that "UpdateArg" reconciles the parameter mismatch between "HelloWorld"
output, '{"payload": "Hello, IFTTT!"}', and "SaveResult" input,
'{value1:"Hello, IFTTT!"}' making use of "InputPath", "ResultPath", and
"OutputPath".

```sh
$ curl -ksX PUT ${FAASSHELL_APIHOST}/statemachine/save_result.json?overwrite=true \
  -H 'Content-Type: application/json' -d @samples/ifttt/asl/save_result.json -u $DEMO 
  {
    "asl": {
      "Comment":"A Hello World example of the Amazon States Language using a Task state",
      "StartAt":"HelloWorld",
      "States": {
        "HelloWorld": {
          "Next":"UpdateArg",
          "Resource":"frn:wsk:functions:::function:hello",
          "Type":"Task"
        },
        "UpdateArg": {
          "InputPath":"$.payload",
          "Next":"SaveResult",
          "OutputPath":"$.ifttt",
          "ResultPath":"$.ifttt.value1",
          "Type":"Pass"
        },
        "SaveResult": {
          "End":true,
          "Resource":"frn:ifttt:webhooks:::function:save_result",
          "Type":"Task"
        }
      }
    },
    "dsl":"asl([task('HelloWorld',\"frn:wsk:functions:::function:hello\",[]),pass('UpdateArg',[result_path('$.ifttt.value1'),input_path('$.payload'),output_path('$.ifttt')])$
  task('SaveResult',\"frn:ifttt:webhooks:::function:save_result\",[])])",
    "name":"save_result.json",
    "namespace":"demo",
    "output":"ok"
  }
```

```sh
$ curl -ksX POST ${FAASSHELL_APIHOST}/statemachine/save_result.json?blocking=true \
  -H 'Content-Type: application/json' -d '{"input": {"name": "IFTTT"}}' \
  -u $DEMO | jq .output -
  "Congratulations! You've fired the save_result event"
```

Open your Google sheets "IFTTT_Maker_Webhooks_Events", https://docs.google.com/spreadsheets/, and check if "Hello, IFTTT!" is save in a new row.
