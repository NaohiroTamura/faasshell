# Demo

## Hello All FaaS

This demonstrates that one state machine calls hello function of each
FaaS provider in sequential or in parallel.

### Set Environment Variables

```sh
$ export AWS_ACCESS_KEY_ID=AAAAAAAAAAAAAAAAAAAA
$ export AWS_SECRET_ACCESS_KEY=BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB

$ export AZURE_HOSTKEY=CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

$ export WSK_AUTH=DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD:EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
$ export WSK_APIHOST=FQDN_OR_IP
```

```sh
$ export HTTP_PROXY="http://id:pw@proxy.example.com:8080"
$ export HTTPS_PROXY="https://id:pw@proxy.example.com:8433"
$ export NO_PROXY="localhost,127.0.0.1,0.0.0.0,172.17.0.1"
```

### Start FaaS Shell in Docker

```sh
$ docker run -d -p 5984:5984 apache/couchdb

$ make -e docker_image_prefix=nao16t/ run

$ FAASSHELL_APIHOST=http://127.0.0.1:8080

$ DEMO=ec29e90c-188d-11e8-bb72-00163ec1cd01:0b82fe63b6bd450519ade02c3cb8f77ee581f25a810db28f3910e6cdd9d041bf
```

### Hello All FaaS in Sequential

source file: [samples/common/asl/hello_all_seq.json](/samples/common/asl/hello_all_seq.json)

![samples/common/asl/hello_all_seq.json](/samples/common/graph/hello_all_seq.png)


```sh
$ curl -sX PUT ${FAASSHELL_APIHOST}/statemachine/hello_all_seq.json?overwrite=true \
  -H 'Content-Type: application/json' -d @samples/common/asl/hello_all_seq.json \
  -u $DEMO | jq .dsl -
  "asl([task('HelloAWS',\"arn:aws:lambda:us-east-2:410388484666:function:hello\",[result_path('$.aws')]),task('HelloGCP',\"grn:gcp:lambda:us-central1:glowing-program-196406:cloudfunctions.net:hello\",[result_path('$.gcp')]),task('HelloAzure',\"mrn:azure:lambda:japan-east:glowing-program-196406:azurewebsites.net:hello\",[result_path('$.azure')]),task('HelloBluemix',\"wsk:hello\",[result_path('$.bluemix')])])"
```

```sh
$ curl -sX POST ${FAASSHELL_APIHOST}/statemachine/hello_all_seq.json?blocking=true \
  -H 'Content-Type: application/json' -d '{"input": {"name": "FaaS Shell"}}' \
  -u $DEMO  | jq .output -
  {
    "name": "FaaS Shell",
      "gcp": {
          "payload": "Hello, FaaS Shell!"
      },
      "bluemix": {
          "payload": "Hello, FaaS Shell!"
      },
      "azure": {
          "payload": "Hello, FaaS Shell!"
      },
      "aws": {
          "payload": "Hello, FaaS Shell!"
      }
  }
```

### Hello All FaaS in Parallel

source file: [samples/common/asl/hello_all_par.json](/samples/common/asl/hello_all_par.json)

![samples/common/asl/hello_all_par.json](/samples/common/graph/hello_all_par.png)

```sh
$ curl -sX PUT ${FAASSHELL_APIHOST}/statemachine/hello_all_par.json?overwrite=true \
  -H 'Content-Type: application/json' -d @samples/common/asl/hello_all_par.json \
  -u $DEMO | jq .dsl -
  "asl([parallel('Parallel',branches([[task('HelloAWS',\"arn:aws:lambda:us-east-2:410388484666:function:hello\",[result_path('$.par.aws'),output_path('$.par')])],[task('HelloGCP',\"grn:gcp:lambda:us-central1:glowing-program-196406:cloudfunctions.net:hello\",[result_path('$.par.gcp'),output_path('$.par')])],[task('HelloAzure',\"mrn:azure:lambda:japan-east:glowing-program-196406:azurewebsites.net:hello\",[result_path('$.par.azure'),output_path('$.par')])],[task('HelloBluemix',\"wsk:hello\",[result_path('$.par.bluemix'),output_path('$.par')])]]),[]),pass('Final State',[])])"
```

```sh
$ curl -sX POST ${FAASSHELL_APIHOST}/statemachine/hello_all_par.json?blocking=true \
  -H 'Content-Type: application/json' -d '{"input": {"name": "Parallel FaaS Shell"}}' \
  -u $DEMO  | jq .output -
[
    {
        "aws": {
            "payload": "Hello, Parallel FaaS Shell!"
        }
    },
    {
        "gcp": {
            "payload": "Hello, Parallel FaaS Shell!"
        }
    },
    {
        "azure": {
            "payload": "Hello, Parallel FaaS Shell!"
        }
    },
    {
        "bluemix": {
            "payload": "Hello, Parallel FaaS Shell!"
        }
    }
]
```
