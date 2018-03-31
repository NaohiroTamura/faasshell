# Demo

## Hello All FaaS

This demonstrates that one state machine calls hello function of each
FaaS provider in sequential or in parallel.

### Set Environment Variables

```sh
$ export AWS_ACCESS_KEY_ID=AAAAAAAAAAAAAAAAAAAA
$ export AWS_SECRET_ACCESS_KEY=BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB

$ export GOOGLE_APPLICATION_CREDENTIALS=/full/path/to_your_json_credential_file

$ export AZURE_HOSTKEY=CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
$ export AZURE_TENANT_ID=DDDDDDDD-DDDD-DDDD-DDDD-DDDDDDDDDDDD
$ export AZURE_CLIENT_ID=EEEEEEEE-EEEE-EEEE-EEEE-EEEEEEEEEEEE
$ export AZURE_CLIENT_SECRET=FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

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

$ make -e docker_image_prefix=nao16t run

$ FAASSHELL_APIHOST=https://127.0.0.1:8443

$ DEMO=ec29e90c-188d-11e8-bb72-00163ec1cd01:0b82fe63b6bd450519ade02c3cb8f77ee581f25a810db28f3910e6cdd9d041bf
```

### Hello All FaaS in Sequential

source file: [samples/common/asl/hello_all_seq.json](/samples/common/asl/hello_all_seq.json)

![samples/common/asl/hello_all_seq.json](/samples/common/graph/hello_all_seq.png)


```sh
$ export aws_region=us-east-2
$ export aws_account_id=410388484666
$ export gcp_location_id=us-central1
$ export gcp_project_id=glowing-program-196406
$ export azure_webapp_name=glowing-program-196406

$ envsubst < samples/common/asl/hello_all_seq.json | \
  curl -ksX PUT ${FAASSHELL_APIHOST}/statemachine/hello_all_seq.json?overwrite=true \
  -H 'Content-Type: application/json' -d @/dev/stdin \
  -u $DEMO | jq .dsl -
  "asl([task('HelloAWS',\"arn:aws:lambda:us-east-2:410388484666:function:hello\",[result_path('$.aws')]),task('HelloGCP',\"frn:gcp:lambda:us-central1:glowing-program-196406:cloudfunctions.net:hello\",[result_path('$.gcp')]),task('HelloAzure',\"frn:azure:lambda:japan-east:glowing-program-196406:azurewebsites.net:hello\",[result_path('$.azure')]),task('HelloBluemix',\"frn:wsk:functions:::function:hello\",[result_path('$.bluemix')])])"
```

```sh
$ curl -ksX POST ${FAASSHELL_APIHOST}/statemachine/hello_all_seq.json?blocking=true \
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
$ export aws_region=us-east-2
$ export aws_account_id=410388484666
$ export gcp_location_id=us-central1
$ export gcp_project_id=glowing-program-196406
$ export azure_webapp_name=glowing-program-196406

$ envsubst < samples/common/asl/hello_all_par.json | \
  curl -ksX PUT ${FAASSHELL_APIHOST}/statemachine/hello_all_par.json?overwrite=true \
  -H 'Content-Type: application/json' -d @/dev/stdin \
  -u $DEMO | jq .dsl -
  "asl([parallel('Parallel',branches([[task('HelloAWS',\"arn:aws:lambda:us-east-2:410388484666:function:hello\",[result_path('$.par.aws'),output_path('$.par')])],[task('HelloGCP',\"frn:gcp:lambda:us-central1:glowing-program-196406:cloudfunctions.net:hello\",[result_path('$.par.gcp'),output_path('$.par')])],[task('HelloAzure',\"frn:azure:lambda:japan-east:glowing-program-196406:azurewebsites.net:hello\",[result_path('$.par.azure'),output_path('$.par')])],[task('HelloBluemix',\"frn:wsk:functions:::function:hello\",[result_path('$.par.bluemix'),output_path('$.par')])]]),[]),pass('Final State',[])])"
```

```sh
$ curl -ksX POST ${FAASSHELL_APIHOST}/statemachine/hello_all_par.json?blocking=true \
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
