# [FaaS Shell](https://naohirotamura.github.io/faasshell)

[![CircleCI](https://circleci.com/gh/NaohiroTamura/faasshell.svg?style=svg&circle-token=54edddbcc0d69eb03c0fd341258c80a6acec4088)](https://circleci.com/gh/NaohiroTamura/faasshell)
[![License](https://img.shields.io/badge/license-Apache--2.0-blue.svg)](http://www.apache.org/licenses/LICENSE-2.0)

*FaaS Shell* is a shell for Serverless Function Workflow, which is a
common abstraction layer on top of FaaS Infrastructures to execute
multiple workflow languages across multiple clouds.

*Faas Shell* compiles workflow language and generate workflow DSL.
Then FaaS Shell interpreter executes the workflow DSL.
This architecture aims at mitigating potential vendor lock-in of your
application.

*FaaS Shell* currently supports one workflow language as the first
step, and five FaaS infrastructures.

* Workflow Language
  * [Amazon State Language][1]
  
* FaaS Infrastructure
  * [AWS Lambda][2]
  * [Google Cloud Functions][3]
  * [Microsoft Azure Functions][4]
  * [IBM Cloud Functions][5] / [Apache OpenWhisk][6]

You can run your [Amazon State Language][1] workflow code created in
[AWS Step Functions][7] in anywhere, and your workflow can call not
only [AWS Lambda][1] but also functions in other FaaS Infrastructures.

[1]: https://states-language.net/spec.html "Amazon State Language"
[2]: https://aws.amazon.com/lambda/ "AWS Lambda"
[3]: https://cloud.google.com/functions/ "Google Cloud Functions"
[4]: https://azure.microsoft.com/en-us/services/functions/ "Microsoft Azure Functions"
[5]: https://www.ibm.com/cloud/functions "IBM Cloud Functions"
[6]: https://openwhisk.apache.org/ "Apache OpenWhisk"
[7]: https://aws.amazon.com/step-functions/ "AWS Step Functions"

## Quick Start

- [Create a Lambda State Machine][8] by following AWS Step Functions tutorial.
  And save the statemachine as "hello_world_task.json".

[8]: https://docs.aws.amazon.com/step-functions/latest/dg/tutorial-creating-lambda-state-machine.html "Create a Lambda State Machine"

- Set your AWS Access Keys to environment variables.

  ```sh
  $ export AWS_ACCESS_KEY_ID=AAAAAAAAAAAAAAAAAAAA
  $ export AWS_SECRET_ACCESS_KEY=BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
  ```
  
  
- Start Faas Shell

  ```sh
  $ docker run -d -p 5984:5984 apache/couchdb

  $ docker run -d --net=host -v /tmp:/logs \
	           -e AWS_ACCESS_KEY_ID=$AWS_ACCESS_KEY_ID \
	           -e AWS_SECRET_ACCESS_KEY=$AWS_SECRET_ACCESS_KEY \
               nao16t/faasshell
  ```

  - Set the proxy environment variables if you are in private network.

    ```sh
    $ export HTTP_PROXY="http://id:pw@proxy.example.com:8080"
    $ export HTTPS_PROXY="https://id:pw@proxy.example.com:8433"
    $ export NO_PROXY="localhost,127.0.0.1,0.0.0.0,172.17.0.1"

    $ docker run -d --net=host -v /tmp:/logs \
                 -e AWS_ACCESS_KEY_ID=$AWS_ACCESS_KEY_ID \
                 -e AWS_SECRET_ACCESS_KEY=$AWS_SECRET_ACCESS_KEY \
                 -e HTTP_PROXY=$HTTP_PROXY \
                 -e HTTPS_PROXY=$HTTPS_PROXY \
                 -e NO_PROXY=$NO_PROXY \
                 nao16t/faasshell
    ```

- Execute the state machine

  ```sh
  $ DEMO=ec29e90c-188d-11e8-bb72-00163ec1cd01:0b82fe63b6bd450519ade02c3cb8f77ee581f25a810db28f3910e6cdd9d041bf
  
  $ FAASSHELL_APIHOST=http://127.0.0.1:8080
  
  $ curl -sX PUT ${FAASSHELL_APIHOST}/statemachine/hello_world_task.json \
  -H 'Content-Type: application/json' -d @hello_world_task.json -u $DEMO
  {
    "asl": {
      "Comment":"A Hello World example of the Amazon States Language using a Task state",
      "StartAt":"HelloWorld",
      "States": {
        "HelloWorld": {
          "End":true,
          "Resource":"arn:aws:lambda:us-east-2:410388484666:function:hello",
          "Type":"Task"
        }
      }
    },
    "dsl":"asl([task('HelloWorld',\"arn:aws:lambda:us-east-2:410388484666:function:hello\",[])])",
    "name":"hello_world_task.json",
    "namespace":"demo",
    "output":"ok"
  }

  $ curl -sX POST ${FAASSHELL_APIHOST}/statemachine/hello_world_task.json?blocking=true \
  -H 'Content-Type: application/json' -d '{"input": {"name": "Curl"}}' -u $DEMO
   {
    "asl": {
      "Comment":"A Hello World example of the Amazon States Language using a Task state",
      "StartAt":"HelloWorld",
      "States": {
        "HelloWorld": {
          "End":true,
          "Resource":"arn:aws:lambda:us-east-2:410388484666:function:hello",
          "Type":"Task"
        }
      }
    },
    "dsl":"asl([task('HelloWorld',\"arn:aws:lambda:us-east-2:410388484666:function:hello\",[])])",
    "input": {"name":"Curl"},
    "name":"hello_world_task.json",
    "namespace":"demo",
    "output": {"payload":"Hello, Curl!"}
  }
  ```

## [Getting Started](samples/)

[Getting started](samples/) shows how to build and deploy, how to use
REST API, and how to work with other FaaS.

Samples directory contains the following State Machine Template.

- Hello world (pass state)
- Hello world (task state)
- Wait state
- Retry failure
- Parallel
- Catch failure
- Choice state
- Job Status Poller
- Task Timer
