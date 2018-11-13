# Demo

## Activity Task

Function is short lived task, generally within five minutes.

On the other hand, activity is long running task hosted outside of FaaS.

### Start FaaS Shell in Docker

```sh
$ docker run -d -p 5984:5984 apache/couchdb

$ make -e docker_image_prefix=nao16t run

$ FAASSHELL_APIHOST=http://127.0.0.1:8080

$ DEMO=ec29e90c-188d-11e8-bb72-00163ec1cd01:0b82fe63b6bd450519ade02c3cb8f77ee581f25a810db28f3910e6cdd9d041bf
```

### Register state machine

```sh
$ curl -ksX PUT ${FAASSHELL_APIHOST}/statemachine/activity_task.json \
  -H 'Content-Type: application/json' -d @samples/common/asl/activity_task.json -u $DEMO
{
  "asl": {
    "Comment":"A Hello World example of the Amazon States Language using a Task state",
    "StartAt":"HelloWorld",
    "States": {
      "HelloWorld": {
        "End":true,
        "Resource":"frn::states:::activity:test",
        "Type":"Task"
      }
    }
  },
  "dsl":"fsm([task('HelloWorld',\"frn::states:::activity:test\",[])])",
  "name":"activity_task.json",
  "namespace":"demo",
  "output":"ok"
}
```

### Activity Task (Success case)

#### [StartExecution][1]

Start state machine in background.

```sh
$ curl -ksX POST ${FAASSHELL_APIHOST}/statemachine/activity_task.json?blocking=false \
  -H 'Content-Type: application/json' -d '{"input": {"name": "Activity"}}' -u $DEMO
{
  "asl": {
    "Comment":"A Hello World example of the Amazon States Language using a Task state",
    "StartAt":"HelloWorld",
    "States": {
      "HelloWorld": {
        "End":true,
        "Resource":"frn::states:::activity:test",
        "Type":"Task"
      }
    }
  },
  "dsl":"fsm([task('HelloWorld',\"frn::states:::activity:test\",[])])",
  "input": {"name":"Activity"},
  "name":"activity_task.json",
  "namespace":"demo",
  "output": {"execution_id":"dc7174c6-3647-11e8-b097-080027306bdf"}
}
```

#### [GetActivityTask][2]

Activity task gets taskToken and input parameter.

```sh
$ curl -ksLX GET ${FAASSHELL_APIHOST}/activity/frn::states:::activity:test -u $DEMO
{
  "input": {"name":"Activity"},
  "output":"ok",
  "taskToken":"e380661e-3647-11e8-82d9-080027306bdf"
}
```

#### [SendTaskHeartbeat][3]

Activity task heartbeats to the state machine.

```sh
$ curl -kiLX PATCH ${FAASSHELL_APIHOST}/activity/frn::states:::activity:test \
 -H 'Content-Type: application/json' -d '{"taskToken": "e380661e-3647-11e8-82d9-080027306bdf"}' \
 -u $DEMO
HTTP/1.1 200 OK
Date: Mon, 02 Apr 2018 07:32:02 GMT
Connection: Keep-Alive
Content-Type: application/json; charset=UTF-8
Content-Length: 2

{}
```

#### [SendTaskSuccess][4]

Activity task does some job, and sends the return value to the state machine.

```sh
$ curl -kiX POST ${FAASSHELL_APIHOST}/activity/frn::states:::activity:test \
  -H 'Content-Type: application/json' \
  -d '{"output": {"payload": "Hello, Activity!"}, "taskToken": "e380661e-3647-11e8-82d9-080027306bdf" }' \
  -u $DEMO
HTTP/1.1 200 OK
Date: Mon, 02 Apr 2018 07:32:28 GMT
Connection: Keep-Alive
Content-Type: application/json; charset=UTF-8
Content-Length: 2

{}
```

#### [DescribeExecution][6]

Get the state machine execution result.

```sh
$ curl -ksLX GET ${FAASSHELL_APIHOST}/executions/dc7174c6-3647-11e8-b097-080027306bdf -u $DEMO
{
  "end":1522654348.1769109,
  "execution_id":"dc7174c6-3647-11e8-b097-080027306bdf",
  "hostname":"vagrant-ubuntu-trusty-64",
  "namespace":"demo",
  "result": {"input": {"name":"Activity"}, "output": {"payload":"Hello, Activity!"}},
  "start":1522654290.380311,
  "statemachine":"activity_task.json"
}
```

### Activity Task (Failure case)

#### [StartExecution][1]

Start state machine in background.

```sh
$ curl -ksX POST ${FAASSHELL_APIHOST}/statemachine/activity_task.json?blocking=false \
  -H 'Content-Type: application/json' -d '{"input": {"name": "Activity"}}' \
  -u $DEMO | jq .output -
{
  "execution_id": "095ddd90-3651-11e8-ab4f-080027306bdf"
}
```

#### [GetActivityTask][2]

Activity task gets taskToken and input parameter.

```sh
$ curl -ksLX GET ${FAASSHELL_APIHOST}/activity/frn::states:::activity:test -u $DEMO
{
  "input": {"name":"Activity"},
  "output":"ok",
  "taskToken":"10e6b1f4-3651-11e8-88af-080027306bdf"
}
```

#### [SendTaskFailure][5]

Activity task got an error, and sends the error to the state machine.

```sh
$ curl -ksX POST ${FAASSHELL_APIHOST}/activity/frn::states:::activity:test \
  -H 'Content-Type: application/json' \
  -d '{"error": "not found", "cause":"db error", "taskToken": "10e6b1f4-3651-11e8-88af-080027306bdf" }' \
  -u $DEMO
{}
```

#### [DescribeExecution][6]

Get the state machine execution result.

```sh
$ curl -ksLX GET ${FAASSHELL_APIHOST}/executions/095ddd90-3651-11e8-ab4f-080027306bdf \
  -u $DEMO | jq .result -
{
  "output": {
      "error": "not found",
      "cause": "db error"
  },
  "input": {
      "name": "Activity"
  }
}
```

[1]: https://docs.aws.amazon.com/step-functions/latest/apireference/API_StartExecution.html "StartExecution"
[2]: https://docs.aws.amazon.com/step-functions/latest/apireference/API_GetActivityTask.html "GetActivityTask"
[3]: https://docs.aws.amazon.com/step-functions/latest/apireference/API_SendTaskHeartbeat.html "SendTaskHeartbeat"
[4]: https://docs.aws.amazon.com/step-functions/latest/apireference/API_SendTaskSuccess.html "SendTaskSuccess"
[5]: https://docs.aws.amazon.com/step-functions/latest/apireference/API_SendTaskFailure.html "SendTaskFailure"
[6]: https://docs.aws.amazon.com/step-functions/latest/apireference/API_DescribeExecution.html "DescribeExecution"
