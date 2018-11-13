# Demo

## Event State

Event State is an extension of [Amazon State Language][1]. This demonstrates how an event triggers function in Event State.

[1]: https://states-language.net/spec.html "Amazon State Language"

### Start FaaS Shell in Docker

```sh
$ docker run -d -p 5984:5984 apache/couchdb

$ make -e docker_image_prefix=nao16t run

$ FAASSHELL_APIHOST=http://127.0.0.1:8080

$ DEMO=ec29e90c-188d-11e8-bb72-00163ec1cd01:0b82fe63b6bd450519ade02c3cb8f77ee581f25a810db28f3910e6cdd9d041bf
```

### Register state machine

```sh
$ curl -ksX PUT ${FAASSHELL_APIHOST}/statemachine/event_state.json?overwrite=true \
  -H 'Content-Type: application/json' -d @samples/common/asl/event_state.json -u $DEMO
{
  "asl": {
    "Comment":"A Hello World example of the Amazon States Language using an AWS Lambda function",
    "StartAt":"HelloWorld",
    "States": {
      "HelloWorld": {
        "End":true,
        "Resource":"frn::states:::event:test",
        "Type":"Event"
      }
    }
  },
  "dsl":"fsm([event('HelloWorld',\"frn::states:::event:test\",[])])",
  "name":"event_state.json",
  "namespace":"demo",
  "output":"ok"
}
```

### Start statemachine

Start statemacine in background.

Notice that the "HelloWorld" state has the event type resource as "Resource":"frn::**states**:::**event**:test".

```sh
$ curl -ksX POST ${FAASSHELL_APIHOST}/statemachine/event_state.json?blocking=false \
  -H 'Content-Type: application/json' -d '{"input": {"name": "Event"}}' -u $DEMO
{
  "asl": {
    "Comment":"A Hello World example of the Amazon States Language using an AWS Lambda function",
    "StartAt":"HelloWorld",
    "States": {
      "HelloWorld": {
        "End":true,
        "Resource":"frn::states:::event:test",
        "Type":"Event"
      }
    }
  },
  "dsl":"fsm([event('HelloWorld',\"frn::states:::event:test\",[])])",
  "input": {"name":"Event"},
  "name":"event_state.json",
  "namespace":"demo",
  "output": {"execution_id":"043d464c-4de5-11e8-9392-00163e8c34b9"}
}
```

### Send evnet

Send the event "frn::states:::event:test" to the statemachine with the parameter '{"action":"frn:wsk:**functions**:::**function**:hello"}'.

Then the statemachine invokes the function "frn:wsk:**functions**:::**function**:hello" with the parameter '{"name": "Event"}'.

```sh
$ curl -ksX POST ${FAASSHELL_APIHOST}/trigger/frn::states:::event:test \
  -H 'Content-Type: application/json' -d '{"action":"frn:wsk:functions:::function:hello"}' -u $DEMO
{}
```

### Get result

```sh
$ curl -ksX GET ${FAASSHELL_APIHOST}/executions/043d464c-4de5-11e8-9392-00163e8c34b9 -u $DEMO
{
  "end":1525250736.5557952,
  "execution_id":"043d464c-4de5-11e8-9392-00163e8c34b9",
  "hostname":"bluemix.lxd",
  "namespace":"demo",
  "result": {"input": {"name":"Event"}, "output": {"payload":"Hello, Event!"}},
  "start":1525250664.884613,
  "statemachine":"event_state.json"
}
```
