# Translation Text Demo

## Register statemachine
- AWS Lambda

  ```sh
  ubuntu@trusty:~/faasshell[master]$ envsubst < samples/aws/asl/translation.json | \
  curl -ksX PUT ${FAASSHELL_APIHOST}/statemachine/translation.json?overwrite=true \
  -H 'Content-Type: application/json' -d @/dev/stdin -u $DEMO
  ```

- Microsoft Azure Functions

  ```sh
  ubuntu@trusty:~/faasshell[master]$ envsubst < samples/azure/asl/translation.json | \
  curl -ksX PUT ${FAASSHELL_APIHOST}/statemachine/translation.json?overwrite=true \
  -H 'Content-Type: application/json' -d @/dev/stdin -u $DEMO
  ```

- Google Cloud Functions

  ```sh
  ubuntu@trusty:~/faasshell[master]$ envsubst < samples/gcp/asl/translation.json | \
  curl -ksX PUT ${FAASSHELL_APIHOST}/statemachine/translation.json?overwrite=true \
  -H 'Content-Type: application/json' -d @/dev/stdin -u $DEMO
  ```

- IBM Cloud Functions / Apache OpenWhisk

  ```sh
  ubuntu@trusty:~/faasshell[master]$ curl -ksX PUT ${FAASSHELL_APIHOST}/statemachine/translation.json?overwrite=true \
  -H 'Content-Type: application/json' -d @samples/wsk/asl/translation.json -u $DEMO
  ```

## Confirm the registerd statemachine

```sh
ubuntu@trusty:~/faasshell[master]$ curl -ksX GET ${FAASSHELL_APIHOST}/statemachine/translation.json -u $DEMO
```

## Execute the statemachine

```sh
ubuntu@trusty:~/faasshell[master]$ curl -ksX POST ${FAASSHELL_APIHOST}/statemachine/translation.json?blocking=true \
-H 'Content-Type: application/json' \
-d '{
    "input": {
        "forward": {
            "payload":"hello world",
            "translateFrom": "en",
            "translateTo":"de"
        },
        "backward": {
            "translateFrom": "de",
            "translateTo":"en"
        }
    }
}' -u $DEMO

ubuntu@trusty:~/faasshell[master]$ curl -ksX POST ${FAASSHELL_APIHOST}/statemachine/translation.json?blocking=true \
-H 'Content-Type: application/json' \
-d '{
    "input": {
        "forward": {
            "payload":"the professor lectures to the student with the cat.",
            "translateFrom": "en",
            "translateTo":"zh"
        },
        "backward": {
            "translateFrom": "zh",
            "translateTo":"en"
        }
    }
}' -u $DEMO
```
