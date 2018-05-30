# Translation Text Demo

## Sequential Translation

### Register statemachine

  ```sh
  ubuntu@trusty:~/faasshell[master]$ envsubst < samples/common/asl/translation_all_seq.json | \
  curl -ksX PUT ${FAASSHELL_APIHOST}/statemachine/translationn_all_seq.json?overwrite=true \
  -H 'Content-Type: application/json' -d @/dev/stdin -u $DEMO
```

### Confirm the registerd statemachine

```sh
ubuntu@trusty:~/faasshell[master]$ curl -ksX GET ${FAASSHELL_APIHOST}/statemachine/translationn_all_seq.json -u $DEMO
```

### Execute the statemachine

```sh
ubuntu@trusty:~/faasshell[master]$ curl -ksX POST ${FAASSHELL_APIHOST}/statemachine/translationn_all_seq.json?blocking=true \
-H 'Content-Type: application/json' \
-d '{
    "input": {
        "forward": {
            "payload":"the professor lectures to the student with the cat.",
            "translateFrom": "en",
            "translateTo":"ar"
        },
        "backward": {
            "translateFrom": "ar",
            "translateTo":"en"
        }
    }
}' -u $DEMO

ubuntu@trusty:~/faasshell[master]$ curl -ksX POST ${FAASSHELL_APIHOST}/statemachine/translationn_all_seq.json?blocking=true \
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

ubuntu@trusty:~/faasshell[master]$ curl -ksX POST ${FAASSHELL_APIHOST}/statemachine/translationn_all_seq.json?blocking=true \
-H 'Content-Type: application/json' \
-d '{
    "input": {
        "forward": {
            "payload":"the professor lectures to the student with the cat.",
            "translateFrom": "en",
            "translateTo":"ja"
        },
        "backward": {
            "translateFrom": "ja",
            "translateTo":"en"
        }
    }
}' -u $DEMO
```

## Parallel Translation

### Register statemachine

  ```sh
  ubuntu@trusty:~/faasshell[master]$ envsubst < samples/common/asl/translation_all_par.json | \
  curl -ksX PUT ${FAASSHELL_APIHOST}/statemachine/translationn_all_par.json?overwrite=true \
  -H 'Content-Type: application/json' -d @/dev/stdin -u $DEMO
```

### Confirm the registerd statemachine

```sh
ubuntu@trusty:~/faasshell[master]$ curl -ksX GET ${FAASSHELL_APIHOST}/statemachine/translationn_all_par.json -u $DEMO
```

### Execute the statemachine

```sh
ubuntu@trusty:~/faasshell[master]$ curl -ksX POST ${FAASSHELL_APIHOST}/statemachine/translationn_all_par.json?blocking=true \
-H 'Content-Type: application/json' \
-d '{
    "input": {
        "forward": {
            "payload":"the professor lectures to the student with the cat.",
            "translateFrom": "en",
            "translateTo":"de"
        },
        "backward": {
            "translateFrom": "de",
            "translateTo":"en"
        }
    }
}' -u $DEMO

ubuntu@trusty:~/faasshell[master]$ curl -ksX POST ${FAASSHELL_APIHOST}/statemachine/translationn_all_par.json?blocking=true \
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

ubuntu@trusty:~/faasshell[master]$ curl -ksX POST ${FAASSHELL_APIHOST}/statemachine/translationn_all_par.json?blocking=true \
-H 'Content-Type: application/json' \
-d '{
    "input": {
        "forward": {
            "payload":"the professor lectures to the student with the cat.",
            "translateFrom": "en",
            "translateTo":"ja"
        },
        "backward": {
            "translateFrom": "ja",
            "translateTo":"en"
        }
    }
}' -u $DEMO
```
