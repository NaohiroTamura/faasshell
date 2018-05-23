{
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

{
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
}


naohirot@bluemix:~/work/github/faasshell[master]$ curl -ksX PUT ${FAASSHELL_APIHOST}/statemachine/translation.json?overwrite=true \
-H 'Content-Type: application/json' -d @samples/aws/asl/translation.json -u $DEMO

naohirot@bluemix:~/work/github/faasshell[master]$ curl -ksX GET ${FAASSHELL_APIHOST}/statemachine/translation.json -u $DEMO

naohirot@bluemix:~/work/github/faasshell[master]$ curl -ksX POST ${FAASSHELL_APIHOST}/statemachine/translation.json?blocking=true \
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

naohirot@bluemix:~/work/github/faasshell[master]$ curl -ksX POST ${FAASSHELL_APIHOST}/statemachine/translation.json?blocking=true \
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

