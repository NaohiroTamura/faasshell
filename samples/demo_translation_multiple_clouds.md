# Translation Service Evaluation Demo

In order to evaluate Translation Service in each provider, we use an
ambiguous sentence, "[the professor lectures to the student with the
cat][1]", it may be that the professor is lecturing with the cat, or
that the student has the cat.

We translate this ambiguous sentence into another language once, and
translate it back to English again.

Then we compare the original ambiguous sentence with the final result
to see if the ambiguous semantics is retained.

[1]: https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-28.html#%_sec_4.3.2 "Structure and Interpretation of Computer Programs"

## Sequential Translation

### Register statemachine

source file: [samples/common/asl/translation_all_seq.json](/samples/common/asl/translation_all_seq.json)

![samples/common/asl/translate_all_seq.json](/samples/common/graph/translate_all_seq.png)

  ```sh
  ubuntu@trusty:~/faasshell[master]$ envsubst < samples/common/asl/translation_all_seq.json | \
  curl -ksX PUT ${FAASSHELL_APIHOST}/statemachine/translation_all_seq.json?overwrite=true \
  -H 'Content-Type: application/json' -d @/dev/stdin -u $DEMO
```

### Confirm the registerd statemachine

```sh
ubuntu@trusty:~/faasshell[master]$ curl -ksX GET ${FAASSHELL_APIHOST}/statemachine/translation_all_seq.json -u $DEMO
```

### Execute the statemachine

#### via Germany (de)
```sh
ubuntu@trusty:~/faasshell[master]$ curl -ksX POST ${FAASSHELL_APIHOST}/statemachine/translation_all_seq.json?blocking=true \
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
```

All of translation services kept the the ambiguous semantics via
Germany.

```javascript
AWS {"backward":{"payload":"the professor teaches the student with the cat."},"forward":{"payload":"der Professor unterrichtet den Schüler mit der Katze."}}

Azure {"backward":{"payload":"The professor gives lectures to the pupil with the cat."},"forward":{"payload":"der Professor hält Vorträge an den Schüler mit der Katze."}}

GCP {"backward":{"payload":"The professor lectures the student with the cat."},"forward":{"payload":"der Professor hält dem Schüler mit der Katze Vorträge."}}

IBM {"backward":{"payload":"The professor lectures the students with the cat."},"forward":{"payload":"der Professor Vorlesungen die Studenten mit der Katze."}}
```

## Parallel Translation

### Register statemachine

source file: [samples/common/asl/translation_all_par.json](/samples/common/asl/translation_all_par.json)

![samples/common/asl/translate_all_par.json](/samples/common/graph/translate_all_par.png)

  ```sh
  ubuntu@trusty:~/faasshell[master]$ envsubst < samples/common/asl/translation_all_par.json | \
  curl -ksX PUT ${FAASSHELL_APIHOST}/statemachine/translation_all_par.json?overwrite=true \
  -H 'Content-Type: application/json' -d @/dev/stdin -u $DEMO
```

### Confirm the registerd statemachine

```sh
ubuntu@trusty:~/faasshell[master]$ curl -ksX GET ${FAASSHELL_APIHOST}/statemachine/translation_all_par.json -u $DEMO
```

### Execute the statemachine

#### via Alabic (ar)

```sh
ubuntu@trusty:~/faasshell[master]$ curl -ksX POST ${FAASSHELL_APIHOST}/statemachine/translation_all_par.json?blocking=true \
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
```

All of translation services kept the the ambiguous semantics via
Alabic.

```javascript
AWS {"backward":{"payload":"The professor gives lectures to the student with the cat."},"forward":{"payload":"الأستاذ يلقي محاضرات على الطالبمع القط."}}

Azure {"backward":{"payload":"Professor Lectures for the student with the cat."},"forward":{"payload":"أستاذ محاضرات للطالب مع القط."}}

GCP {"backward":{"payload":"The teacher lectures the student with the cat."},"forward":{"payload":"يحاضر الأستاذ للطالب مع القطة."}}

IBM {"backward":{"payload":"A professor lecture for the student with the cat."},"forward":{"payload":"محاضرة الأستاذ للطالب مع القطة."}}
```

#### via Chinese (zh)

```sh
ubuntu@trusty:~/faasshell[master]$ curl -ksX POST ${FAASSHELL_APIHOST}/statemachine/translation_all_par.json?blocking=true \
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

Only Azure translation service kept the the ambiguous semantics via
Chinese.

```javascript
AWS {"backward":{"payload":"Professor lectures with cats students."},"forward":{"payload":"教授讲座与猫的学生。"}}

Azure {"backward":{"payload":"The professor gave lectures to the students with cats."},"forward":{"payload":"教授用猫讲课给学生听。"}}

GCP {"backward":{"payload":"The professor and the cat taught the students together."},"forward":{"payload":"教授和猫一起给学生讲课。"}}

IBM {"error":{"code":404,"error_code":404,"error_message":"Model not found."}}
```

#### via Japanese (ja)

```sh
ubuntu@trusty:~/faasshell[master]$ curl -ksX POST ${FAASSHELL_APIHOST}/statemachine/translation_all_par.json?blocking=true \
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

Only GCP translation service kept the the ambiguous semantics via
Japanese.

```javascript
AWS {"error":"UnsupportedLanguagePairException"}

Azure {"backward":{"payload":"The professor takes the cat and lectures it to the student."},"forward":{"payload":"教授は猫連れて学生に講義をする。"}}

GCP {"backward":{"payload":"The professor lectures students with a cat."},"forward":{"payload":"教授は猫と一緒に学生に講義をする。"}}

IBM {"backward":{"payload":"The cat professor lecture to students."},"forward":{"payload":"猫は教授が学生に講義。"}}
```
