# Demo

## Commit Count Report

### Deploy and Test FaaS

#### GitHub

```sh
ubuntu@xeniel:~/github/faasshell[master]$ wsk action create graphql samples/wsk/actions/graphql.pl --docker nao16t/swipl7action -i

ubuntu@xeniel:~/github/faasshell[master]$ wsk action update graphql --param github_token $GITHUB_TOKEN

ubuntu@xeniel:~/github/faasshell[master]$ wsk action invoke graphql -ir \
    -p target fujitsu.com \
    -p owner '"naohirotamura"' \
    -p name '"faasshell"' \
    -p since '\"2018-06-21T00:00:00+00:00\"' \
    -p until '\"2018-07-20T00:00:00+00:00\"'
```

#### Google Sheets

```sh
ubuntu@xeniel:~/github/faasshell[master]$ gcloud beta functions deploy googlesheet \
    --trigger-http --source=samples/gcp/functions/googlesheets \
    --entry-point=googlesheets

ubuntu@xeniel:~/github/faasshell[master]$ gcloud beta functions call googlesheet \
    --data '{
        "sheetId": "1ywCxG8xTKOYK89AEZIqgpTvbvpbrb1s4H_bMVvKV59I",
        "values": [
            ["fujitsu.com", "\"naohirotamura\"", "\"faasshell\"",
                "\"2018-06-21T00:00:00+00:00\"", "\"2018-07-20T00:00:00+00:00\"", 2]
        ]}'
```

### Deploy and Execute Statemachine

```sh
ubuntu@xeniel:~/github/faasshell[master]$ envsubst < samples/common/asl/commit_count_report.json | \
    curl -ksX PUT ${FAASSHELL_APIHOST}/statemachine/commit_count_report.json?overwrite=true \
    -H 'Content-Type: application/json' -d @/dev/stdin -u $DEMO
```

```sh
ubuntu@xeniel:~/github/faasshell[master]$ curl -ksX POST ${FAASSHELL_APIHOST}/statemachine/commit_count_report.json?blocking=true \
    -H 'Content-Type: application/json' \
    -d "{\"input\": {
          \"github\": {
             \"target\": \"fujitsu.com\",
             \"owner\": \"\\\"naohirotamura\\\"\",
             \"name\": \"\\\"faasshell\\\"\",
             \"since\": \"\\\"2018-06-21T00:00:00+00:00\\\"\",
             \"until\": \"\\\"2018-07-20T00:00:00+00:00\\\"\"
          },
          \"gsheet\": {
             \"sheetId\": \"1ywCxG8xTKOYK89AEZIqgpTvbvpbrb1s4H_bMVvKV59I\"
          }
       }}" -u $DEMO
```
