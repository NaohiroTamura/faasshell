# Google Sheets Function

## Install

```sh
ubuntu@xeniel:~/github/faasshell[master]$ gcloud beta functions deploy googlesheet \
    --trigger-http --source=samples/gcp/functions/googlesheets \
    --entry-point=googlesheets
```

## Execution

```sh
ubuntu@xeniel:~/github/faasshell[master]$ gcloud beta functions call googlesheet \
    --data '{
        "sheetId": "1ywCxG8xTKOYK89AEZIqgpTvbvpbrb1s4H_bMVvKV59I",
        "values": [
            ["fujitsu.com", "\"naohirotamura\"", "\"faasshell\"",
                "\"2018-06-21T00:00:00+00:00\"", "\"2018-07-20T00:00:00+00:00\"", 2]
        ]}'
```
