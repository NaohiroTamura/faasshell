# Google Sheets Function

## Install

```sh
ubuntu@xeniel:~/github/faasshell[master]$ gcloud beta functions deploy googlesheet \
    --trigger-http --source=samples/gcp/functions/googlesheets \
    --entry-point=googlesheets
```
