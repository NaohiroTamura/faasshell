# Environment Variables

## FaaS Shell

| environment variable  | value        | default value             |
| :-------------------- | :----------- | :------------------------ |
| FAASSHELL_SVC_PORT    | number       | 8080 (http), 8433 (https) |


## Database

Database is either [Apache CouchDB][1] or [IBM Cloudant][2].

| environment variable  | value            | default value         |
| :-------------------- | :--------------- | :-------------------- |
| FAASSHELL_DB_AUTH     | ID:PW            |                       |
| FAASSHELL_DB_APIHOST  | URL              | http://127.0.0.1:5984 |

In Kubernetes or OpenShift, FaaS Shell pod looks up the environment variable
"**COUCHDB_SERVICE_HOST**" and "**COUCHDB_SERVICE_PORT**" in faasshell namespace
if "**FAASSHELL_DB_APIHOST**" is not set.

[1]: http://couchdb.apache.org/ "Apache CouchDB"
[2]: https://www.ibm.com/cloud/cloudant "IBM Cloudant"


## Message Queue

Message Queue is either [SWI Prolog][3] built_in or [Apache Kafka][4].

| environment variable  | literal value           | default value  |
| :-------------------- | :---------------------- | :------------- |
| FAASSHELL_MQ          | "built_in" or "kafka"   | "built_in"     |

In Kubernetes or OpenShift, [Apache Kafka][4] is necessary if the number of
replication of FaaS Shell pod is more than two.

[3]: http://www.swi-prolog.org/ "SWI-Prolog"
[4]: https://kafka.apache.org/ "Apache Kafka"


## Proxy

| environment variable  | format                                   |
| :-------------------- | :--------------------------------------- |
| HTTP_PROXY            | scheme:[//[user[:password]@]host[:port]] |
| HTTPS_PROXY           | scheme:[//[user[:password]@]host[:port]] |
| NO_PROXY              | host_or_ip,host_or_ip,...                |


## FaaS

Setting vendor access information into environment variables is tentative implementation for PoC.

**This implementation will be changed later.**

| vendor        | environment variable 1 | environment variable 2 | environment variable 3 | environment variable 4 |
| :------------ | :--------------------- | :--------------------- | :--------------------- | :--------------------- |
| AWS           | AWS_ACCESS_KEY_ID      | AWS_SECRET_ACCESS_KEY  | -                      | -                      |
| GCP           | GOOGLE_APPLICATION_CREDENTIALS | -              | -                      | -                      |
| Azure         | AZURE_HOSTKEY          | AZURE_TENANT_ID        | AZURE_CLIENT_ID        | AZURE_CLIENT_SECRET    |
| IBM/OpenWhisk | WSK_AUTH               | WSK_APIHOST            | -                      | -                      |
