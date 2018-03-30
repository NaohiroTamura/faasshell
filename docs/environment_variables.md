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

**Later, all credentials will be managed as fassshell user's property in DB or CLI parameter.**

| vendor        | environment variable           | descripton                                  |
| :------------ | :----------------------------- | :------------------------------------------ |
| AWS ([\*1][5])| AWS_ACCESS_KEY_ID              | aws_access_key_id     in ~/.aws/credentials |
|               | AWS_SECRET_ACCESS_KEY          | aws_secret_access_key in ~/.aws/credentials |
| GCP ([\*2][6],[\*3][7]) | GOOGLE_APPLICATION_CREDENTIALS | full path to JSON credential file |
| Azure         | AZURE_HOSTKEY                  | Host keys ([\*4][8])                        |
|               | AZURE_TENANT_ID                | tenant ID ([\*5][9])                        |
|               | AZURE_CLIENT_ID                | client_id is the Application ID ([\*6][10]) |
|               | AZURE_CLIENT_SECRET            | client_secret  ([\*6][10])                  |
| IBM/OpenWhisk ([\*7][11]) | WSK_AUTH           | AUTH    in ~/.wskprops                      |
|               | WSK_APIHOST                    | APIHOST in ~/.wskprops                      |
| IFTTT         | IFTTT_KEY                      | click "Document" button ([\*8][12])         |


[5]: https://docs.aws.amazon.com/general/latest/gr/managing-aws-access-keys.html "Managing Access Keys for Your AWS Account"
[6]: https://cloud.google.com/docs/authentication/getting-started "Getting Started with Authentication"
[7]: https://cloud.google.com/docs/authentication/production "Setting Up Authentication for Server to Server Production Applications"
[8]: https://docs.microsoft.com/en-us/azure/azure-functions/functions-bindings-http-webhook#trigger---usage "Azure Functions HTTP and webhook bindings"
[9]: https://docs.microsoft.com/en-us/azure/azure-resource-manager/resource-group-create-service-principal-portal#get-tenant-id "Create identity for Azure app in portal"
[10]: https://docs.microsoft.com/en-us/azure/active-directory/develop/active-directory-protocols-oauth-service-to-service "Azure AD Service to Service Auth using OAuth2.0"
[11]: https://console.bluemix.net/docs/openwhisk/openwhisk_cli.html#stand-alone-cli "Stand-alone CLI"
[12]: https://ifttt.com/maker_webhooks "Do more with Webhooks - IFTTT"
