# Knative Deployment Experiment

## Installing Knative

* [Knative Install on Kubeadm](https://github.com/NaohiroTamura/knative-experiment/blob/master/docs/install/Knative-with-Kubeadm.md)

## Deploying FaaS Shell

```sh
$ docker build -t nao16t/faasshell-knative .

$ docker push nao16t/faasshell-knative

$ . ../bin/faasshell.env

$ kubectl create secret generic faasshell \
		--from-literal=faasshell_db_auth=${FAASSHELL_DB_AUTH} \
		--from-literal=faasshell_db_apihost=${FAASSHELL_DB_APIHOST} \
		--from-literal=aws_access_key_id=${AWS_ACCESS_KEY_ID} \
		--from-literal=aws_secret_access_key=${AWS_SECRET_ACCESS_KEY} \
		--from-file=gcp_app_cred=${GOOGLE_APPLICATION_CREDENTIALS} \
		--from-literal=azure_hostkey=${AZURE_HOSTKEY} \
		--from-literal=azure_tenant_id=${AZURE_TENANT_ID} \
		--from-literal=azure_client_id=${AZURE_CLIENT_ID} \
		--from-literal=azure_client_secret=${AZURE_CLIENT_SECRET} \
		--from-literal=wsk_auth=${WSK_AUTH} \
		--from-literal=wsk_apihost=${WSK_APIHOST} \
        --from-literal=ifttt_key=${IFTTT_KEY}

$ kubectl apply -f faasshell.yml

$ kubectl get pod
```

## Access FaaS Shell

```sh
$ export IP_ADDRESS=$(kubectl get node -o 'jsonpath={.items[0].status.addresses[0].address}'):$(kubectl get svc knative-ingressgateway -n istio-system -o 'jsonpath={.spec.ports[?(@.port==80)].nodePort}')

$ export HOST_URL=$(kubectl get services.serving.knative.dev faasshell-knative -o jsonpath='{.status.domain}')

$ curl -H "Host: ${HOST_URL}" http://${IP_ADDRESS} -u $DEMO
{"version":"$Id rev.2018-07-08.2e2c6c4 $"}
```
