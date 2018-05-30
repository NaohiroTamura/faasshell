'use strict';

let fs = require ('fs');
let https = require ('https');
let msRestAzure = require('ms-rest-azure');
let KeyVault = require('azure-keyvault');
let KEY_VAULT_URI = 'https://translatortextkey.vault.azure.net/';

function getKeyVaultCredentials(){
    return msRestAzure.loginWithAppServiceMSI();
};

function getKeyVaultSecret(credentials) {
    let keyVaultClient = new KeyVault.KeyVaultClient(credentials);
    return keyVaultClient.getSecret(KEY_VAULT_URI, 'TranslatorTextKey', "");
};

module.exports = function (context, req) {
    context.log('JavaScript HTTP trigger function processed a request.');
    context.log(`MSI_SECRET: ${process.env.MSI_SECRET}`);
    context.log(`MSI_ENDPOINT: ${process.env.MSI_ENDPOINT}`);

    let host = 'api.cognitive.microsofttranslator.com';
    let path = '/translate?api-version=3.0';

    // Language to translate to
    let params = '&to=' + req.body.translateTo;
    let text = req.body.payload;

    let response_handler = function (response) {
        let body = '';
        response.on ('data', function (d) {
            body += d;
        });
        response.on ('end', function () {
            let json = JSON.stringify(JSON.parse(body), null, 4);
            context.log(json);
            context.res = {
                status: 200,
                body: {payload: JSON.parse(body)[0].translations[0].text}
            };
            context.done();
        });
        response.on ('error', function (e) {
            context.log ('Error: ' + e.message);
        });
    };

    let get_guid = function () {
        return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
            var r = Math.random() * 16 | 0, v = c == 'x' ? r : (r & 0x3 | 0x8);
            return v.toString(16);
        });
    }

    let Translate = function (content, subscriptionKey) {
        let request_params = {
            method : 'POST',
            hostname : host,
            path : path + params,
            headers : {
                'Content-Type' : 'application/json',
                'Ocp-Apim-Subscription-Key' : subscriptionKey,
                'X-ClientTraceId' : get_guid (),
            }
        };

        let req = https.request (request_params, response_handler);
        req.write (content);
        req.end ();
    }

    let content = JSON.stringify ([{'Text' : text}]);
    Translate (content, process.env.TRANSLATOR_KEY);

/*
    return getKeyVaultCredentials().then(
        getKeyVaultSecret
    ).then(function (secret){
        context.log(`Your secret value is: ${secret.value}.`);
        Translate (content, secret.value)
    }).catch(function (err) {
        throw (err);
    }); */
};
