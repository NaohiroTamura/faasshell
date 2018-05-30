'use strict';

var AWS = require('aws-sdk');

var translate = new AWS.Translate();

console.log('Loading function');

exports.handler = (event, context, callback) => {
    console.log('Received event:', JSON.stringify(event, null, 2));

    var params = {
        SourceLanguageCode: event.translateFrom,
        TargetLanguageCode: event.translateTo,
        Text: event.payload
    };
    translate.translateText(params, function(err, data) {
        if (err) {
            console.log(err, err.stack);
            callback(err);
        }
        else {
            console.log(data);
            callback(null, {payload: data.TranslatedText});
        }
    });
};
