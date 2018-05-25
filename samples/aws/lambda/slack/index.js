'use strict';

var request = require('request');

exports.handler = (event, context, callback) => {
    let payload = {
        channel: event.channel,
        username: event.username,
        text: JSON.stringify(event.text)
    };

    let body = {
        payload: JSON.stringify(payload)
    };

    return new Promise((resolve, reject) => {
        request.post({
            url: process.env.SLACK_INCOMMING_WEBHOOK_URL,
            formData: body
        },
        (err, res, body) => {
            if (err) {
                console.log('error: ', err, body);
                reject(err);
            } else {
                console.log('success: ', event.text, 'successfully sent');
                resolve({});
            }
        });
    })
    .then((p) => callback(null, p))
    .catch((e) => callback(e));
}
