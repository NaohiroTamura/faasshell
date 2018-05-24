var request = require('request');

function main(params) {
    let payload = {
        channel: params.channel,
        username: params.username,
        text: JSON.stringify(params.text)
    };

    let body = {
        payload: JSON.stringify(payload)
    };

    return new Promise((resolve, reject) => {
        request.post({
            url: params.url,
            formData: body
        },
        (err, res, body) => {
            if (err) {
                console.log('error: ', err, body);
                reject(err);
            } else {
                console.log('success: ', params.text, 'successfully sent');
                resolve();
            }
        });
    });
}
