'use strict';

exports.handler = (event, context, callback) => {
    console.log('Received event:', JSON.stringify(event, null, 2));
    let name = event.name || 'World';
    let delay = event.sleep * 1000 || 0;
    new Promise((resolve, reject) => {
        setTimeout(() => {
            resolve({payload: 'Hello, ' + name + '!',
                    sleep: delay});
        }, delay);})
        .then((p) => callback(null, p))
        .catch((e) => callback(e));
};
