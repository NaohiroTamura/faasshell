'use strict';

console.log('Loading function');

exports.handler = (event, context, callback) => {
    console.log('Received event:', JSON.stringify(event, null, 2));
    var name = event.name || 'World';
    callback(null, {payload:  'Hello, ' + name + '!'});
};
