'use strict';

console.log('Loading function');

exports.handler = (event, context, callback) => {
    console.log('Received event:', JSON.stringify(event, null, 2));

    function CustomError(message) {
        this.name = 'CustomError';
        this.message = message;
    }
    CustomError.prototype = new Error();

    let code = event.error || "new CustomError('This is a custom error!')";
    let error = eval(code);
    callback(error);
};
