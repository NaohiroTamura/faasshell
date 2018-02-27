module.exports = function (context, req) {
    context.log('JavaScript HTTP trigger function processed a request.');

    var name = req.body.name || 'World';
    context.res = {
        // status: 200, /* Defaults to 200 */
        body: {payload:  'Hello, ' + name + '!'}
    };
    context.done();
};
