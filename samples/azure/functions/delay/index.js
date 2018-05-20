module.exports = function (context, req) {
    context.log('JavaScript HTTP trigger function processed a request.');

    let name = req.body.name || 'World';
    let delay = req.body.sleep * 1000 || 0;
    context.log(`delay: ${delay}`)
    return new Promise((resolve) => {
        setTimeout(() => {
            context.log('timer set off');
            resolve({payload: 'Hello, ' + name + '!', sleep: delay});
        }, delay)})
        .then((p) => {
            context.res = p;
            // Error: Choose either to return a promise or call 'done'.
            // Do not use both in your script.
            // context.done();
        });
};
