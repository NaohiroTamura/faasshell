/**
 * Responds to any HTTP request that can provide a "message" field in the body.
 *
 * @param {!Object} req Cloud Function request context.
 * @param {!Object} res Cloud Function response context.
 */
exports.delay = (req, res) => {
    let name = req.body.name || 'World';
    let delay = req.body.sleep * 1000 || 0;
    console.log(`delay: ${delay}`)
    return new Promise((resolve) => {
        setTimeout(() => {
            console.log('timer set off');
            resolve({payload: 'Hello, ' + name + '!', sleep: delay});
        }, delay)})
        .then((p) => {
            res.status(200).send(p);
        });
};
