/**
 * Responds to any HTTP request that can provide a "name" field in the body.
 *
 * @param {!Object} req Cloud Function request context.
 * @param {!Object} res Cloud Function response context.
 */
exports.helloWorld = (req, res) => {
  var name = req.body.name || 'World';
  res.status(200).send({payload:  'Hello, ' + name + '!'});
};
