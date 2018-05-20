/**
 * Translate
 *
 * @param {!Object} req Cloud Function request context.
 * @param {!Object} res Cloud Function response context.
 */

// Imports the Google Cloud client library
const Translate = require('@google-cloud/translate');

exports.translate = (req, res) => {

    const translate = new Translate();

    // The text to translate
    const text = req.body.payload;
    // The source and target language
    const source = req.body.translateFrom;
    const target = req.body.translateTo;

    // Translates some text into the target language
    translate
        .translate(text, {from: source, to: target})
        .then(results => {
            const translation = results[0];

            console.log(`Text: ${text}`);
            console.log(`Translation: ${translation}`);
            res.status(200).send({payload: translation});
        })
        .catch(err => {
            console.error('ERROR:', err);
            res.status(500).send({error: err});
        });
};
