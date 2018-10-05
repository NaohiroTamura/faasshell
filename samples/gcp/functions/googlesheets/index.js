/**
 * Responds to any HTTP request that can provide a "message" field in the body.
 *
 * @param {!Object} req Cloud Function request context.
 * @param {!Object} res Cloud Function response context.
 */
const {google} = require('googleapis');

exports.googlesheets = (req, res) => {
    google.auth.getClient({
        scopes: [
            'https://www.googleapis.com/auth/drive',
            'https://www.googleapis.com/auth/drive.file',
            'https://www.googleapis.com/auth/spreadsheets'
        ]
    })
    .then( client => {
        console.log(`client: ${client}`);
        const sheets = google.sheets({
            version: 'v4',
            auth: client
        });
        return(sheets);
    })
    .then( sheets => {
        console.log(`sheets: ${sheets}`);
        sheets.spreadsheets.values.append({
            spreadsheetId: req.body.sheetId,
            range: 'Sheet1!A1:C1',
            valueInputOption: 'USER_ENTERED',
            resource: {
                values: req.body.values
            }
        }, (err, response) => {
            if (err) {
                console.error(err.errors);
                if(res) {
                    res.status(500).send({error: err.errors});
                }
            }
            else {
                console.log(response.data);
                if(res) {
                    res.status(200).send(response.data);
                }
            }
        });
    })
    .catch( e => {
        console.error(e);
        if(res) {
            res.status(401).send({error: 'Unauthorized'});
        }
    });
};

// set GOOGLE_APPLICATION_CREDENTIALS environment variable for local execution,
// and add service account email to the sheet by pushing "SHARE" button.
if (module === require.main) {
    const req = {
        body: {
            sheetId: '1ywCxG8xTKOYK89AEZIqgpTvbvpbrb1s4H_bMVvKV59I',
            values: [
                    [Date(), 'Justin', 'Website'],
                    [Date(), 'Node.js', 'Fun']
            ]
        }
    };
    exports.googlesheets(req, null)
}
/*
{"sheetId": "1ywCxG8xTKOYK89AEZIqgpTvbvpbrb1s4H_bMVvKV59I",
 "values": [
   ["DateTime1", "Justin", "Website"],
   ["DateTime2", "Node.js", "Fun"]
 ]
}
*/
