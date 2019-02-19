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
        return google.sheets({
            version: 'v4',
            auth: client
        });
    })
    .catch( err => {
        console.error(err);
        if(res) {
            res.status(401).send({error: 'Unauthorized'});
        }
    })
    .then( sheets => {
        console.log(`sheets: ${sheets}`);
        sheets.spreadsheets.batchUpdate({
            spreadsheetId: req.body.sheetId,
            requestBody: {
                includeSpreadsheetInResponse: true,
                requests: [
                    {
                        insertDimension: {
                            range: {
                                sheetId: 0,
                                dimension: 'ROWS',
                                startIndex: 0,
                                endIndex: 1
                            },
                            inheritFromBefore: false
                        }
                    }
                ]
            }
        })
        .catch( err => {
            console.error(err.errors);
            if(res) {
                res.status(500).send({error: err.errors});
            }
        })
        .then( response => {
            console.log(response.data);
            const now = [Date()];
            const data = req.body.values.map(e => now.concat(e));
            return sheets.spreadsheets.values.append({
                spreadsheetId: req.body.sheetId,
                range: 'Sheet1!A1:A1',
                valueInputOption: 'USER_ENTERED',
                resource: {
                    values: data
                }
            })
        })
        .catch( err => {
            console.error(err.errors);
            if(res) {
                res.status(500).send({error: err.errors});
            }
        })
        .then( response => {
            console.log(response.data);
            if(res) {
                res.status(200).send(response.data);
            }
        });
    });
};

// set GOOGLE_APPLICATION_CREDENTIALS environment variable for local execution,
// and add service account email to the sheet by pushing "SHARE" button.
if (module === require.main) {
    const req = {
        body: {
            sheetId: '1ywCxG8xTKOYK89AEZIqgpTvbvpbrb1s4H_bMVvKV59I',
            values: [
                ["fujitsu.com", '"naohirotamura"', '"faasshell"',
                 '"2018-06-21T00:00:00+00:00"', '"2018-07-20T00:00:00+00:00"', 2],
                //["redhat.com", '"containers"', '"buildah"',
                // '"2018-02-21T00:00:00+00:00"', '"2018-04-20T00:00:00+00:00"', 53]
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
