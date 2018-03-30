### create "save_result" applet

* open [Do more with Webhooks - IFTTT][1]in browser
* push "+this" button
* search "webhooks"
* push "Webhooks" icon
* push "Receive a web request" area
* type "save_result" in the Event Name text field
* push "Create trigger" button
* push "+that" button
* search "Google Sheets"
* push "Google Sheets" icon
* select "Add row to spreadsheet"
* push "Create action" button without changing the following default parameters:
  * Spreadsheet name: IFTTT_Maker_Webhooks_Events
  * Formatted row: \{\{OccurredAt\}\} \|\|\| \{\{EventName\}\} \|\|\| \{\{Value1\}\} \|\|\|\{\{Value2\}\} \|\|\| \{\{Value3\}\}
  * Drive folder path: IFTTT/MakerWebooks/\{\{EventName\}\}

### test "save_result" applet

* open [Do more with Webhooks - IFTTT][1]in browser
* push "Documentation Button"
* test the applet with your key by following the steps in
  ```sh
  $ curl -isX POST https://maker.ifttt.com/trigger/save_result/with/key/XXXXXXXXXXXXXXXXXXXXX \
    -H 'Content-Type: application/json' \
    -d '{"value1":"first", "value2":"second", "value3":"third"}'
  ```
* verify your Google sheets "IFTTT_Maker_Webhooks_Events",
  https://docs.google.com/spreadsheets/, and check if the parameters are saved in
  a new row.

[1]: https://ifttt.com/maker_webhooks "Do more with Webhooks - IFTTT"
