from __future__ import print_function

import json

print('Loading function')

def lambda_handler(event, context):
    print("Received event: " + json.dumps(event, indent=2))
    result = event.get("status", "ERROR")
    if result == "ERROR":
        raise Exception("This is an error")
    else:
        return event
