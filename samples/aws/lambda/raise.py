from __future__ import print_function

import json

print('Loading function')

def lambda_handler(event, context):
    print("Received event: " + json.dumps(event, indent=2))
    code = event.get("error", "CustomError('This is a custom error!')")
    error = eval(code)
    raise error

class CustomError(Exception):
    pass
