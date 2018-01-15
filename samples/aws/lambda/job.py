from __future__ import print_function

import json

print('Loading function')


def lambda_handler(event, context):
    #print("Received event: " + json.dumps(event, indent=2))
    result = event.get("params", ["DEFAULT"])
    print(result[0])
    return {"status": result[0], "params": result[1:]}
    #raise Exception('Something went wrong')
