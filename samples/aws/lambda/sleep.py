from __future__ import print_function

import json
import time

print('Loading function')

def lambda_handler(event, context):
    #print("Received event: " + json.dumps(event, indent=2))
    name = event.get("name", "World")
    sleep = event.get("sleep", 0)
    print(sleep)
    time.sleep(sleep)
    return {"payload": "Hello, " + name + "!",
            "sleep": sleep}
