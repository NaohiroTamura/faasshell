#
# testing for task timer
#
from __future__ import print_function

import json

def main(event):
    print("Received event: " + json.dumps(event, indent=2))
    result = event.get("status", "ERROR")
    if result == "ERROR":
        raise Exception("This is an error")
    else:
        return event

if __name__ == '__main__':
    try:
        main({})
    except Exception as e:
        print(e)

    print(main({"status": "Sent"}))
