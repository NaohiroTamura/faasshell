#
# testing for failure
#
from __future__ import print_function

import json

def main(event):
    print("Received event: " + json.dumps(event, indent=2))
    code = event.get("error", "CustomError('This is a custom error!')")
    error = eval(code)
    raise error

class CustomError(Exception):
    pass

if __name__ == '__main__':
    try:
        main({})
    except Exception as e:
        print(e)

    try:
        main({"error": "AssertionError('Created dynamically!')"})
    except Exception as e:
        print(e)
