import time

def main(args):
    name = args.get("name", "World")
    sleep = args.get("sleep", 0)
    print(sleep)
    time.sleep(sleep)
    return {"payload": "Hello, " + name + "!",
            "sleep": sleep}

if __name__ == '__main__':
    print(main({"name":"whisk", "sleep": 5}))
