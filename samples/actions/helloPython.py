import time

def main(args):
    name = args.get("name", "stranger")
    greeting = "Hello " + name + "!"
    sleep = args.get("sleep", 10)
    time.sleep(sleep)
    print(greeting)
    return {"greeting": greeting,
            "sleep": sleep}

if __name__ == '__main__':
    print(main({"name":"whisk", "sleep": 5}))
