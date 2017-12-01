import random

def main(args):
    name = args.get("payload", "stranger")
    if random.random() > 0.5:
        result = "FAILED"
    else:
        result = "SUCCEEDED"
    print(name, result)
    return {"status": result}

if __name__ == '__main__':
    print(main({"payload": "standalone"}))
