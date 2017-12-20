#
# testing for job_status_poller
#
def main(args):
    result = args.get("params", ["DEFAULT"])
    print(result[0])
    return {"status": result[0], "guid": {"params": result[1:]}}

if __name__ == '__main__':
    a = main({"params": ["SUCCEEDED", "FAILED", "DEFAULT"]})
    print(a)
    b = main(a['guid'])
    print(b)
    c = main(b['guid'])
    print(c)
    d = main({})
