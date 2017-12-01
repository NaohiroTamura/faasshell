asl([wait('Wait for Timestamp',timestamp("2017-11-20T09:36:00Z"),[]),
     task('Send SNS Message',"hello1",
          [retry([case('ErrorEquals'(["States.ALL"]),
                       [interval_seconds(1),max_attempts(3),backoff_rate(2.0)])]
   )])]).
