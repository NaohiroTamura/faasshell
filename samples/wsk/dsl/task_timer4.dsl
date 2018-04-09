asl([wait('Wait for Timestamp',timestamp("2017-11-20T09:36:00Z"),[]),
     task('Send SNS Message',"frn:wsk:functions:::function:hello1",
          [retry([case(error_equals(["States.ALL"]),
                       [interval_seconds(1),max_attempts(3),backoff_rate(2.0)])]
   )])]).
