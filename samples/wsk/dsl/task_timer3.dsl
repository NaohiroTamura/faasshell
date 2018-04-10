fsm([wait('Wait for Seconds',seconds(10),[]),
     task('Send SNS Message',"frn:wsk:functions:::function:hello1",
          [retry([case(error_equals(["States.ALL"]),
                       [interval_seconds(1),max_attempts(3),backoff_rate(2.0)])]
   )])]).
