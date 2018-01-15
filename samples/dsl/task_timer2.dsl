asl([wait('Wait for Seconds',seconds_path(trigger),[]),
     task('Send SNS Message',"wsk:hello1",
          [retry([case('ErrorEquals'(["States.ALL"]),
                       [interval_seconds(1),max_attempts(3),backoff_rate(2.0)])]
   )])]).
