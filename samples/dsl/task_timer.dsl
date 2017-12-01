asl([wait('Wait for Timestamp',timestamp_path(trigger_date),[]),
     task('Send SNS Message',"hello1",
          [retry([case('ErrorEquals'(["States.ALL"]),
                       [interval_seconds(1),max_attempts(3),backoff_rate(2.0)])]
   )])]).
