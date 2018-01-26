asl([wait('Wait for Timestamp',seconds_path('$.timer_seconds'),[]),
     task('Send SNS Message',"wsk:sns",
          [retry([case('ErrorEquals'(["States.ALL"]),
                       [interval_seconds(1),max_attempts(3),backoff_rate(2)])])])]).
