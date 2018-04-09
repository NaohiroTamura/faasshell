asl([wait('Wait for Timestamp',seconds_path('$.timer_seconds'),[]),
     task('Send SNS Message',"frn:wsk:functions:::function:sns",
          [retry([case(error_equals(["States.ALL"]),
                       [interval_seconds(1),max_attempts(3),backoff_rate(2)])])])]).
