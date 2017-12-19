asl([task('FirstState',"helloPython",[]),
         wait(wait_using_seconds,seconds(3),[]),
         wait(wait_using_timestamp,timestamp("2015-09-04T01:59:00Z"),[]),
         wait(wait_using_timestamp_path,timestamp_path(expirydate),[]),
         wait(wait_using_seconds_path,seconds_path(expiryseconds),[]),
         task('FinalState',"hello",[])]).
