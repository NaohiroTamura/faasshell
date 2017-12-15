asl([task('Submit Job',"hello",[result_path(guid)]),
     wait('Wait X Seconds',seconds_path(wait_time),[]),
     task('Get Job Status',"job",[input_path(guid)]),
     choices('Job Complete?',
             [case('StringEquals'(status,"FAILED"),
                   [fail('Job Failed',[error("DescribeJob returned FAILED"),
                                       cause("AWS Batch Job Failed")])]),
              case('StringEquals'(status,"SUCCEEDED"),
                   [task('Get Final Job Status',"hello",[input_path(guid)])])],
             [default([goto(state('Wait X Seconds'))])])]).

