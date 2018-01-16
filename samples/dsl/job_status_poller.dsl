asl([task('Submit Job',"wsk:/whisk.system/utils/echo",[result_path('$.guid')]),
     wait('Wait X Seconds',seconds_path('$.wait_time'),[]),
     task('Get Job Status',"wsk:job",[input_path('$.guid'),result_path('$.guid')]),
     choices('Job Complete?',
             [case('StringEquals'('$.guid.status',"FAILED"),
                   [fail('Job Failed',[error("DescribeJob returned FAILED"),
                                       cause("AWS Batch Job Failed")])]),
              case('StringEquals'('$.guid.status',"SUCCEEDED"),
                   [task('Get Final Job Status',"wsk:hello",[])])],
             [default([goto(state('Wait X Seconds'))])])]).

