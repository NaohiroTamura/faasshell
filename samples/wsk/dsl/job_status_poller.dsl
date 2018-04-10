fsm([task('Submit Job',"frn:wsk:functions:::function:/whisk.system/utils/echo",
          [result_path('$.guid')]),
     wait('Wait X Seconds',seconds_path('$.wait_time'),[]),
     task('Get Job Status',"frn:wsk:functions:::function:job",
          [input_path('$.guid'),result_path('$.guid')]),
     choices('Job Complete?',
             [case(string_equals('$.guid.status',"FAILED"),
                   [fail('Job Failed',[error("DescribeJob returned FAILED"),
                                       cause("AWS Batch Job Failed")])]),
              case(string_equals('$.guid.status',"SUCCEEDED"),
                   [task('Get Final Job Status',"frn:wsk:functions:::function:hello",[])])],
             [default([goto(state('Wait X Seconds'))])])]).

