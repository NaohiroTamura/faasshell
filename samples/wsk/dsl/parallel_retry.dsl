fsm([parallel('Parallel',
              branches([[task('HelloWorld',"frn:wsk:functions:::function:helloPython",[timeout_seconds(2)])],
                        [pass('Pass',[]),wait('Wait 10s',seconds(1),[])]]),
              [result_path(parallel),
               retry([case(error_equals(["CustomError"]),
                           [interval_seconds(1),max_attempts(2),backoff_rate(2.0)]),
                      case(error_equals(["States.TaskFailed"]),
                           [interval_seconds(3),max_attempts(2),backoff_rate(2.0)]),
                      case(error_equals(["States.ALL"]),
                           [interval_seconds(1),max_attempts(3),backoff_rate(2.0)])
                    ])
             ]),
     pass('Final State',[])]).
