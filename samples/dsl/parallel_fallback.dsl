asl([parallel('Parallel',
              branches([[task('HelloWorld',"helloPython",[timeout_seconds(2)])],
                        [pass('Pass',[]),wait('Wait 10s',seconds(1),[])]]),
              [result_path(parallel),
               fallback([case('ErrorEquals'(["CustomError"]),
                              [pass('CustomErrorFallback',[result("A")])]),
                         case('ErrorEquals'(["States.TaskFailed"]),
                              [pass('ReservedTypeFallback',[result("B")])]),
                         case('ErrorEquals'(["States.ALL"]),
                              [pass('CatchAllFallback',[result("C")])])])
             ]),
     pass('Final State',[])]).
