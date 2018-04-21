fsm([parallel('Parallel',
              branches([[task('HelloWorld',"frn:wsk:functions:::function:helloPython",[timeout_seconds(2)])],
                        [pass('Pass',[]),wait('Wait 10s',seconds(1),[])]]),
              [result_path(parallel),
               catch([case(error_equals(["CustomError"]),
                              [pass('CustomErrorFallback',[result("A")])]),
                         case(error_equals(["States.TaskFailed"]),
                              [pass('ReservedTypeFallback',[result("B")])]),
                         case(error_equals(["States.ALL"]),
                              [pass('CatchAllFallback',[result("C")])])])
             ]),
     pass('Final State',[])]).
