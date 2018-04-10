fsm([parallel('Parallel',
              branches([[wait('Wait 20s',seconds(5),[])],
                        [pass('Pass',[]),wait('Wait 10s',seconds(1),[])]
                      ]), []),
     pass('Final State',[])]).
