fsm([task('HelloWorld',"frn:wsk:functions:::function:hello",[]),
     pass('UpdateArg',[result_path('$.ifttt.value1'),
                       input_path('$.payload'),
                       output_path('$.ifttt')]),
     task('SaveResult',"frn:ifttt:webhooks:::function:save_result",[])]).
