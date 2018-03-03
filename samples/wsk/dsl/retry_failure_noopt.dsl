asl([task('CreateAccount',"wsk:helloPython",
          [timeout_seconds(2),
           fallback([case('ErrorEquals'(["CustomError"]),
                          [pass('CustomErrorFallback',[result("A")])]),
                     case('ErrorEquals'(["States.TaskFailed"]),
                          [pass('ReservedTypeFallback',[result("B")])]),
                     case('ErrorEquals'(["States.ALL"]),
                          [pass('CatchAllFallback',[result("C")])])])])]).
