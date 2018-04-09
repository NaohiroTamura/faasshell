asl([task('CreateAccount',"frn:wsk:functions:::function:helloPython",
          [timeout_seconds(2),
           fallback([case(error_equals(["CustomError"]),
                          [pass('CustomErrorFallback',[result("A")])]),
                     case(error_equals(["States.TaskFailed"]),
                          [pass('ReservedTypeFallback',[result("B")])]),
                     case(error_equals(["States.ALL"]),
                          [pass('CatchAllFallback',[result("C")])])])])]).
