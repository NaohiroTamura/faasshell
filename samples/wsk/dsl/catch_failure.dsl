asl([task('CreateAccount',"frn:wsk:functions:::function:hello",
          [fallback([case(error_equals(["CustomError"]),
                          [pass('CustomErrorFallback','{"Result":"This is a fallback from a custom Lambda function exception"}')]),
                     case(error_equals(["States.TaskFailed"]),
                          [pass('ReservedTypeFallback','{"Result":"This is a fallback from a reserved error code"}')]),
                     case(error_equals(["States.ALL"]),
                          [pass('CatchAllFallback','{"Result":"This is a fallback from any error code"}')])])])]).
