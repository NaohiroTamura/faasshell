asl([task('CreateAccount',"wsk:hello",
          [fallback([case('ErrorEquals'(["CustomError"]),
                          [pass('CustomErrorFallback','{"Result":"This is a fallback from a custom Lambda function exception"}')]),
                     case('ErrorEquals'(["States.TaskFailed"]),
                          [pass('ReservedTypeFallback','{"Result":"This is a fallback from a reserved error code"}')]),
                     case('ErrorEquals'(["States.ALL"]),
                          [pass('CatchAllFallback','{"Result":"This is a fallback from any error code"}')])])])]).
