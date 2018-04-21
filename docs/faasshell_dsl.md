# FaaS Shell DSL

## Overview

FaaS Shell DSL is internal DSL hosted on Prolog. Basically the syntax is same as Prolog.

## Basic Features

    ,
        command separator

    .
        end of command

    'sequence of character'
    constant
        atom, start with lower case letter or any sequence of characters put between single quotes

    "string"
        string

    [a,b,c]
        list

    _{key: "value"}
        dictionary

    {command1, command2}
        block of commands

    a(b,c)
        command

    $.a[0:2]
        input_path, result_path, or output_path

## Variable Commands

    Var
        local variable start with lower case letter
        it works as local variable reference if it is right hand side of =,
        or local variable evaluation if it is used alone

    A = Var
    A = $var
        local variable substitution

    set(var, Value)
        global variable substitution

    $var
        global variable reference

    #var
        global variable evaluation

    unset(var)
        delete global variable

    unsetall
        delete all of global variables

    getall
        display all of global variables


## State Commands

    startsm(input)
        input : dictionary

    pass(state_name, options)
        state_name : atom
        options : [
            input_path(Path),
            output_path(Path),
            result(Value),
            result_path(Path)
        ]

    task(state_name, frn, options)
        state_name : atom
        frn : function resource name
        options : [
            input_path(Path),
            output_path(Path),
            result_path(Path),
            timeout_seconds(TimeoutSeconds),
            heartbeat_seconds(HeartbeatSeconds),
            catch([case(error_equals(["CustomError"]),[Commands]),
                   ...
                  ]),
            retry([case(error_equals([ErrorCode]),
                        [interval_seconds(N),max_attempts(N),backoff_rate(N)]),
                    ...
                  ])
        ]

    choices(state_name, cases, options)
        state_name : atom
        cases : [
            case(ChoiceCondition, [Commands]),
            ...
        ]
        options : [
            input_path(Path),
            output_path(Path),
            default([Commands])
        ]

    wait(state_name, wait_condition, options)
        state_name : atom
        wait_condition: seconds(N) | timestamp(T) | timestamp_path(Path) | seconds_path(Path)
        options : [
            input_path(Path),
            output_path(Path)
        ]

    succeed(state_name)
        state_name : atom

    fail(state_name, options)
        state_name : atom
        options : [
            input_path(Path),
            output_path(Path),
            error(Error),
            cause(Cause)
        ]

    parallel(state_name, branches, options)
        state_name : atom
        branches : [ [Commands],
                     ...
        ]
        options : [
            input_path(Path),
            output_path(Path),
            result_path(Path),
            catch([case(error_equals([ErrorCodes]),[Commands]),
                   ...
                  ]),
            retry([case(error_equals([ErrorCodes]),
                        [interval_seconds(N),max_attempts(N),backoff_rate(N)]),
                    ...
                  ])
        ]

    event(state_name, options)
        state_name : atom
        options : [
            input_path(Path),
            output_path(Path),
            timeout_seconds(TimeoutSeconds)
        ]

    goto(state(state_name))
        state_name : atom, local variable evaluation, or global variable evaluation


## Choice Condition

    boolean_equals(InputPath, Value)

    numeric_equals(InputPath, Value)

    numeric_greater_than(InputPath, Value)

    numeric_greater_than_equals(InputPath, Value)

    numeric_less_than(InputPath, Value)

    numeric_less_than_equals(InputPath, Value)

    string_equals(InputPath, Value)

    string_greater_than(InputPath, Value)

    string_greater_than_equals(InputPath, Value)

    string_less_than(InputPath, Value)

    string_less_than_equals(InputPath, Value)

    timestamp_equals(InputPath, Value)

    timestamp_greater_than(InputPath, Value)

    timestamp_greater_than_equals(InputPath, Value)

    timestamp_less_than(InputPath, Value)

    timestamp_less_than_equals(InputPath, Value)
