# Demo

## IFTTT as FaaS in REPL

### Prepare Functions

* IFTTT "save_result" Applet

  Follow the steps in the document, [samples/ifttt/applet/save_result.md](./ifttt/applet/save_result.md).

* IBM Cloud Functions / Apache OpenWhisk "hello" Action

  ```sh
  $ wsk action create hello samples/wsk/actions/hello.js -i
  ```

### Set Environment Variables

```sh
$ export WSK_AUTH=XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX:YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY
$ export WSK_APIHOST=FQDN_OR_IP

$ export IFTTT_KEY=ZZZZZZZZZZZZZZZZZZZZZ
```

If you are in the private network, set the proxy environment variables.

```sh
$ export HTTP_PROXY="http://id:pw@proxy.example.com:8080"
$ export HTTPS_PROXY="https://id:pw@proxy.example.com:8433"
$ export NO_PROXY="localhost,127.0.0.1,0.0.0.0,172.17.0.1"
```

### Start REPL

The command line has to end with the period, '.', full stop.

REPL supports *GNU readline*, you can edit the command line and its history such as bash. The history is saved in ~/.faasshell_history.

REPL console will be implemented in Web Browser later (TODO).

```
$ git clone https://github.com/NaohiroTamura/faasshell

$ cd faasshell

$ src/faasshell_repl.pl
faasshell> help.
----------------------------------------------------------------------
global variable: begin with a lower case letter
local  variable: begin with a upper case letter
JSON   value   : _{key:"string", number:1, list:["a","b"]}
----------------------------------------------------------------------
debug(on)      : display debug message
debug(off)     : suppress debug message
startsm(Input) : start state machine and inject Input data
endsm(Output)  : end state machine and get Output data, it is optional
set(x,y)       : set a value 'y' to a global variable 'x'
unset(x)       : unset a global variable 'x'
unsetall       : unset all global variables
getall         : get all values of the global variables
$x             : refer to a value of the global variable 'x'
#x             : evaluate a value of the global variable 'x'
X=Y            : substitute a value 'Y' to the local variable 'X'
----------------------------------------------------------------------
```

### Execute IFTTT as FaaS demo without variables

The DSL of the FaaS Shell runtime for *IFTTT as FaaS demo* is found in [samples/ifttt/dsl/save_result.dsl](./ifttt/dsl/save_result.dsl). DSL commands are enclosed by "fsm([])". 
So just coping the three commands, task(), pass() and another task(), into REPL with the full stop should work as below.

The command line can be divided into multiple lines by enter key.

The prompt '\|' shows that it's continuous line.

```prolog
faasshell> startsm(_{name:"Repl"}),
| task('HelloWorld',"frn:wsk:functions:::function:hello",[]),
| pass('UpdateArg',[result_path('$.ifttt.value1'), input_path('$.payload'), output_path('$.ifttt')]),
| task('SaveResult',"frn:ifttt:webhooks:::function:save_result",[]).

Output=Congratulations! You've fired the save_result event

faasshell>

```

### Execute IFTTT as FaaS demo with local variables

The local valuables start with upper case letter such as "Hello", "UpdateArg", and "SaveResult".

The scope of the local variable is within one command line.

```prolog
faasshell> Hello = task('HelloWorld',"frn:wsk:functions:::function:hello",[]),
| UpdateArg = pass('UpdateArg',[result_path('$.ifttt.value1'),input_path('$.payload'),output_path('$.ifttt')]),
| SaveResult = task('SaveResult',"frn:ifttt:webhooks:::function:save_result",[]),
| startsm(_{name:"Repl"}), Hello, UpdateArg, SaveResult.

Output=Congratulations! You've fired the save_result event
Hello=task(HelloWorld,frn:wsk:functions:::function:hello,[])
UpdateArg=pass(UpdateArg,[result_path($.ifttt.value1),input_path($.payload),output_path($.ifttt)])
SaveResult=task(SaveResult,frn:ifttt:webhooks:::function:save_result,[])

faasshell>
```

### Execute IFTTT as FaaS demo with global variables

The global variables start with lower case letter such as "hello", "update_arg", and "save_result".

The scope of the global variable is within a REPL, it's effective until exiting REPL.

```prolog
faasshell> set(hello, task('HelloWorld',"frn:wsk:functions:::function:hello",[])).
Output=task(HelloWorld,frn:wsk:functions:::function:hello,[])

faasshell> set(update_arg, pass('UpdateArg',[result_path('$.ifttt.value1'),input_path('$.payload'),output_path('$.ifttt')])).
Output=pass(UpdateArg,[result_path($.ifttt.value1),input_path($.payload),output_path($.ifttt)])

faasshell> set(save_result, task('SaveResult',"frn:ifttt:webhooks:::function:save_result",[])).
Output=task(SaveResult,frn:ifttt:webhooks:::function:save_result,[])

faasshell> startsm(_{name:"Repl"}), #hello, #update_arg, #save_result.
Output=Congratulations! You've fired the save_result event

faasshell>
```

### End REPL

Push Control-D twice. The debug prompt should be hidden in production (TODO).

```sh
faasshell> Control-D
faasshell debug> Control-D
```
