# Resource Naming

[Amazon State Language][1] specifies that "Resource field" MUST be a URI.

    A Task State MUST include a “Resource” field, whose value MUST be
    a URI that uniquely identifies the specific task to execute. The
    States language does not constrain the URI scheme nor any other
    part of the URI.

[1]: https://states-language.net/spec.html "Amazon State Language"

## Function

FaaS Shell looks up the following information form the "Resource
field", and dispatch the function invocation to an appropriate FaaS
vendor plug-in.

* Beginning with a lower-case letter indicates literal.
* Beginning with an upper-case letter indicates variable.
* ':' indicates a separator.
* '_' indicates an arbitrary value, that is ignored.


### AWS
```sh
arn:aws:lambda:Region:Account:function:Function
```

### GCP
```sh
frn:gcp:functions:Location:Project:function:Function
```

### Azure
```sh
frn:azure:functions:_:WebappName:function:Function
```

### IBM/OpenWhisk
```sh
frn:wsk:functions:_:_:function:Action
```

### IFTTT
```sh
frn:ifttt:webhooks:_:_:function:Applet
```

## Activity

FaaS Shell judges that it's activity if "Resource field" matches the
following pattern. This is common among FaaS.

```sh
_:_:states:_:_:activity:Activity
```
