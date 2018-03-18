# Generating Self Signed Certificate

Type "make cert" in the top directory, otherwise follow the steps below in this directory.

1. create CA

```sh
$ /usr/lib/ssl/misc/CA.pl -newca
```

2. generate a certificate request

```sh
$ /usr/lib/ssl/misc/CA.pl -newreq
```

3. sign the request with the CA's signature

```sh
$ /usr/lib/ssl/misc/CA.pl -signreq
```

4. put them into server directory

```sh
$ mkdir server

$ mv newkey.pem server/server-key.pem

$ mv newcert.pem server/server-cert.pem

$ echo "your_paasword" > server/password

$ chmod 0400 server/*.pem server/password
```
