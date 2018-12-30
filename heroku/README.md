# Heroku deployment steps

## Initial Install

```sh
ubuntu@xenial:~/faasshell/heroku[master]$ heroku login

ubuntu@xenial:~/faasshell/heroku[master]$ heroku container:login

ubuntu@xenial:~/faasshell/heroku[master]$ heroku create
Creating app... done, ⬢ protected-depths-49487
https://protected-depths-49487.herokuapp.com/ | https://git.heroku.com/protected-depths-49487.git

ubuntu@xenial:~/faasshell/heroku[master]$ ./faasshell.config protected-depths-49487

ubuntu@xenial:~/faasshell/heroku[master]$ heroku container:push web --app protected-depths-49487

ubuntu@xenial:~/faasshell/heroku[master]$ heroku container:release web --app protected-depths-49487

ubuntu@xenial:~/faasshell/heroku[master]$ curl https://protected-depths-49487.herokuapp.com/ -u $DEMO
{"version":"$Id rev.2018-07-08.2e2c6c4 $"}

ubuntu@xenial:~/faasshell/heroku[master]$ heroku logs --app protected-depths-49487 -t
```

## Update Install

```sh
ubuntu@xenial:~/faasshell/heroku[master]$ heroku login

ubuntu@xenial:~/faasshell/heroku[master]$ heroku container:login

ubuntu@xenial:~/faasshell/heroku[master]$ heroku container:push web --app protected-depths-49487

ubuntu@xenial:~/faasshell/heroku[master]$ heroku container:release web --app protected-depths-49487

ubuntu@xenial:~/faasshell/heroku[master]$ curl https://protected-depths-49487.herokuapp.com/ -u $DEMO
{"version":"$Id rev.2018-11-16.1a84b0a $"}
```
