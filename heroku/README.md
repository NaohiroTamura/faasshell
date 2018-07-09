# Heroku deployment steps

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

ubuntu@xenial:~/faasshell/heroku[master]$ heroku logs --app protected-depths-49487 -t
```
