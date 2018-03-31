# Development Environment

## Requirements

* ubuntu 14.04 or later
* docker 1.12.0 or later
* git
* perl
* python 2.7 or later
* make
* envsubst
* curl
* [Source-To-Image (S2I)](https://github.com/openshift/source-to-image/releases)
* [Docker Compose](https://docs.docker.com/compose/install/)
* [kubectl](https://kubernetes.io/docs/tasks/tools/install-kubectl/)

## git setup

* git keyword substitution $Id$ is set for "src/faasshell_version.pl"

    ```sh
    $ git clone https://github.com/NaohiroTamura/faasshell

    $ cd faasshell

    $ cat .gitattributes
    faasshell_version.pl filter=keyword
    ```

* type the following commands to make the substitution effective

    ```sh
    $ git config filter.keyword.clean 'perl -pe "s/\\\$Id[^\\\$]*\\\$/\\\$Id\\\$/"'

    $ git config filter.keyword.smudge .gitfilter/keyword.smudge
    ```

* verify that the filters are set successfully

    ```sh
    $ cat .git/config
    ...
    [filter "keyword"]
        clean = perl -pe \"s/\\\\\\$Id[^\\\\\\$]*\\\\\\$/\\\\\\$Id\\\\\\$/\"
        smudge = .gitfilter/keyword.smudge
    ```

## swipl command setup

* create swipl docker image

    ```sh
    $ pushd .circleci/images

    $ docker build -t YOUR_PREFIX/swipl7jpl .

    $ popd
    ```

* setup swipl command

    In terms of the environment variables, please read the reference,
    [docs/environment_variables.md](docs/environment_variables.md).

    ```sh
    $ cp ~/bin/faasshell.env.template ~/bin/faasshell.env

    $ vi ~/bin/faasshell.env
    ...
    export docker_image_prefix=YOUR_PREFIX
    ...

    $ source ~/bin/faasshell.env

    $ export PATH=~/bin:$PATH

    $ swipl
    Welcome to SWI-Prolog (threaded, 64 bits, version 7.5.15)
    SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
    Please run ?- license. for legal details.

    For online help and background, visit http://www.swi-prolog.org
    For built-in help, use ?- help(Topic). or ?- apropos(Word).

    ?-
    ```
