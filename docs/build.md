# Build

## Set Environment Variables

- For those who are behind Proxy Server

  ```sh
  $ export HTTP_PROXY="http://id:pw@proxy.example.com:8080"
  $ export HTTPS_PROXY="https://id:pw@proxy.example.com:8433"
  $ export NO_PROXY="localhost,127.0.0.1,0.0.0.0,172.17.0.1"
  ```

## Build Image

- Install [Source-To-Image (S2I)](https://github.com/openshift/source-to-image/releases)
- Build builder image
  ```sh
  $ make build_image
  ```
- Build application image
  ```sh
  $ make -e docker_image_prefix=YOUR_PREFIX app_image
  ```
