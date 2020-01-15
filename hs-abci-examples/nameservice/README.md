# Nameservice

The Nameservice application is a sample application meant to showcase the SDK. It roughly follows the example application from the golang cosmos-sdk, which you can find [here](https://github.com/cosmos/sdk-tutorials/tree/master/nameservice).

There is also a [tutorial](./tutorial/README.md) that explains how the Nameservice app was built.

## Running the Application

The Nameservice application depends on a few services, so we provide a `docker-compose.yaml` file and highly suggest running the application in Docker. There is a `make deploy-nameservice` command which can be run from the project root to deploy the application.

**NOTE** This will also attempt build the nameservice binaries in Docker, which can take a long time. If you are on (ubuntu) linux, you can use the `make docker-test-prebake` command first to build the application locally and copy the binaries to the correct image. If you then run `make deploy-nameservice`, it will automatically use these binaries instead of rebuilding in Docker.


### Environment Variables

You can provide the following environment variables when running `make deploy-nameservice` to customize the logger output:

- LOG_SEVERITY (defaults to **info**) : minimum log severtiy level {debug, info, notice, warning, error, critical, alert, emergency}
- LOG_VERBOSITY (defaults to **0**) : for each loggable data point, the level of information actually logged {0, 1, 2, 3}

## Logging and Metrics

There is a lot to say about how [logging](./docs/Logging.md) and [metrics](./docs/Metrics.md) are managed if you decide to use them.