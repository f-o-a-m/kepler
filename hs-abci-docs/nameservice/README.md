# Nameservice

The `Nameservice` application is a sample application that showcases the SDK. It roughly follows the example application from the golang cosmos-sdk, which you can find [here](https://github.com/cosmos/sdk-tutorials/tree/master/nameservice).

Not that the app itself is also a [tutorial](./tutorial/README.md)! This tutorial explains in depth how the Nameservice app was built. You're encouraged to read this tutorial after reading this.

## Running the Application

The `Nameservice` application depends on a few external services. We provide a `docker-compose.yaml` file and highly suggest running the application inside Docker. There is a `make deploy-nameservice` command which can be run from the project root to deploy the application.

**NOTE** This will also attempt build the nameservice binaries in Docker, which can take a long time. If you are on (Ubuntu) Linux, you can use the `make docker-test-prebake` command first to build the application locally and copy the binaries to the correct image. If you then run `make deploy-nameservice`, it will automatically use these binaries instead of rebuilding inside Docker.

### Environment Variables

You can provide the following environment variables when running `make deploy-nameservice` to customize the logger output:

- `LOG_SEVERITY` (defaults to **info**): minimum log severtiy level `{debug, info, notice, warning, error, critical, alert, emergency}`
- `LOG_VERBOSITY` (defaults to **0**) : for each loggable data point, the level of information actually logged `{0, 1, 2, 3}`

## [Next: Logging](./docs/Logging.md)
