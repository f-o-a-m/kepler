# Metrics

The SDK has some built in support for metrics via [prometheus](https://prometheus.io/), but ultimately you may choose a different runtime interpretation for the metrics, or even choose to ignore it entirely.

The `Nameservice` application uses application specific metrics, for instance increasing the message counters for module level messages, or for timing module responses. It also uses the server metrics via the [metrics middleware](https://github.com/f-o-a-m/hs-abci/blob/master/hs-abci-extra/src/Network/ABCI/Server/Middleware/Metrics.hs) to count ABCI messages and to time server responses. This middleware is highly recommended for any production system.

## Setting up metrics

The `Nameservice` Docker network is configured to run a prometheus metrics server in addition to a Datadog agent that scrapes  and pushes metrics to [datadog](https://www.datadoghq.com/). You must supply a Datadog API key as an environment variable *DD_API_KEY* when you launch the network if you want to do do this. If you don't already have an account, you can [create one](https://www.datadoghq.com/free-datadog-trial/) and receive a two week free trial to play around with this application.

To simply test if prometheus is indeed collecting your metrics, you can visit `http://localhost:5555/metrics` and you should see something. (`5555` is the default value for the `STATS_PORT` environment variable in the docker compose file.)
