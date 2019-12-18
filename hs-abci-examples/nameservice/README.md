# nameservice

## Metrics

Run `nameservice` via the `Makefile`:

```
make deploy-nameservice-local
```

Along with running `nameservice`, this also starts a Prometheus] metrics server.
By default, the metrics server runs on `localhost:9200`. To use a different port, 
set the `STATS_PORT` environment variable to the desired port value.

To see these metrics in Datadog, follow the 
[Prometheus host config instructions](https://docs.datadoghq.com/getting_started/integrations/prometheus/?tab=host#pagetitle)
to configure a local Datadog agent to scrape the endpoint.

Alternatively, use the `docker-compose` command:

```
make deploy-nameservice-docker
```
