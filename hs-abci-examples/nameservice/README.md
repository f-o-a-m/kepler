# nameservice

## Metrics via Prometheus

Run `nameservice` via the `Makefile`:

```
make deploy-nameservice-local
```

Along with running `nameservice`, this also starts a Prometheus metrics server.
By default, the metrics server runs on `localhost:9200`. To use a different port, 
set the `STATS_PORT` environment variable to the desired port value.

To see these metrics in Datadog, follow the 
[Prometheus host config instructions](https://docs.datadoghq.com/getting_started/integrations/prometheus/?tab=host#pagetitle)
to configure a local Datadog agent to scrape the endpoint.

At minimum, to scrape all `nameservice` prometheus metrics, the appropriate `conf.yaml` (described above) 
should be use the following settings:

```yaml
init_config:

instances:

  - prometheus_url: http://localhost:9200/metrics
    ## namespace option prefixes all metric names in datadog
    namespace: nameservice 
    ## metrics names used in the nameservice app
    metrics:
      - count_buy
      - count_set
      - count_delete
      - histogram_buy*
      - histogram_set*
      - histogram_delete*
```

Alternatively, use the `docker-compose` command:

```
make deploy-nameservice-docker
```

Once the `nameservice` server is running, start a Tendermint node:

```bash
> tendermint init
> tendermint node
```

Then run the `nameservice` tests:

```bash
make test-nameservice
```

Once the test run is completed, you should now be able to view metrics
on [Datadog's metrics explorer](https://app.datadoghq.com/metric/explorer).
Firstly, ensure that the prometheus server is populated with metrics by
visiting `localhost:9200` in a browser. You should see something like this:

```
# TYPE count_buy counter
count_buy  4
# TYPE count_delete counter
count_delete  1
# TYPE count_set counter
count_set  2
# TYPE histogram_buy histogram
histogram_buy_bucket{le="1.0e-4"} 0.0
...
# TYPE histogram_delete histogram
histogram_delete_bucket{le="1.0e-4"} 0.0
...
# TYPE histogram_set histogram
histogram_set_bucket{le="1.0e-4"} 0.0
...
```

In Datadog's metrics explorer, you should be able to search for metrics
prefixed with the `namespace` value set in your agent's config.

## Alternative Logging

In addition to Prometheus metrics, the `nameservice` app includes options for
logging to Elasticsearch (ES) and direct to Datadog metrics logging. By default, the app
logs to the console via Katip's context logger:

```
[2019-12-20 16:19:27][nameservice.server][Info][local][PID 89617][ThreadId 21][type:info] Request Received
[2019-12-20 16:19:27][nameservice.server][Info][local][PID 89617][ThreadId 21][message_type:info][response_time:2.6e-5][message_count:1]
```

These logs include request/response info and as well some metric logging.

Alternatively, the app is set up to log the same information to Elastic search
and Datadog by setting the following environment variables:

```bash
DD_API_KEY ## Datadog API key
ES_HOST    ## Elasticsearch server host
ES_PORT    ## Elasticsearch server port
```

We recommend using the [ELK (v683) docker image](https://hub.docker.com/r/sebp/elk/tags)
to run an ES server and Kibana to search and filter incoming logs.

```bash
> docker pull sebp/elk:683
> docker run -p 5601:5601 -p 9201:9200 -p 5044:5044 -it --name elk sebp/elk:683
```

This command remaps the `ES_PORT` value from `9200` to `9201` to avoid collision with
the Prometheus server that `nameservice` runs by default. Kibana runs on port `5601`.

After both Kibana and ES are running, start `nameservice` and a Tendermint node 
via the commands:

```bash
> DD_API_KEY=<DD_API_KEY> ES_HOST=localhost ES_PORT=9201 make deploy-nameservice
> tendermint init
> tendermint node
```

You should no longer see logs on the console. Instead, you should be able to see metrics
logs on [Datadog's log explorer](https://app.datadoghq.com/logs). On Kibana, go to
`Management > Kibana > Index Patterns` and create an index pattern for `nameservice` by
simply typing its name. To view the logs in this index, you should be able to go to
`Machine Learnine > Data Visualization` and select the `nameservice` index pattern you 
just created. You should now see both metrics and some stats on specific `nameservice`
log fields.
