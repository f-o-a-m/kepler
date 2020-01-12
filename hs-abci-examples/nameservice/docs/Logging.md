# Logging

The SDK has built in support for structured logging via the [katip](https://hackage.haskell.org/package/katip) logging library. Even still, the SDK is agnostic to where you want your logs to go. Katip logs are managed by *scribes* whos job is precisely this, so it depends on which scribes you use. The two most common scribes are the [console scribe](https://hackage.haskell.org/package/katip-0.8.3.0/docs/Katip-Scribes-Handle.html#v:mkHandleScribe) and the [Elasticsearch scribe](https://hackage.haskell.org/package/katip-elasticsearch).

The Nameservice application has support for either scribe -- it will use Elasticsearch if you provide the `ES_HOST` and `ES_PORT` environment variables or otherwise will default to console logging. The docker deployment is configured to use Elasticsearch.

## Logging to Elasticsearch

The docker network includes an `elk` image (Elasticsearch, Logstash, Kibana). You can read more about this stack [here](https://www.elastic.co/what-is/elk-stack), but the summary is that it is a powerful solution for hosting searchable structured logs. When logging to Elasticsearch, you can use the Kibana dashboard for creating queries and visualizations. We will cover the basics here.