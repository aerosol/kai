# kai

[KairosDB](http://kairosdb.github.io/) Erlang Client

:warning: **WIP. Do not use yet.**

## Roadmap / planned features:

1. Inserting metrics:
  - pool of raw tcp clients (aka [`telnet API`](http://kairosdb.github.io/kairosdocs/telnetapi/index.html))
  - schedules ELB-friendly "ping", prevents corporate firewalls from killing idle connections
  
2. Retreiving metrics and aggregations:
  - [REST API](http://kairosdb.github.io/kairosdocs/restapi/index.html) wrappers
  - API for building [metrics queries](http://kairosdb.github.io/kairosdocs/restapi/QueryMetrics.html)
  
3. [pre-aggregation](http://kairosdb.github.io/kairosdocs/FAQ.html#why-would-i-pre-aggregate) facility

4. benchmarks / load-testing tools
