# Zeta #

An [Erlang](http://erlang.org) client for [Riemann](http://aphyr.github.com/riemann/).

# Quickstart #

By default Zeta is configured to sent to a Riemann server at
`127.0.0.1:5555`, so fix that in your global configuration

```erlang
{zeta, {clients, [{default, {{127,0,0,1}, 5555, undefined}}]}}
```

then include and start the Zeta application
```erlang
10> application:start(zeta).
ok
11> zeta:ev(hi, 0.0).
#zeta_event{service = "hi",metric_f = 0.0, ...}
12> zeta:ev({hostname, [with, a, host]}, 1.0).
#zeta_event{service = "with a host",host = "hostname",
            metric_f = 1.0, ...}
13> zeta:evh([default, host], 2.0).
#zeta_event{service = "default host",host = "nonode@nohost",
            metric_f = 2.0, ...}
14> zeta:svh([send, metric], 3.0).
ok
15> zeta:cvh([send, metric, ignore, response], 4.0).
ok
```

Feel free to instrument your code even if the app isn't running. The
messages will just be ignored.

# To do #

- Decide how to handle TCP/UDP methods.
- Make UDP automatically upgrade to TCP if the packet is too large
- Allow for multiple client endpoints
- Make the interface a bit nicer?

# Why "Zeta" #

Possibly the most important unproven conjecture in number theory is
the
[Riemann Hypothesis](http://en.wikipedia.org/wiki/Riemann_hypothesis)
which is concerned with the zeros of the complex Riemann *Zeta*
function. There are also the "Riemann Sums" of great importance in
analysis, but honestly "zeta" is a cooler name.
