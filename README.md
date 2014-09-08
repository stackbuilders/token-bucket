[![Build Status](https://travis-ci.org/stackbuilders/token-bucket.svg)](https://travis-ci.org/stackbuilders/token-bucket)

# Token Bucket Server

In many applications you will want to throttle connections to an
external integration. One common way to do so is with a
[token bucket algorithm](http://en.wikipedia.org/wiki/Token_bucket).

This application is a TCP server which follows the token bucket
algorithm. By connecting to this server, clients can easily follow
rate limiting rules according to a predefined acceptable number of
queries per second.

A single server instance can be used for multiple "buckets."

# Required Resources

When using a single server for multiple buckets, generally the server
should serve out tokens very close to the acceptable rate of queries
per second. You should make sure that the server that you run the
token bucket server on has a number of cores somewhere near the number
of buckets that you define to ensure that it can keep up with
refilling the token buckets at the correct rate.

If you are running the token bucket server on the same machine that is
doing other processing work you should have a number of cores that
allows for the processing work plus the token bucket server.

# Benchmarks

Testing shows that client applications should be held very near the
desired rate. However short samples could lead to results over the
desired rate, and when machines are overloaded the actual rate will
likely be lower than the specified queries per second.

From a test script included with the application, the rate below was
achieved when running the test application and the token bucket server
on the same machine:

## Trial run with 2 threads doing 100000 requests each and a 0.001 sleep interval.

|       name |       time |    granted |       rate |       goal |      dev % |
|        --- |        --- |        --- |        --- |        --- |        --- |
|    bucketa |     133.11 |        134 |       1.01 |          1 |       0.67 |
|    bucketb |     131.45 |      13039 |      99.20 |        100 |      -0.80 |
|    bucketc |     131.32 |      61836 |     470.88 |        500 |      -5.82 |

### Benchmark Results Explained

The deviation for the 1 QPS bucket above is acceptable, since a token
was added to the bucket by the server in the last partial second of
querying.

It is likely that the test machine couldn't keep up with the demand at
the higher bucket levels, so we see deviation increasing as the QPS
rate increases to 500 QPS. This may be mitigated by separating out the
requesting client from the server machine, or running on a machine
with more cores if this QPS level is needed.

# Usage

Create a configuration file with each line containing a bucket name a
space, and then the number of operations per second that is acceptable
for each bucket. Buckets can be named using any non-space character.

    bucket_a  1
    bucket_b  100

Run the server, specifying the port and path to the configuration file:

    cabal run 4444 /path/to/config_filename

# Protocol

Clients should connect to the server using TCP. The server responds to
the commands `get bucket_name`, where `bucket_name` is the name of a
bucket specified in the configuration, and `quit` which ends the TCP
session.

## `get bucket_name`

Tries to get a token from the given `bucket_name`.

The value `bucket_name` should be one of the buckets in your configuration.

The `get bucket_name` command returns the following values:

* `1` - A token was successfully obtained.
* `0` - The given bucket currently has no tokens.
* `BUCKET NOT FOUND` - the bucket name was not found in the
  configuration.

## `quit`

Ends the current session. Returns `BYE`.

# License

MIT, see [the LICENSE file](LICENSE).
