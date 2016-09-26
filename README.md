Abstract Stream Recording
=========================

Overview
--------

This is very simple Erlang process messages recorder.
We do simple flush with [append,write] of accumulated messages in process queue.
This is future KVS backend for blockchain and transactional applications.

Features
--------

* Binary in-process accumulation
* Process restarting on flush for stable memory and I/O usage
* VBS -- variative buffer size for higher speeds
* Hashing included in tests
* OTP-less, pure Erlang code
* HTTP API

Run
---

```
> writer:observe(3).
whereis writer: <0.254.0>
whereis writer: <0.265.0>
whereis writer: <0.273.0>
<0.255.0>: writer1: rate 145 MB/s messages 9966 in 1/1 sec
[ok,ok,ok]
<0.266.0>: writer2: rate 113 MB/s messages 9966 in 1/1 sec
<0.274.0>: writer3: rate 114 MB/s messages 9966 in 1/1 sec
<0.299.0>: writer2: rate 95 MB/s messages 22469 in 2/3 sec
<0.308.0>: writer3: rate 92 MB/s messages 22648 in 2/3 sec
<0.284.0>: writer1: rate 92 MB/s messages 28800 in 3/4 sec
<0.375.0>: writer2: rate 112 MB/s messages 19002 in 2/5 sec
<0.385.0>: writer3: rate 100 MB/s messages 18288 in 2/5 sec
<0.386.0>: writer1: rate 96 MB/s messages 18281 in 2/6 sec
<0.429.0>: writer2: rate 107 MB/s messages 22355 in 2/7 sec
<0.455.0>: writer1: rate 98 MB/s messages 19043 in 2/8 sec
<0.448.0>: writer3: rate 95 MB/s messages 19889 in 2/7 sec
<0.507.0>: writer2: rate 106 MB/s messages 21413 in 2/9 sec
<0.527.0>: writer3: rate 104 MB/s messages 18835 in 2/9 sec
```

<img src="https://pbs.twimg.com/media/CtToV9MWIAEhn1s.jpg:large">

Parameters
----------

### append = async | sync.

```
> application:set_env(streams,append,async).
```

### signature = sha512 | sha256 | ripemd160 | none.

```
> application:set_env(streams,signature,sha512).
```

Credits
--------

* Maxim Sokhatsky

OM A HUM
