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
whereis writer: <0.239.0>
whereis writer: <0.249.0>
<0.261.0>: writer1: rate 192 MB/s messages 9966 in 1/1 sec
whereis writer: <0.263.0>
[ok,ok,ok]
<0.281.0>: writer2: rate 135 MB/s messages 9966 in 1/1 sec
<0.295.0>: writer3: rate 121 MB/s messages 9966 in 1/1 sec
<0.319.0>: writer1: rate 129 MB/s messages 19091 in 1/2 sec
<0.324.0>: writer2: rate 132 MB/s messages 13431 in 1/2 sec
<0.341.0>: writer3: rate 108 MB/s messages 12065 in 1/2 sec
<0.367.0>: writer2: rate 119 MB/s messages 13130 in 1/3 sec
<0.370.0>: writer1: rate 109 MB/s messages 12840 in 1/3 sec
<0.381.0>: writer3: rate 127 MB/s messages 10812 in 1/3 sec
<0.411.0>: writer1: rate 115 MB/s messages 10855 in 1/4 sec
```

<img src="https://pbs.twimg.com/media/CtTySlyWYAAThxo.jpg:large">

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
