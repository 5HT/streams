Erlang Stream Recording
=======================

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
1> observer:start().
ok

2> writer:test(1).
whereis writer: <0.386.0>
<0.386.0>
Written writer1: rate 185 MB/s messages 6149 in 0 sec
Written writer1: rate 182 MB/s messages 45368 in 2 sec

3> writer:test(2).
whereis writer: <0.499.0>
<0.499.0>
Written writer2: rate 171 MB/s messages 6149 in 0 sec
Written writer1: rate 158 MB/s messages 44676 in 2 sec
Written writer2: rate 156 MB/s messages 42105 in 2 sec
Written writer1: rate 144 MB/s messages 38833 in 2 sec
Written writer2: rate 147 MB/s messages 38278 in 2 sec
Written writer1: rate 147 MB/s messages 35481 in 2 sec
```

<img src="https://pbs.twimg.com/media/CtI_Y6kWAAALgXS.jpg:large">

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

Creadits
--------

* Maxim Sokhatsky

OM A HUM
