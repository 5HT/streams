Erlang Stream Recording
=======================

Overview
--------

This is very simple Erlang process messages recorder.
We do simple flush with [append,write] of accumulated messages in process queue.

Features
--------

* Binary in-process accumulation
* Process restarting on flush for stable memory and I/O usage
* Append writers per up to 100MB chunks
* Hashing included in tests
* OTP-less, pure Erlang code

Run
---

Start `observer` to see I/O and Memory usage in real-time.
NOTE: The streams prevalence of CPU cores causes performance downgrade.

```
> observer:start().
```

Start four streams. Later we will do this under supervision.

```
> writer:test(1).
> writer:test(2).
> writer:test(3).
> writer:test(4).
```

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
