# Erlang Chat Server

A simple chat server written in Erlang/OTP 25.
It runs on the port 4000 by default.


## Build

```bash

$ rebar3 compile

```


## Run

```bash

$ rebar3 shell --apps chat_server

```


## Test

Once the application is running it can be tested by using (multiple instances of) `telnet`.

```bash

telnet localhost 4000

```

Once connected, type a message and press `Enter` to send it to the server.

``` bash

Hello, world!

```

The server will echo back the message.


### Example telnet session

```bash

$ telnet localhost 4000
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
Hello
Hello

```


## Release

```bash

$ rebar3 release

```


## References

References used during development:
- [Learn You Some Erlang for Great Good!](https://learnyousomeerlang.com/)
- [Erlang: A Generic Server Tutorial | 20bits](http://20bits.com/article/erlang-a-generic-server-tutorial)
- [Erlang: A Generalized TCP Server | 20bits](http://20bits.com/article/erlang-a-generalized-tcp-server)
