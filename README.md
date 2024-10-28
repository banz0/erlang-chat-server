# Erlang Chat Server

This is a simple chat server implemented in Erlang/OTP 25.
It allows multiple clients to connect via TCP and chat with each other in real-time.

## Build

```bash

$ rebar3 compile

```


## Run

```bash

$ rebar3 shell --apps chat_server

```


## Usage

Once it's running, you can connect to the server using `telnet` or any other TCP client.
The client commands you can use and server messages you'll receive are listed below.


### Client commands

`CONNECT:<nick>`: Connect with the server and register your username.

`SAY:<msg>`: Broadcast a message to every user.


### Server messages

`CONNECT:OK:<nick1>:<nick2>:(...):<nickN>`: Positive response to the CONNECT command. Will also return the list of currently connected users.

`CONNECT:ERROR:<reason>`: Negative response to the CONNECT command.


### Example telnet session

```bash

$ telnet localhost 4000
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
CONNECT:foo
CONNECT:OK:foo
SAY:hello, world

```


## Release

```bash

$ rebar3 release

```


## References

References used during development:
- [luisgabriel/erl-chat-server](https://github.com/luisgabriel/erl-chat-server)
- [Erlang: A Generic Server Tutorial | 20bits](http://20bits.com/article/erlang-a-generic-server-tutorial)
- [Erlang: A Generalized TCP Server | 20bits](http://20bits.com/article/erlang-a-generalized-tcp-server)
- [Learn You Some Erlang for Great Good!](https://learnyousomeerlang.com/)
