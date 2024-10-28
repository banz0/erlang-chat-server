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


## Release

```bash

$ rebar3 release

```


## Use

Once it's running, you can connect to the server using `telnet` or any other TCP client.
The client commands you can use and server messages you'll receive are listed below.


### Client commands

`CONNECT:<nick>`: Connect with the server and register your username.

`SAY:<msg>`: Broadcast a message to every user.

`CREATE:<room>`: Create a new chat room.

`LIST:`: List all available chat rooms.

`JOIN:<room>`: Join a given chat room.

`LEAVE:<room>`: Leave a previously joined chat room. Not possible for the creator of the room.

`DESTROY:<room>`: Destroy a chat room. Only possible for the creator of the room.


### Server messages

`CONNECT:OK:<nick1>:<nick2>:(...):<nickN>`: Positive response to the CONNECT command. Will also return the list of currently connected users.

`CONNECT:ERROR:<reason>`: Negative response to the CONNECT command.

`CREATE:OK:<room1>:<room2>:(...):<roomN>`: Positive response to the CREATE command. Will also return the updated list of rooms.

`CREATE:ERROR:<reason>`: Negative response to the CREATE command.

`LIST:OK:<room1>:<room2>:(...):<roomN>`: Positive response to the LIST command. Will return the list of currently available rooms.

`LIST:No rooms available`: no chat rooms currently available.

`JOIN:OK:<room1>:<room2>:(...):<roomN>`: Positive response to the JOIN command. Will also return the list of user currently connected to the room.

`JOIN:ERROR:<reason>`: Negative response to the JOIN command.

`LEAVE:OK:<room1>:<room2>:(...):<roomN>`: Positive response to the LEAVE command. Will also return the list of user left in the room.

`LEAVE:ERROR:<reason>`: Negative response to the LEAVE command.

`DESTROY:OK:<room1>:<room2>:(...):<roomN>`: Positive response to the DESTROY command. Will also return the list of remaining rooms.

`DESTROY:ERROR:<reason>`: Negative response to the DESTROY command.

`ROOM:<room>:USER:<nick>:SAID:<message>`: Received if a user sends a message to any room the user is also in.

`ROOM:<room>:CREATED`: Received whenever a new room is created.

`ROOM:<room>:DESTROYED`: Received whenever a room is destroyed.

`ROOM:<room>:USER:<nick>:SAID:<message>`: Received whenever a message is sent to any room the user is also in.

`ROOM:<room>:USER:<nick>:JOINED`: Received whenever a new member joins a room the user is also in.

`ROOM:<room>:USER:<nick>:LEFT`: Received whenever a member leaves a room the user is also in.


### Example telnet session

```bash

$ telnet localhost 4000
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
CONNECT:foo
CONNECT:OK:foo
LIST:
LIST:ok:bar
JOIN:bar
SAY:bar:hello and goodbye
LEAVE:bar

```


## References

References used during development:
- [luisgabriel/erl-chat-server](https://github.com/luisgabriel/erl-chat-server)
- [Erlang: A Generic Server Tutorial | 20bits](http://20bits.com/article/erlang-a-generic-server-tutorial)
- [Erlang: A Generalized TCP Server | 20bits](http://20bits.com/article/erlang-a-generalized-tcp-server)
- [Learn You Some Erlang for Great Good!](https://learnyousomeerlang.com/)
