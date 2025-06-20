# Chat

Demo project for evaluation. __Chat__ is an Erlang/OTP app that acts as a chat server.

## Overview

- When started, server accepts multiple connections over TCP from named users.

## Setup

### Prerequisites
The project relies on Rebar3 and Erlang/OTP 25 already being present in your system.

### Checkout and build

    $ git clone https://github.com/stefan-rogin/chat.git
    $ cd chat
    $ rebar3 compile
    $ rebar3 release
    [...]
    ===> Release successfully assembled: _build/default/rel/chat

### Run

#### Step 1: Start server

    $ ./_build/default/rel/chat/bin/chat console
    Chat server listening on port 8080
    1>
 
 Console will show output from the application, such as connected and disconnected users:

    User connected: one
    User connected: two

#### Step 2: Connect from clients

Use telnet to connect to the server. It's possible to use multiple terminals at the same time, for parallel connections. At login, the chat server expects a username-like entry, closing the connection otherwise.

    $ telnet localhost 8080
    [...]
    Login:one
    Welcome, one.
    Type /help to see available commands.
    /root
    Command not known, type /help to see available commands.
    /help
    Available commands: /users, /help
    /users
    Online users: one

Open a second connection from a different terminal.

    $ telnet localhost 8080
    Login:two
    Welcome, two.
    Type /help to see available commands.
    /users
    Online users: one, two

## Remarks

Known shortcomings of the current stage, to be addressed in next steps.

- The app doesn't handle well parallel connections from the same user.
- Logging is crude.
- There are no tests.