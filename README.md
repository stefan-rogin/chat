# Chat

Demo project for evaluation. __Chat__ is an Erlang/OTP app that acts as a chat server.

## Overview

- When started, outputs _Hello_, then shuts down the VM.

## Setup

### Prerequisites
The project relies on Rebar3 and Erlang/OTP 25 already being present in your system.

### Checkout and build

    $ git clone https://github.com/stefan-rogin/chat.git
    $ cd chat
    $ rebar3 compile

### Run

    $ rebar3 release
    [...]
    ===> Release successfully assembled: _build/default/rel/chat
    $ ./_build/default/rel/chat/bin/chat foreground
    [...]
    Hello
