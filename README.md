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

## Deployment

The folder /terraform contains configuration files for creating the infrastructure in AWS. Use `terraform apply/destroy` commands to create and teardown resources. Pre-required are local installs of Terraform and aws-cli, together with access to AWS including key pairs for the project's instance(s). 

A basic `deploy.sh` script can be used at this stage to automatically upload a local build artefact to the EC2 instance. Terraform outputs IP and DNS of created instances, to use with deploy.sh or to connect to the running service. The script requires an environement variable `SERVICE_CHAT_AWS_KEY_PATH` with the path to the key pair file. 

    $ terraform apply
    [...]
    Apply complete! Resources: 2 added, 0 changed, 0 destroyed.
    Outputs:

    instance_public_dns = "ec2-18-199-171-238.eu-central-1.compute.amazonaws.com"
    instance_public_ip = "18.199.171.238"
    $ ./deploy.sh 18.199.171.238
    [...]
    Done.
    $ telnet 18.199.171.238 8080
    [...]
    Login:one
    Welcome, one.
    ...

## Remarks

Known shortcomings of the current stage, to be addressed in next steps.

- The app doesn't handle well parallel connections from the same user.
- Logging is crude.
- There are no tests.
- A less cumbersome Github Actions deployment setup is in progress.