# Chat

Demo project for evaluation. __Chat__ is an Erlang/OTP app that acts as a chat server.

## Overview

- When started, server accepts multiple connections over TCP from named users.
- The project contains Terraform deployment configuration with NLB and ASG with 2 instances minimum, and an instance template.
- The chat service is checkedout and built on instances at boot, then started as a service.

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

## Deployment on AWS

The folder /terraform contains configuration files for creating the infrastructure in AWS. Use `terraform apply/destroy` commands to create and teardown resources. Pre-required are local installs of Terraform and aws-cli, together with access to AWS. The LB DNS is printed in console at the end of `terraform apply`.

All resources are tagged with `Project=service_chat` to ease their identification in AWS. The file `terraform.tfvars` contains several options that can customize the deployment - region, application port etc.

    $ terraform apply
    [...]
    Apply complete! Resources: 6 added, 0 changed, 0 destroyed.
    Outputs:

    nlb_dns_name = "Public access DNS: chat-nlb-78a7692ad6078424.elb.eu-central-1.amazonaws.com"
    $ telnet chat-nlb-78a7692ad6078424.elb.eu-central-1.amazonaws.com 8080
    [...]
    Login:one
    Welcome, one.
    ...

## Remarks

Known shortcomings of the current stage, to be addressed in next steps.

- The app doesn't handle well parallel connections from the same user.
- Logging is crude.
- There are no tests.
- The app is not ready to work with multiple instances sharing the same state.
- Known issue: when automatically deployed with `terraform apply`, the service fails to start correctly. The workaround is to manually start the app on the instances. 

    ubuntu@ip-172-31-41-241:~$ sudo systemctl stop chat
    ubuntu@ip-172-31-41-241:~$ sudo systemctl disable chat
    ubuntu@ip-172-31-41-241:~$ /home/ubuntu/chat/_build/default/rel/chat/bin/chat daemon
    [...]
    telnet chat-nlb-1c25346c40b654b7.elb.eu-central-1.amazonaws.com 8080
    [...]
    Login:three
    Welcome, three.