# Chat

Demo project for evaluation. __Chat__ is an Erlang/OTP app that acts as a chat server.

## Overview

- When started, server accepts multiple connections over TCP from named users.
- Once connected, users can create, destroy, join and leave rooms. 
- Users can send messages to rooms, received by all room members.
- The project contains Terraform deployment configuration with NLB and ASG, security group, and an instance template.

```
Available commands:
/rooms, /create <Room>, /join <Room>, /leave, /destroy <Room>
/users, /quit, /help
Join a room to send messages to its members.
```

## Project structure

- The project foundation is a rebar3 application.
- There are two gen_server modules:
    - `chat_server`: Controls chat server state. Actions are delegated to `handlers/server_handler` module.
    - `chat_user`: Spawned for each user, they are responsible for the interface between users and server. The messages exchange is handled by `handlers/user_handler`.

## Setup

### Prerequisites
The project relies on Rebar3 and Erlang/OTP 25 already being present in your system.

### Checkout and build

    $ git clone https://github.com/stefan-rogin/chat.git
    $ cd chat
    $ rebar3 release
    [...]
    ===> Release successfully assembled: _build/default/rel/chat

### Test

You can run tests with `rebar3 eunit`, or with `rebar3 eunit && rebar3 cover --verbose` to include the coverage report in the console.

### Run

#### Step 1: Start server

    $ ./_build/default/rel/chat/bin/chat console
    Chat server listening on port 8080
    1>
 
 Console will show output from the application, such as connected and disconnected users:

    User connected: one
    User connected: two

#### Step 2: Connect from clients

Use `telnet` or `nc` to connect to the server from multiple terminals, to impersonate different users. At login, the chat server expects a username-like entry, closing the connection otherwise.

| **User one**                                | **User two**                                  |
|---------------------------------------------|-----------------------------------------------|
| `$ telnet localhost 8080`                   | `$ telnet localhost 8080`                     |
| `[...]`                                     | `[...]`                                       |
| `Login:one`                                 | `Login:two`                                   |
| `Welcome, one.`                             | `Welcome, two.`                               |
| `Type /help to see available commands.`     | `Type /help to see available commands.`       |
| `/create Planes`                            |                                               |
| `Room Planes created.`                      |                                               |
|                                             | `/users`                                      |
|                                             | `Online users: one, two`                      |
|                                             | `/rooms`                                      |
|                                             | `Rooms: Planes`                               |
|                                             | `/join Planes`                                |
|                                             | `[*]: User two joined Planes.`                |
| `/join Planes`                              |                                               |
| `[*]: User one joined Planes.`              | `[*]: User one joined Planes.`                |
| `[two]: hi`                                 | `hi`                                          |
| `hi! i can't believe it's working!`         | `[one]: hi! i can't believe it's working!`    |
| `[two]: i know!`                            | `i know!`                                     |
| `bye now`                                   | `[one]: bye now`                              |
| `/leave`                                    |                                               |
| `You left room Planes.`                     | `[*]: User one left Planes.`                  |
| `/quit`                                     |                                               |
| `Bye.`                                      |                                               |
|                                             | `/users`                                      |
|                                             | `Online users: two`                           |


## Deployment on AWS

The folder /terraform contains configuration files for creating the infrastructure in AWS. Use `terraform apply/destroy` commands to create and teardown resources. Pre-required are local installs of Terraform and aws-cli, together with access to AWS. The LB DNS is printed in console at the end of `terraform apply`.

All resources are tagged with `Project=service_chat` to ease their identification in AWS. The file `terraform.tfvars` contains several options that can customize the deployment - region, application port etc.

```
$ terraform apply
[...]
Apply complete! Resources: 6 added, 0 changed, 0destroyed.
Outputs:
nlb_dns_name = "Public access DNS:chat-nlb-78a7692ad6078424.elb.eu-central-1.amazonawscom"
$ telnet chat-nlb-78a7692ad6078424.elb.eu-central-1amazonaws.com 8080
[...]
Login:one
Welcome, one.
...
```

## Remarks

Known shortcomings of the current stage.

- Logging is crude.
- Tests are few, limited to use cases not involving mocking `gen_tcp` or `chat_server` (for `user_handler` tests).
- The app is not ready to work with multiple instances sharing the same state.
- Known issue: when automatically deployed with `terraform apply`, the service fails to start correctly. The workaround is to manually start the app on the instances. 

```
ubuntu@ip-172-31-41-241:~$ sudo systemctl stop chat
ubuntu@ip-172-31-41-241:~$ sudo systemctl disable chat
ubuntu@ip-172-31-41-241:~$ /home/ubuntu/chat/_builddefault/rel/chat/bin/chat daemon
[...]
telnet chat-nlb-1c25346c40b654b7.elb.eu-central-1amazonaws.com 8080
[...]
Login:three
Welcome, three.
```