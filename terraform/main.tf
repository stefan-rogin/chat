provider "aws" {
  region = var.region
}

data "aws_ami" "ubuntu_latest" {
  most_recent = true

  filter {
    name   = "name"
    values = ["ubuntu/images/hvm-ssd/ubuntu-jammy-22.04-amd64-server-*"]
  }

  filter {
    name   = "virtualization-type"
    values = ["hvm"]
  }

  owners = ["ubuntu"]
}

resource "aws_security_group" "service_chat_sg" {
  name   = "service-chat-sg"
  vpc_id = var.vpc_id

  dynamic "ingress" {
    for_each = var.allowed_ports
    content {
      from_port   = ingress.value
      to_port     = ingress.value
      protocol    = "tcp"
      cidr_blocks = ["0.0.0.0/0"]
    }
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = {
    Name    = "service_chat_sg"
    Project = var.project_tag
  }
}

resource "aws_instance" "service_chat" {
  ami                    = data.aws_ami.ubuntu_latest.id
  instance_type          = var.instance_type
  key_name               = var.key_name
  vpc_security_group_ids = [aws_security_group.service_chat_sg.id]

  user_data = <<-EOF
    #!/bin/bash
    sudo apt update
    sudo apt install -y erlang
  EOF

  tags = {
    Name    = "service_chat"
    Project = var.project_tag
  }
}

output "instance_public_ip" {
  value = aws_instance.service_chat.public_ip
}

output "instance_public_dns" {
  value = aws_instance.service_chat.public_dns
}