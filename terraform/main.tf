provider "aws" {
  region = var.region
}

# Security Group
resource "aws_security_group" "service_chat_sg" {
  name   = "service-chat-sg"
  vpc_id = var.vpc_id

  ingress {
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ingress {
    from_port   = var.service_port
    to_port     = var.service_port
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
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

# Instance template
resource "aws_launch_template" "chat_lt" {
  name_prefix            = "chat-lt-"
  image_id               = data.aws_ami.ubuntu_latest.id
  instance_type          = var.instance_type
  key_name               = var.key_name
  update_default_version = true
  vpc_security_group_ids = [aws_security_group.service_chat_sg.id]

  user_data = base64encode(<<-EOF
#!/bin/bash
set -e

sudo apt update
sudo apt install -y erlang rebar3

sudo -u ubuntu bash <<EOT
cd /home/ubuntu
git clone https://github.com/stefan-rogin/chat.git
cd chat
rebar3 release
_build/default/rel/chat/bin/chat daemon
EOT
EOF
  )


  tag_specifications {
    resource_type = "instance"
    tags = {
      Name    = "chat-instance"
      Project = var.project_tag
    }
  }
}

# Network Load Balancer
resource "aws_lb" "chat_nlb" {
  name               = "chat-nlb"
  internal           = false
  load_balancer_type = "network"
  subnets            = [aws_subnet.public_a.id, aws_subnet.public_b.id]

  tags = {
    Name    = "chat-nlb"
    Project = var.project_tag
  }
}

# Target Group
resource "aws_lb_target_group" "chat_tg" {
  name     = "chat-tg"
  port     = var.service_port
  protocol = "TCP"
  vpc_id   = var.vpc_id

  health_check {
    protocol            = "TCP"
    port                = var.service_port
    healthy_threshold   = 2
    unhealthy_threshold = 2
    interval            = 10
    timeout             = 5
  }

  tags = {
    Name    = "chat-tg"
    Project = var.project_tag
  }
}

# ALB Listener
resource "aws_lb_listener" "chat_listener" {
  load_balancer_arn = aws_lb.chat_nlb.arn
  port              = var.service_port
  protocol          = "TCP"

  default_action {
    type             = "forward"
    target_group_arn = aws_lb_target_group.chat_tg.arn
  }

  tags = {
    Project = var.project_tag
  }
}

# Auto Scaling Group
resource "aws_autoscaling_group" "chat_asg" {
  name             = "chat-asg"
  max_size         = 4
  min_size         = 2
  desired_capacity = 2
  vpc_zone_identifier = [
    aws_subnet.public_a.id,
    aws_subnet.public_b.id
  ]
  target_group_arns         = [aws_lb_target_group.chat_tg.arn]
  health_check_type         = "EC2"
  health_check_grace_period = 60

  launch_template {
    id      = aws_launch_template.chat_lt.id
    version = "$Latest"
  }

  tag {
    key                 = "Name"
    value               = "chat-asg-instance"
    propagate_at_launch = true
  }

  tag {
    key                 = "Project"
    value               = var.project_tag
    propagate_at_launch = true
  }
}

#Subnets
resource "aws_subnet" "public_a" {
  vpc_id                  = var.vpc_id
  cidr_block              = "172.31.48.0/24"
  availability_zone       = "eu-central-1a"
  map_public_ip_on_launch = true

  tags = {
    Name    = "public-subnet-a"
    Project = var.project_tag
  }
}

resource "aws_subnet" "public_b" {
  vpc_id                  = var.vpc_id
  cidr_block              = "172.31.49.0/24"
  availability_zone       = "eu-central-1b"
  map_public_ip_on_launch = true

  tags = {
    Name    = "public-subnet-b"
    Project = var.project_tag
  }
}

# AMI
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
  owners = ["099720109477"]
}

output "nlb_dns_name" {
  value = "Public access DNS: ${aws_lb.chat_nlb.dns_name}"
}
