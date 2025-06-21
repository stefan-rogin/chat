variable "region" {
  type    = string
  default = "eu-central-1"
}

variable "vpc_id" {
  type = string
}

variable "allowed_ports" {
  type    = list(number)
  default = [22, 8080]
}

variable "instance_ami" {
  type = string
}

variable "instance_type" {
  type    = string
  default = "t2.micro"
}

variable "key_name" {
  type = string
}

variable "project_tag" {
  type    = string
  default = "service_chat"
}