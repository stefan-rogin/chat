variable "region" {
  type    = string
  default = "eu-central-1"
}

variable "service_port" {
  type    = number
  default = 8080
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