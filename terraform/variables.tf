variable "region" {
  type    = string
  default = "eu-central-1"
}

variable "service_port" {
  type    = number
  default = 4000
}

variable "instance_type" {
  type    = string
  default = "t2.micro"
}

variable "project_tag" {
  type    = string
  default = "service_chat"
}