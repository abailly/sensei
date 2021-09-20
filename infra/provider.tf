provider "google" {
  region = "europe-west1"
  zone = "europe-west1-b"
  project = "pankzsoft-terraform-admin"
}

variable "private_key_file" {
  type = string
}

variable "server_key" {
  type = string
  description = "The JWK key server uses for authenticating requests."
}

variable "image_id" {
  type = string
  description = "The image tag of service to deploy"
}
