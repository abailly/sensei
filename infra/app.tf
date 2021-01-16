resource "null_resource" "sensei" {

  triggers = {
    image_id = var.image_id
  }

  provisioner "file" {
    source      = "docker-compose.yml"
    destination = "/home/curry/docker-compose.yml"

    connection {
      type = "ssh"
      user = "curry"
      host = google_compute_address.sensei-address.address
      private_key = file(var.private_key_file)
    }
  }

  provisioner "file" {
    source      = "nginx.tmpl"
    destination = "/home/curry/nginx.tmpl"

    connection {
      type = "ssh"
      user = "curry"
      host = google_compute_address.sensei-address.address
      private_key = file(var.private_key_file)
    }
  }

  provisioner "remote-exec" {
    inline = [
      "usermod -G docker curry"
    ]

    connection {
      type = "ssh"
      user = "root"
      host = google_compute_address.sensei-address.address
      private_key = file(var.private_key_file)
    }
  }

  provisioner "remote-exec" {
    inline = [
      "IMAGE_ID=${var.image_id} docker-compose -f /home/curry/docker-compose.yml up -d"
    ]

    connection {
      type = "ssh"
      user = "root"
      host = google_compute_address.sensei-address.address
      private_key = file(var.private_key_file)
    }
  }
}
