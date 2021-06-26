resource "google_compute_address" "sensei-address" {
  name = "sensei-address"
}

resource "google_compute_instance" "sensei" {
  project      = "pankzsoft-terraform-admin"
  name         = "sensei-1"
  # 2 CPU/2GB
  machine_type = "e2-small"
  allow_stopping_for_update = true

  tags = [ "sensei-server" ]

  metadata = {
    sshKeys = file("ssh_keys")
  }


  boot_disk {
    initialize_params {
      size  = 50
      image = "compute-1580580048"
    }
  }

  network_interface {
    network       = "default"
    access_config {
      nat_ip = google_compute_address.sensei-address.address
    }
  }
}

output "instance_id" {
  value = google_compute_instance.sensei.self_link
}

output "instance_ip" {
  value = google_compute_address.sensei-address.address
}
