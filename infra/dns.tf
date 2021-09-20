resource "google_dns_managed_zone" "pankzsoft-app-zone" {
  name        = "pankzsoft-app"
  dns_name    = "app.pankzsoft.com."
  description = "DNS zone to manage Pankzsoft apps"
  visibility  = "public"
}

# A entry pointing to VM hosting the app
resource "google_dns_record_set" "sensei" {
  name         = "sensei.${google_dns_managed_zone.pankzsoft-app-zone.dns_name}"
  managed_zone = google_dns_managed_zone.pankzsoft-app-zone.name
  type         = "A"
  ttl          = 300

  rrdatas = [google_compute_address.sensei-address.address]
}

output "name_servers" {
  value = google_dns_managed_zone.pankzsoft-app-zone.name_servers
}
