#!/bin/bash -e
# configure Ubuntu-based machines with everything needed for building

# install Docker
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -
sudo apt-key fingerprint 0EBFCD88
sudo apt-add-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu $(lsb_release -cs) edge"

# install neovim
sudo add-apt-repository ppa:neovim-ppa/stable

# install gcloud
# Create environment variable for correct distribution
CLOUD_SDK_REPO="cloud-sdk-$(lsb_release -c -s)"
export DEBIAN_FRONTEND=noninteractive

# Add the Cloud SDK distribution URI as a package source
echo "deb http://packages.cloud.google.com/apt $CLOUD_SDK_REPO main" | sudo tee -a /etc/apt/sources.list.d/google-cloud-sdk.list

# Import the Google Cloud Platform public key
curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | sudo apt-key add -

# Update the package list
sudo -E apt-get update

# Ugrade image
sudo -E apt-get upgrade -y

sudo -E apt-get install -y apt-transport-https  ca-certificates  curl  software-properties-common git \
     emacs libtinfo-dev tmux graphviz wget jq python3 python3-pip bzip2 readline-common google-cloud-sdk \
     neovim docker-ce inotify-tools silversearcher-ag

# install docker-compose
sudo curl -L "https://github.com/docker/compose/releases/download/1.21.2/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
sudo chmod +x /usr/local/bin/docker-compose

# prefer ipv4 connections over ipv6
echo "precedence ::ffff:0:0/96  100" | sudo tee -a /etc/gai.conf

# install stack
curl -sSL https://get.haskellstack.org/ | sh

# configure unattended upgrades
distro_id=$(lsb_release -is)
distro_codename=$(lsb_release -cs)

cat | sudo tee /etc/apt/apt.conf.d/50unattended-upgrades <<EOF
Unattended-Upgrade::Allowed-Origins {
  "${distro_id}:${distro_codename}";
  "${distro_id}:${distro_codename}-security";
  "${distro_id}:${distro_codename}-updates";
  "${distro_id}ESM:${distro_codename}";
}
Unattended-Upgrade::Remove-Unused-Kernel-Packages "true";
Unattended-Upgrade::Remove-Unused-Dependencies "true";
Unattended-Upgrade::Automatic-Reboot "true";
Unattended-Upgrade::Automatic-Reboot-Time "05:38";
EOF

cat | sudo tee /etc/apt/apt.conf.d/20auto-upgrades <<EOF
APT::Periodic::Update-Package-Lists "1";
APT::Periodic::Download-Upgradeable-Packages "1";
APT::Periodic::AutocleanInterval "7";
APT::Periodic::Unattended-Upgrade "1";
EOF
