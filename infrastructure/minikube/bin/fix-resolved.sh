#!/usr/bin/env sh

minikube ssh <<EOF
echo "DNS=10.96.0.10" | sudo tee -a /etc/systemd/resolved.conf >/dev/null
echo "Domains=cluster.local" | sudo tee -a /etc/systemd/resolved.conf >/dev/null
sudo systemctl restart systemd-resolved
sudo systemctl restart kubelet
resolvectl status | grep "DNS Servers"
exit
EOF

