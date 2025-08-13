# Notes
- use `kubectl kustomize` to generate k8s resource files, e.g.
  `kubectl -n myns apply -f <(kubectl kustomize base/)`

# NFS
If the NFS share cannot be mounted, this might be due to
some DNS resolution issues (in minikube).

Minikube's systemd-resolved is not set up in a way that it
can resolve k8s' services' names
(e.g. nfs.dmarc-dev.svc.cluster.local) by default.

For this to work, run the script `fix-resolved.sh` in the
`infrastructure/minikube/bin` folder.

