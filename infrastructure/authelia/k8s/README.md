# Notes
- Install these resources in the same namespaces as the traefik resources.
- use `kubectl kustomize` to generate k8s resource files, e.g.
  `kubectl -n myns apply -f <(kubectl kustomize base/)`

