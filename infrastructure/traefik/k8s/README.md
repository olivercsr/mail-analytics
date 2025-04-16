# Notes

- traefik should be installed in a separate namespace in the kubernetes cluster (e.g. "traefik"),
  as it can serve the entire cluster. Which namespaces are actually being served can be configured
  in the "traefik.yml" file in the traefik "configmap.yml".
- besides the resource files in here, you may also want to install traefik's custom resource
  definitions (CRDs):
  `kubectl apply -f https://raw.githubusercontent.com/traefik/traefik/v3.3/docs/content/reference/dynamic-configuration/kubernetes-crd-definition-v1.yml`
  these will for example provide middlewares that you might need, e.g. in order to do path/prefix
  rewriting of routes.

