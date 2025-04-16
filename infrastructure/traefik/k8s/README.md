# Note

traefik should be installed in a separate namespace in the kubernetes cluster (e.g. "traefik"),
as it can serve the entire cluster. Which namespaces are actually being served can be configured
in the "traefik.yml" file in the traefik "configmap.yml".

