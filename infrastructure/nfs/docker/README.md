Note that the NFS server needs to be run with elevated privileges,
as it makes use of kernel features.

So start the Docker container either with:
- `--cap-add SYS_ADMIN`
- `--privileged`

