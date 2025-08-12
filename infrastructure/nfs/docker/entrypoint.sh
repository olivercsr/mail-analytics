#!/usr/bin/env sh

mount -t nfsd nfsd /proc/fs/nfsd

exportfs -avfr

/usr/sbin/rpc.nfsd --tcp --udp --port 2049 --nfs-version 4.2 --no-nfs-version 3
/usr/sbin/rpc.mountd --foreground --port 32767 --nfs-version 4.2 --no-nfs-version 2 --no-nfs-version 3 &

wait

