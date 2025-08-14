#!/bin/sh -e

trap stop SIGTERM SIGINT

stop() {
  /usr/sbin/rpc.nfsd 0
  killall /usr/sbin/rpc.mountd
  umount /proc/fs/nfsd /var/lib/nfs/rpc_pipefs

  exit 0
}


mount -t nfsd nfsd /proc/fs/nfsd

# rm -vf /etc/idmapd.conf

exportfs -avfr

/sbin/rpcbind
/usr/sbin/rpc.nfsd --tcp --udp --port 2049 --nfs-version 4.2 --no-nfs-version 3
/usr/sbin/rpc.mountd --foreground --port 20048 --nfs-version 4.2 --no-nfs-version 2 --no-nfs-version 3 &
# /usr/sbin/rpc.nfsd --tcp --udp --port 2049
# /usr/sbin/rpc.statd --port 32765
# /usr/sbin/rpc.idmapd
# /usr/sbin/rpc.mountd --foreground --port 20048 &

wait

