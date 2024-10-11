
## How to use Roswell to build and share binaries

From the project root:

Run as a script:

    chmod +x roswell/cl-dmarc-cli.ros
    ./roswell/cl-dmarc-cli.ros

Build a binary:

    ros build roswell/cl-dmarc-cli.ros

and run it:

    ./roswell/cl-dmarc-cli

Or install it in ~/.roswell/bin:

    ros install roswell/cl-dmarc-cli.ros

It creates the binary in ~/.roswell/bin/
Run it:

    ~/.roswell/bin/cl-dmarc-cli [name]~&

Your users can install the script with ros install owegner/cl-dmarc-cli

Use `+Q` if you don't have Quicklisp dependencies to save startup time.
Use `ros build --disable-compression` to save on startup time and loose on application size.


## See

- https://github.com/roswell/roswell/wiki/
- https://github.com/roswell/roswell/wiki/Reducing-Startup-Time
