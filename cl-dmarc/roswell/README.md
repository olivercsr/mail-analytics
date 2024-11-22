
## How to use Roswell to build and share binaries

From the project root:

Run as a script:

    chmod +x roswell/dmarc-importer.ros
    ./roswell/dmarc-importer.ros

Build a binary:

    ros build roswell/dmarc-importer.ros

and run it:

    ./roswell/dmarc-importer

Or install it in ~/.roswell/bin:

    ros install roswell/dmarc-importer.ros

It creates the binary in ~/.roswell/bin/
Run it:

    ~/.roswell/bin/dmarc-importer [name]~&

Your users can install the script with ros install owegner/dmarc-importer

Use `+Q` if you don't have Quicklisp dependencies to save startup time.
Use `ros build --disable-compression` to save on startup time and loose on application size.


## See

- https://github.com/roswell/roswell/wiki/
- https://github.com/roswell/roswell/wiki/Reducing-Startup-Time
