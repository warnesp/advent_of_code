
## How to use Roswell to build and share binaries

From the project root:

Run as a script:

    chmod +x roswell/advt2024.ros
    ./roswell/advt2024.ros

Build a binary:

    ros build roswell/advt2024.ros

and run it:

    ./roswell/advt2024

Or install it in ~/.roswell/bin:

    ros install roswell/advt2024.ros

It creates the binary in ~/.roswell/bin/
Run it:

    ~/.roswell/bin/advt2024 [name]~&

Your users can install the script with ros install pwarnes/advt2024

Use `+Q` if you don't have Quicklisp dependencies to save startup time.
Use `ros build --disable-compression` to save on startup time and loose on application size.


## See

- https://github.com/roswell/roswell/wiki/
- https://github.com/roswell/roswell/wiki/Reducing-Startup-Time
