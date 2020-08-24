Ready-to-use binaries
=====================

Instructions for macOS, Linux, Windows, and FreeBSD:
----------------------------------------------------
Download both dictionary_data.zip and the words binary for your platfom.
You can expand both archives to the same directory and run from there.
Or you can put the words binary and dictionary data into separate directories so long as the dictionary data directory is either (1) in your PATH environment variable or (2) is identified in a LATINWORDS environment variable (fully qualified path, no trailing slash).

On Windows, PowerShell will give you prettier output than cmd.exe if you keep vt100 format codes on.


Special notes and instructions for OS/2:
----------------------------------------
The OS/2 version was built with an ancient toolchain and libraries.  This has two consequences:   
First, the OS/2 version's dictionary data file format is unique.  The OS/2 archive includes its own set of dictionary data files.  The OS/2 program will not accept the other platforms' dictionary data files (and vice versa).
Second, the OS/2 version lacks two very minor features:  It does not support the new -x ("no exit") parameter, and the dictionary data files must reside in the same directory as the program (and that directory must be set as the working directory).  

The OS/2 version requires emx 0.9d with FIX1 through FIX4 applied.  ArcaOS includes compatible emx libraries in the base install.  If you're on another release you may need to grab emx from Hobbes.  I haven't tested the program on releases older than eCS 1.2, but in theory it should run on Warp 4 and later.

*License note:*

All binaries here were built with FSF GNAT and are subject to the GPL runtime-linking exception.  Col. Whitaker released his software under his own permissive license (see /docs/"Whitaker's original"/words.html for his license information), not any version of the GPL.  All copyrightable expression in my contributions is also subject to Col. Whitaker's permissive license.
