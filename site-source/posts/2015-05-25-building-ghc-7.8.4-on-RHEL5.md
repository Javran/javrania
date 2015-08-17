---
layout: post
title: Building ghc-7.8.4 on RHEL5
tags: haskell, ghc, linux
---

For some reasons I need to run haskell programs on a RHEL5 machine.
Sadly the system packages were too old to support the latest binary,
and I don\'t have the root privilege to install any software
so have to build it from source.

Here I want to share my experience of building ghc-7.8 from source in
a rather old operating system.

I also want to say thanks to Tim Docker, who posted [an
article](https://twdkz.wordpress.com/2011/12/21/installing-ghc-7-0-3-and-the-haskell-platform-on-rhel-5-6/)
without which my post might not be possible.

Basically, you need to first have a working ghc binary before you can build a newer one yourself.
Please refer to [GHC Wiki](https://ghc.haskell.org/trac/ghc/wiki/Building/Preparation/Tools) for details.

We will begin with ghc-6.8.3, bootstrap ghc-6.10, ghc-7.0, ghc-7.4 and finally ghc-7.8.

# Download related files

We need all the files above.
For items marked with `(latest)`, just downloading the latest version instead should also work.

I assume you have set the environment variable `SOURCES` (in your `.bashrc`) to the directory containing all your tarball files.
(it is recommended here to use absolute path),
which will also be our working directory for building up everything related.

* ghc

    If any of the following link does not work, go to [this download page](https://www.haskell.org/ghc/download)
    and download them accordingly.

    * [ghc-6.8.3-x86_64-unknown-linux.tar.bz2](https://www.haskell.org/ghc/dist/6.8.3/ghc-6.8.3-x86_64-unknown-linux.tar.bz2)
    * [ghc-6.10.4-src.tar.bz2](https://www.haskell.org/ghc/dist/6.10.4/ghc-6.10.4-src.tar.bz2)
    * [ghc-7.0.3-src.tar.bz2](https://www.haskell.org/ghc/dist/7.0.3/ghc-7.0.3-src.tar.bz2)
    * [ghc-7.4.2-src.tar.bz2](https://www.haskell.org/ghc/dist/7.4.2/ghc-7.4.2-src.tar.bz2)
    * [ghc-7.8.4-src.tar.bz2](https://www.haskell.org/ghc/dist/7.8.4/ghc-7.8.4-src.tar.bz2)

* gcc

    Choose your preferred GCC mirror site on [this page](https://gcc.gnu.org/mirrors.html).
    And download the corresponding version from directory `releases/gcc-x.y.z`.

    All links above are based on [ConcertPass.com Mirror Server](http://mirrors.concertpass.com/).

    * [gcc-4.4.3](http://mirrors.concertpass.com/gcc/releases/gcc-4.4.3/gcc-4.4.3.tar.bz2)
    * [gcc-4.9.2](http://mirrors.concertpass.com/gcc/releases/gcc-4.9.2/gcc-4.9.2.tar.bz2) (latest)

* [binutils-2.25.tar.bz2](http://ftp.gnu.org/gnu/binutils/binutils-2.25.tar.bz2) (latest)

* Checksum:

        $ (cd $SOURCES; sha256sum *)
	    22defc65cfa3ef2a3395faaea75d6331c6e62ea5dfacfed3e2ec17b08c882923  binutils-2.25.tar.bz2
	    97ed664694b02b4d58ac2cafe443d02a388f9cb3645e7778843b5086a5fec040  gcc-4.4.3.tar.bz2
	    2020c98295856aa13fda0f2f3a4794490757fc24bcca918d52cc8b4917b972dd  gcc-4.9.2.tar.bz2
	    d66a8e52572f4ff819fe5c4e34c6dd1e84a7763e25c3fadcc222453c0bd8534d  ghc-6.10.4-src.tar.bz2
	    07d06efa9222638c80b72ceda957d3c7bcbdc2665a9738dd6ef6c0751a05e8ed  ghc-6.8.3-x86_64-unknown-linux.tar.bz2
        156169c28dab837922260a0fbfcc873c679940d805a736dc78aeb1b60c13ccd9  ghc-7.0.3-src.tar.bz2
	    f2ee1289a33cc70539287129841acc7eaf16112bb60c59b5a6ee91887bfd836d  ghc-7.4.2-src.tar.bz2
	    59e3bd514a1820cc1c03e1808282205c0b8518369acae12645ceaf839e6f114b  ghc-7.8.4-src.tar.bz2

Note: I can only confirm that I got a working ghc-7.8.4 following these steps in this specific order.
It is likely that these early versions of ghc has troubles with the latest gcc and binutils.

# Install ghc-6.8.3

In order to get a clean installation, I will isolate each package by using a different
prefix directory. But it is still okay to share the same prefix directory if you want.
The major difference is that if you share prefix directory between packages,
the old files might not be overwritten and thus remain there and become junk.

```bash
$ cd $SOURCES
$ tar -xf ghc-6.8.3-x86_64-unknown-linux.tar.bz2
$ cd ghc-6.8.3
$ mkdir prefix
$ ./configure --prefix $SOURCES/ghc-6.8.3/prefix
$ make install
```

# Install ghc-6.10.4

From now on, whenever you see a `make` command without explicit goal,
feel free to pass `-j N` to `make` to speed up compilation
(where `N` is usually set to the number of available CPU cores plus one).
If something fails, just rerun `make` (maybe with a smaller `N`),
do `make clean` only if you are getting the same error message through
different runs.

```bash
$ cd $SOURCES
$ tar -xf ghc-6.10.4-src.tar.bz2
$ cd ghc-6.10.4
$ mkdir prefix
$ PATH="$SOURCES/ghc-6.8.3/prefix/bin:$PATH" ./configure --prefix $SOURCES/ghc-6.10.4/prefix
$ PATH="$SOURCES/ghc-6.8.3/prefix/bin:$PATH" make
$ make install
```

# Install gcc-4.4.3

Now you need to install a newer version of gcc.
The gcc shipped with RHEL5 is gcc-4.1.2, which is too old to get ghc-7.0 compiled.

The reason why we do not use the latest version of gcc is that it cannot compile ghc-7.0 either...


```bash
$ cd $SOURCES
$ tar -xf gcc-4.4.3.tar.bz2
$ tar -xf gcc-4.9.2.tar.bz2
```
Copy a script from gcc-4.9.2 to gcc-4.4.3:

```bash
$ cp gcc-4.9.2/contrib/download_prerequisites gcc-4.4.3/contrib/
```

And edit the one under gcc-4.4.3 (feel free to replace `$EDITOR` with whatever text editor you like):

```bash
$ $EDITOR gcc-4.4.3/contrib/download_prerequisites
```

Find and comment out the following line:

```
# GRAPHITE_LOOP_OPT=yes
```

Go into the directory, it\'s time to retrieve some dependencies:

```bash
$ cd gcc-4.4.3
$ ./contrib/download_prerequisites
```

Now create another directory under gcc-4.4.3.
To compile gcc you need to make your working directory different
from gcc's. Proceed with routines,
remember to set prefix and enable only the necessary language.
(P.S. `--enable-languages=c,c++` is necessary because
to bootstrap newer versions of gcc, you will need both `gcc` and `g++`).

```bash
$ mkdir prefix
$ mkdir gcc-build
$ cd gcc-build
$ ../configure --prefix $SOURCES/gcc-4.4.3/prefix --enable-languages=c,c++
$ make
$ make install
```



And now gcc-4.4.3 should be ready, here we can set it permanently.
Put the following line to the bottom of your `~/.bashrc` file:

```bash
# if you want to unset $SOURCES afterwards,
# remember to replace it with the real location.
export PATH="$SOURCES/gcc-4.4.3/prefix/bin:$PATH"
```

Now verify:

```bash
$ source ~/.bashrc
$ gcc --version
gcc (GCC) 4.4.3
Copyright (C) 2010 Free Software Foundation, Inc.
This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
```

# Install ghc-7.0.3

```bash
$ cd $SOURCES
$ tar -xf ghc-7.0.3-src.tar.bz2
$ cd ghc-7.0.3
$ mkdir prefix
$ PATH="$SOURCES/ghc-6.10.4/prefix/bin:$PATH" ./configure --prefix $SOURCES/ghc-7.0.3/prefix
$ PATH="$SOURCES/ghc-6.10.4/prefix/bin:$PATH" make
$ make install
```

# Install ghc-7.4.2

```bash
$ cd $SOURCES
$ tar -xf ghc-7.4.2-src.tar.bz2
$ cd ghc-7.4.2
$ mkdir prefix
$ PATH="$SOURCES/ghc-7.0.3/prefix/bin:$PATH" ./configure --prefix $SOURCES/ghc-7.4.2/prefix
$ PATH="$SOURCES/ghc-7.0.3/prefix/bin:$PATH" make
$ make install
```

# (Optional) Update gcc and binutils

This step might not be necessary.

Some packages from hackage (for example ciper-aes)
would fail because of the out-dated binutils.

So before we get to 7.8.4, I want to make sure
we have a newer version of toolchain, by using which
we might also benefit from new optimization techniques.


First we build the latest version of gcc from source.

```bash
$ source ~/.bashrc
$ cd $SOURCES
# should have been extracted when we were compiling gcc-4.4.3
$ cd gcc-4.9.2
$ ./contrib/download_prerequisites
$ mkdir prefix
$ mkdir gcc-build
$ cd gcc-build
$ ../configure --prefix $SOURCES/gcc-4.9.2/prefix --enable-languages=c,c++
$ make
$ make install
```

Then we can change the last few lines added to our `~/.bashrc` to:

```bash
export PATH="$SOURCES/gcc-4.9.2/prefix/bin:$PATH"
```

Then we build binutils:

```bash
$ source ~/.bashrc
$ cd $SOURCES
$ tar -xf binutils-2.25.tar.bz2
$ cd binutils-2.25
$ mkdir prefix
$ ./configure --prefix $SOURCES/binutils-2.25/prefix
$ make
$ make install
```

And add it to `PATH` by appending these lines to your `~/.bashrc`:

```bash
export PATH="$SOURCES/binutils-2.25/prefix/bin:$PATH"
```

And don't forget to source your `bashrc`:

```bash
$ source ~/.bashrc
```

# Install ghc-7.8.4

```bash
$ cd $SOURCES
$ tar -xf ghc-7.8.4-src.tar.bz2
$ cd ghc-7.8.4
$ mkdir prefix
$ PATH="$SOURCES/ghc-7.4.2/prefix/bin:$PATH" ./configure --prefix $SOURCES/ghc-7.8.4/prefix
$ PATH="$SOURCES/ghc-7.4.2/prefix/bin:$PATH" make
$ make install
```

# Add ghc to PATH and Clean up `.bashrc`

By now you might have a quite messy `$PATH`, don't worry, here we will clean it up.

The last few lines in your `.bashrc` will look like:

```bash
export SOURCES="${HOME}/sources"

# if you want to unset $SOURCES afterwards,
# remember to replace it with the real location.
export PATH="$SOURCES/gcc-4.9.2/prefix/bin:$PATH"
export PATH="$SOURCES/binutils-2.25/prefix/bin:$PATH"
```

Here we add one line for ghc-7.8.4, and replace `$SOURCES` with
the corresponding value (here I assume `SOURCE="${HOME}/sources"`,
but you need to change it accordingly).

```bash
#export SOURCES="${HOME}/sources"

# if you want to unset $SOURCES afterwards,
# remember to replace it with the real location.
export PATH="${HOME}/sources/gcc-4.9.2/prefix/bin:$PATH"
export PATH="${HOME}/sources/binutils-2.25/prefix/bin:$PATH"
export PATH="${HOME}/sources/ghc-7.8.4/prefix/bin:$PATH"
```

Now everything is done, just logout and relogin, then you should have ghc-7.8.4 ready.

```bash
# login
$ ld --version
GNU ld (GNU Binutils) 2.25
Copyright (C) 2014 Free Software Foundation, Inc.
This program is free software; you may redistribute it under the terms of
the GNU General Public License version 3 or (at your option) a later version.
This program has absolutely no warranty.
$ gcc --version
gcc (GCC) 4.9.2
Copyright (C) 2014 Free Software Foundation, Inc.
This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

$ ghc --version
The Glorious Glasgow Haskell Compilation System, version 7.8.4
```

# Afterthoughts

I hope this article might be helpful in case you want to do something similar.
I like the idea of bootstrapping, but sometimes it is painful to walk through the process,
especially when the working binary version is too old and you need to bootstrap multiple times.

Lesson learned: backward compatibility is not always reliable.
If you want some old software to work, prefering tools with version relatively
recent to that software over ones with the latest version usually help.
