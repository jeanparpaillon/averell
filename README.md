averell - An incredibly stupid cowboy based web server
======================================================

averell is an incredibly stupid web server to serve a directory through HTTP, like
python's SimpleHTTPServer module.

It is based on the erlang cowboy web server.

# Compilation

```
make
```

or, on Microsoft OS:

```
build.bat
```

# Installation

```
make install
```

# Usage

```
averell [-h] [-p [<port>]] [-c [<cors>]] [-v] [<dir>]

  -h, --help  Show help
  -p, --port  Port number [default: 8000]
  -c, --cors  Enable CORS (allowed origins: *) [default: false]
  -v          Verbose
  <dir>       Directory to serve [default: $CWD]
```
