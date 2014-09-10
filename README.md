averell - An incredibly stupid cowboy based web server
======================================================

averell is an incredibly stupid web server to serve a directory through HTTP, like
python's SimpleHTTPServer module.

It is based on the erlang cowboy web server.

# Usage

Usage: averell [-h] [-p [<port>]] [-v] [<dir>]

  -h, --help  Show help
  -p, --port  Port number [default: 8000]
  -v          Verbose
  <dir>       Directory to serve [default: $CWD]
