## NCLIP

Network clipboard for Emacs which makes possible to use your local
clipboard even when you are running Emacs inside terminal on remote
server.

When enabled it will use remote HTTP server to fetch and update
clipboard content. Included HTTP server `nclip.py` supports OSX
(pbcopy/pbpaste) but it should be pretty straightforward to support
other systems as well (e.g. Linux using xclip).

### Usage

Run `nclip.py` on your local machine. Setup SSH port forwarding so
that when you SSH to remote server it will forward `2547` port on
remote server over SSH to `127.0.0.1:2547`.

```ssh -R 2547:127.0.0.1:2547 server```

### Security

*Warning*: Other people on same host are able to access your clipboard
if you use current version of nclip.el

I'm using it for local development inside Vagrant or on server with
trusted people so I don't care too much about this right now, but it
would be nice to secure access to the clipboard so that people on same
host can't access it by fetching `http://127.0.0.1:2547/`.

Someone could just POST "rm -rf /\n" to `http://127.0.0.1:2547/` so be
aware. :)

### TODO

- Security
- Support other operating systems
- Make host/port configurable