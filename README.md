## NCLIP

Network clipboard for Emacs which makes possible to use your local
clipboard even when you are running Emacs inside terminal on remote
server.

When enabled, it will use remote HTTP server to fetch and update
clipboard content. Included HTTP server `nclip.rb` supports OSX
(pbcopy/pbpaste) but it should be pretty straightforward to support
other systems as well (e.g. Linux using xclip).

### Usage

Run `nclip.rb` on your local machine. Setup SSH port forwarding so
that when you SSH to remote server it will forward `2547` port on
remote server over SSH to `127.0.0.1:2547`.

From command line:
```ssh -R 2547:127.0.0.1:2547 some-server```

Or you can use `~/.ssh/config`:
```
Host some-server
    RemoteForward 127.0.0.1:2547 127.0.0.1:2547
```

### Security

NCLIP uses plain HTTP server which listens on localhost port 2547 so
anyone on the same host can send HTTP requests to it. GET request will
return content of your clipboard and POST request will update it's
content.

To protect your clipboard data when using NCLIP on hosts shared by
more people, NCLIP provides simple token authorization. In order to
use it, you put some secret string into NCLIP_AUTH_TOKEN environment
variable before running `nclip.rb` and Emacs. NCLIP will then use this
token to authorize all clipboard requests.

If you don't run Emacs on shared hosts you can skip this step and
NCLIP will use default token.

### TODO

- Support other operating systems
- Make host/port configurable
- Don't use ENV variable for the token