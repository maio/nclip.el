#!/usr/bin/env python

import SimpleHTTPServer
import SocketServer
import subprocess

PORT = 2547

def get_clip():
    child = subprocess.Popen('pbpaste', stdout=subprocess.PIPE)
    return child.stdout.read()

def set_clip(text):
    child = subprocess.Popen('pbcopy', stdin=subprocess.PIPE)
    child.stdin.write(text)
    child.stdin.close()

class MyHandler(SimpleHTTPServer.SimpleHTTPRequestHandler):
    def do_GET(self):
        self.send_response(200)
        self.end_headers()
        self.wfile.write(get_clip())
        self.wfile.close()

    def do_POST(self):
        self.send_response(200)
        self.end_headers()
        length = int(self.headers['Content-Length'])
        set_clip(self.rfile.read(length))

Handler = MyHandler
SocketServer.TCPServer.allow_reuse_address = True
httpd = SocketServer.TCPServer(("127.0.0.1", PORT), Handler)

print "serving at port", PORT
httpd.serve_forever()
