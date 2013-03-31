#!/usr/bin/env ruby
require 'webrick'
require 'open3'

class OSXClipboard
  def self.paste()
    `pbpaste`
  end

  def self.copy(text)
    stdin, stdout, stderr = Open3.popen3('pbcopy')
    stdin.write(text)
    stdin.close
  end
end

server = WEBrick::HTTPServer.new :Port => 2547

server.mount_proc '/' do |req, res|
  if req.request_method == 'GET'
    res.body = OSXClipboard.paste
  else
    OSXClipboard.copy(req.body)
    res.body = "OK"
  end
end

trap 'INT' do server.shutdown end
server.start
