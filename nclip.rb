#!/usr/bin/env ruby
require 'webrick'
require 'open3'

$AUTH_TOKEN = ENV["NCLIP_AUTH_TOKEN"] || "my-token"
$PORT = 2547

puts "NCLIP server starting at http://127.0.0.1:#{$PORT}"
puts
puts "Using '#{$AUTH_TOKEN}' as the authorization token."
puts

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

server = WEBrick::HTTPServer.new :BindAddress => "127.0.0.1", :Port => $PORT

server.mount_proc '/' do |req, res|
  raise WEBrick::HTTPStatus::Forbidden, "Invalid token." if
    req.query_string != $AUTH_TOKEN

  if req.request_method == 'GET'
    res.body = OSXClipboard.paste
  else
    OSXClipboard.copy(req.body)
    res.body = "OK"
  end
end

trap 'INT' do server.shutdown end
server.start
