# An extremely primitive web server.  We use this to test various
# networking APIs in Halyard.
#
# To run:
#   sudo gem install sinatra
#   ruby http-test-server.rb

require 'rubygems'
require 'sinatra'

get '/' do
  "This is a primitive web server used for testing Halyard."
end

get '/hello' do
  "Hello!\n" * (params[:count] || "1").to_i
end

get '/echo' do
  builder do |xml|
    xml.form :method => "post", :action => "/echo" do
      xml.input :type => "text", :name => "message", :value => ""
      xml.input :type => "submit"
    end
  end
end

post '/echo' do
  request.body
end
