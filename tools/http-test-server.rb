# An extremely primitive web server.  We use this to test various
# networking APIs in Halyard.
#
# To run:
#   sudo gem install sinatra
#   ruby http-test-server.rb

require 'rubygems'
require 'sinatra'
require 'json'

get '/' do
  "This is a primitive web server used for testing Halyard."
end

get '/not-found' do
  status 404
  "This page doesn't exist."
end

get '/hello' do
  "Hello!\n" * (params[:count] || "1").to_i
end

get '/upload' do
  builder do |xml|
    xml.form :method => "post", :action => "/upload" do
      xml.input :type => "text", :name => "message", :value => ""
      xml.input :type => "submit"
    end
  end
end

post '/upload' do
  "post: #{request.body.read}"
end

get '/headers/:name' do 
  content_type 'text/plain'
  env["HTTP_#{params[:name].upcase.gsub(/-/, '_')}"]
end

get '/add' do
  content_type :json
  (params[:x].to_i + params[:y].to_i).to_json
end

post '/echo' do
  content_type request.content_type
  request.body
end


#==========================================================================
#  Simulated HACP LMS API
#==========================================================================

require 'test/unit/assertions'

Sinatra::Application.send(:include, Test::Unit::Assertions)

post '/hacp/register' do
  assert_equal "44463f20-b4c6-4a3e-abf6-b942d010deb3", params[:uuid]
  assert_equal "J. Student", params[:name]
  assert_equal "12345", params[:student_id]

  content_type :json
  {}.to_json
end

post '/hacp/new_session' do
  assert_equal "44463f20-b4c6-4a3e-abf6-b942d010deb3", params[:uuid]
  
  content_type :json
  { 'aicc_url' => "http://localhost:4567/hacp",
    'aicc_sid' => "#{params[:uuid]}:123:4567" }.to_json
end
