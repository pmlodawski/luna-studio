require 'eventmachine'
require 'em-http-server'
require 'mimemagic'

require_relative 'logger.rb'

class HTTPHandler < EM::HttpServer::Server
    include Logging
    def process_http_request
        logger.info("File #{@http_request_uri} requested over HTTP")
        response = EM::DelegatedHttpResponse.new(self)

        filename = File.join(".", @http_request_uri)

        if File.exists?(filename)
            
            response.status = 200
            if File.directory?(filename)
                response.content_type "text/html"

                index_loop = Dir[File.join(filename, "*")].map do |file|
                    %Q[<a href="#{file}">#{file}</a>]
                end

                response.content = <<-INDEX
                    <html>
                        <head></head>
                        <body><h1>Index of #{filename}</h1>
                        <hr>
                        #{index_loop.join("<br>\n")}
                        <hr>
                        </body>

                    </html>
                INDEX
            else
                response.content_type MimeMagic.by_path(filename)
                response.content = File.read(filename)
            end
        else
            response.content_type "text/plain"
            response.status = 404
            response.content = "#{filename}: File not found"
        end

        response.send_response
    end

    def http_request_errback e
      logger.error(e.inspect)
    end
end
