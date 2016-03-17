FROM lunalang/base:latest
MAINTAINER kamil.figiela@gmail.com

RUN locale-gen en_US.UTF-8

ENV LANG en_US.UTF-8

ENV LANGUAGE en_US:en

ENV LC_ALL en_US.UTF-8

COPY env /etc/nodelab

COPY supervisor/supervisord-prod.conf /etc/supervisord.conf

COPY supervisor/nginx.conf /etc/nginx/nginx.conf

COPY supervisor/nginx-default /etc/nginx/sites-enabled/default

CMD /usr/bin/supervisord -c /etc/supervisord.conf

ENV HOME /root

EXPOSE 80 8088 9001

COPY nodelab/www /usr/share/nodelab/gui

COPY dist/bin /usr/local/bin/nodelab

COPY userdata /userdata

COPY userdata /data

VOLUME ["/userdata"]
