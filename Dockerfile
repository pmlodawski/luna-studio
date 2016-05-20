FROM alpine
MAINTAINER kamil.figiela@luna-lang.org

ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

RUN    apk update \
    && apk add zeromq supervisor nginx gmp

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

COPY runtime /runtime

#RUN    cd /runtime \
#    && stack build

VOLUME ["/userdata"]
