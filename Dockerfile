FROM alpine
MAINTAINER kamil.figiela@luna-lang.org

ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

COPY env/config /etc/nodelab/config

RUN    apk update \
    && apk add zeromq s6 nginx gmp \
    && ln -s /userdata/projects /etc/nodelab/projects \
    && adduser -u 82 -D -S -G www-data www-data

COPY s6 /etc/s6

COPY supervisor/nginx.conf /etc/nginx/nginx.conf

COPY supervisor/nginx-default /etc/nginx/sites-enabled/default

CMD /bin/s6-svscan /etc/s6

ENV HOME /root

EXPOSE 80 8088 9001

COPY nodelab/www /usr/share/nodelab/gui

COPY dist/bin /usr/local/bin/nodelab

COPY userdata /userdata

COPY userdata /data

VOLUME ["/userdata"]

