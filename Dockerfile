FROM kfigiela/nodelab-env:latest
MAINTAINER kamil.figiela@gmail.com


COPY env /etc/nodelab

COPY supervisor/supervisord-prod.conf /etc/supervisord.conf

COPY supervisor/nginx.conf /etc/nginx/nginx.conf

COPY supervisor/nginx-default /etc/nginx/sites-enabled/default

CMD /usr/bin/supervisord -c /etc/supervisord.conf

ENV HOME /root

EXPOSE 80 8088 9001

COPY nodelab/www /usr/share/nodelab/gui

COPY build_7.10/.bin /usr/local/bin/nodelab
