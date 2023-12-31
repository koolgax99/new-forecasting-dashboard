FROM rocker/shiny

RUN apt-get update \
    && apt-get -y install libpq-dev libssl-dev \
    && install2.r -e -s -n -1 curl dbplyr DT leaflet \
    && rm -rf /srv/shiny-server/* \
    && rm -rf /var/lib/apt/lists/*
ADD . /srv/shiny-server/

ADD https://raw.githubusercontent.com/rocker-org/shiny/master/shiny-server.sh /usr/bin/

RUN chmod +x /usr/bin/shiny-server.sh

# special script to start shiny server and preserve env variable
CMD /srv/shiny-server/save-env-shiny.sh