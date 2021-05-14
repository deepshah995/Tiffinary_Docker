# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:latest

# system libraries of general use
## install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev
RUN echo "step1"
## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean
RUN echo "step2"
# install renv & restore packages
RUN R -e "install.packages(c('tidyr','leaflet','dplyr','shinydashboard','shinyWidgets','reactable','tableHTML','geosphere','reactlog','shinycssloaders','googlesheets4','shinyalert','shinyjs','crosstalk'), dependencies = TRUE, repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "install.packages('renv', repos = c(CRAN = 'https://cloud.r-project.org'))"

RUN mkdir /root/app
COPY R root/shiny_save

# expose port
EXPOSE 3838

# run app on container start
CMD ["R", "-e", "shiny::runApp('root/shiny_save', host = '0.0.0.0', port = 3838)"]