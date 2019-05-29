FROM rocker/tidyverse:latest

RUN apt-get update -y
RUN apt-get install -y dpkg-dev zlib1g-dev libssl-dev libffi-dev
RUN apt-get install -y curl libcurl4-openssl-dev

ENV R_REMOTES_NO_ERRORS_FROM_WARNINGS=true

COPY . /synapseusagereports
WORKDIR /synapseusagereports

RUN Rscript -e 'devtools::install_deps(pkg = ".", dependencies = TRUE, threads = getOption("Ncpus",1))'
RUN R CMD INSTALL .

COPY inst/scripts/render_report.R /usr/local/bin/
COPY inst/scripts/report_data_query.R /usr/local/bin/
