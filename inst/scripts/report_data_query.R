#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(synapseClient))
suppressPackageStartupMessages(library(RMySQL))
suppressPackageStartupMessages(library(yaml))
suppressPackageStartupMessages(library(testthat))
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(synapseusagereports))

option_list <- list(
  make_option(c("--project_id"), type = "character",
              help = "Synapse Project ID.",
              dest = "project_id",
              metavar = "synapseid"),
  make_option(c("--start_date"), type = "character",
              help = "Date at UTC (YYYY-MM-DD format)",
              dest = "start_date"),
  make_option(c("--end_date"), type = "character",
              help = "Date at UTC (YYYY-MM-DD format)",
              dest = "end_date"),
  make_option(c("--config_file"), type = "character",
              help = "YAML database configuration file.",
              dest = "config_file",
              default="~/datawarehouse_config.yml"),
  make_option(c("--synapse_config"), type = "character",
              help = "Synapse configuration file.",
              dest = "synapse_config",
              default="~/.synapseConfig")
)

opts <- parse_args(OptionParser(option_list = option_list))

start_date <- lubridate::as_date(opts$start_date)
end_date <- lubridate::as_date(opts$end_date)
date_interval <- lubridate::interval(start_date, lubridate::today("UTC"))
n_months_from_today <- date_interval / lubridate::period(1, "months")

if (n_months_from_today > 6) {
  message("Your start date is more than 6 months ago. The results of your queries may be incorrect.")
}

foo <- suppressMessages(synapseLogin())

config <- yaml.load_file(opts$config_file)

con <- dbConnect(MySQL(),
                 user = config$username,
                 password = config$password,
                 host = config$host,
                 dbname=config$db)

queryData <- report_data_query(con, project_id = opts$project_id,
                               start_date = start_date,
                               end_date = end_date)

cat(readr::format_csv(queryData))

