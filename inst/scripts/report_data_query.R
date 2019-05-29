#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(readr))
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
  make_option(c("--query_type"), type = "character",
              help = "Type of query to perform. One of: download, pageview, filedownloadrecord, all",
              dest = "query_type"),
  make_option(c("--start_date"), type = "character",
              help = "Date at UTC (YYYY-MM-DD format)",
              dest = "start_date"),
  make_option(c("--end_date"), type = "character",
              help = "Date at UTC (YYYY-MM-DD format)",
              dest = "end_date"),
  make_option(c("--config_file"), type = "character",
              help = "YAML database configuration file.",
              dest = "config_file",
              default = "~/datawarehouse_config.yml")
)

opts <- parse_args(OptionParser(option_list = option_list))

start_date <- lubridate::as_date(opts$start_date)
end_date <- lubridate::as_date(opts$end_date)
date_interval <- lubridate::interval(start_date, lubridate::today("UTC"))
n_months_from_today <- date_interval / lubridate::period(1, "months")

if (n_months_from_today > 6) {
  message("Your start date is more than 6 months ago. The results of your queries may be incorrect.")
}

config <- yaml.load_file(opts$config_file)

con <- RMySQL::dbConnect(RMySQL::MySQL(),
                         user = config$username,
                         password = config$password,
                         host = config$host,
                         dbname = config$db)

if (opts$query_type == "all") {
  queryData <- report_data_query_all(con, project_id = opts$project_id,
                                     start_date = start_date,
                                     end_date = end_date)
} else {
  queryData <- report_data_query(con, project_id = opts$project_id,
                                 query_type = opts$query_type,
                                 start_date = start_date,
                                 end_date = end_date)

}

cat(readr::format_csv(queryData))

