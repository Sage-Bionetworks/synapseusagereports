#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(synapseClient))
suppressPackageStartupMessages(library(RMySQL))
suppressPackageStartupMessages(library(yaml))
suppressPackageStartupMessages(library(testthat))
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(synapseProjectUsageStatistics))

option_list <- list(
  make_option(c("--project_id"), type = "character",
              help = "Synapse Project ID.",
              dest = "project_id",
              metavar = "synapseid"),
  make_option(c("--months"), type = "integer",
              help = "Number of months",
              dest = "months",
              default=6),
  make_option(c("--config_file"), type = "character",
              help = "YAML database configuration file.",
              dest = "config_file",
              default="~/datawarehouse_config.yml")
)

opts <- parse_args(OptionParser(option_list = option_list))

testthat::expect_lte(opts$months, 6)

foo <- suppressMessages(synapseLogin())

qPageviewTemplate <- 'select ENTITY_ID,CONVERT(AR.TIMESTAMP, CHAR) AS TIMESTAMP,DATE,USER_ID,NODE_TYPE,N.NAME from ACCESS_RECORD AR, PROCESSED_ACCESS_RECORD PAR, NODE_SNAPSHOT N, PROJECT_STATS NODE where AR.RESPONSE_STATUS=200 AND AR.TIMESTAMP > unix_timestamp("%s")*1000 AND AR.TIMESTAMP < unix_timestamp("%s")*1000 AND AR.SESSION_ID = PAR.SESSION_ID and AR.TIMESTAMP = PAR.TIMESTAMP and PAR.ENTITY_ID = NODE.ID AND N.ID = NODE.ID and N.TIMESTAMP = NODE.TIMESTAMP and CLIENT IN ("WEB", "UNKNOWN") AND (PAR.NORMALIZED_METHOD_SIGNATURE IN ("GET /entity/#/bundle", "GET /entity/#/version/#/bundle", "GET /entity/#/wiki2", "GET /entity/#/wiki2/#"));'

qDownloadTemplate <- 'select ENTITY_ID,CONVERT(AR.TIMESTAMP, CHAR) AS TIMESTAMP,DATE,USER_ID,NODE_TYPE,N.NAME from ACCESS_RECORD AR, PROCESSED_ACCESS_RECORD PAR, NODE_SNAPSHOT N, PROJECT_STATS NODE where AR.TIMESTAMP > unix_timestamp("%s")*1000 AND AR.TIMESTAMP < unix_timestamp("%s")*1000 and (AR.RESPONSE_STATUS IN (200, 307)) AND AR.SESSION_ID = PAR.SESSION_ID and AR.TIMESTAMP = PAR.TIMESTAMP and PAR.ENTITY_ID = NODE.ID and N.ID = NODE.ID AND N.TIMESTAMP = NODE.TIMESTAMP and (PAR.NORMALIZED_METHOD_SIGNATURE IN ("GET /entity/#/file", "GET /entity/#/version/#/file"));'

qFDRTemplate <- 'SELECT FDR.ASSOCIATION_OBJECT_ID AS ENTITY_ID, CONVERT(FDR.TIMESTAMP , CHAR) AS TIMESTAMP, DATE_FORMAT(from_unixtime(PROJECT_STATS.TIMESTAMP / 1000), "%%Y-%%m-%%e") AS DATE, FDR.USER_ID, N.NODE_TYPE, N.NAME FROM FILE_DOWNLOAD_RECORD FDR, NODE_SNAPSHOT N, PROJECT_STATS WHERE FDR.TIMESTAMP > unix_timestamp("%s")*1000 AND FDR.TIMESTAMP < unix_timestamp("%s")*1000 AND N.ID = PROJECT_STATS.ID AND PROJECT_STATS.ID = FDR.ASSOCIATION_OBJECT_ID AND FDR.ASSOCIATION_OBJECT_TYPE = "FileEntity" AND N.TIMESTAMP = PROJECT_STATS.TIMESTAMP;'

config <- yaml.load_file(opts$config_file)

projectId <- gsub("syn", "", opts$project_id)
proj <- synGet(opts$project_id)

timestampBreaksDf <- makeDateBreaks(opts$months) %>% dplyr::arrange(date)

con <- dbConnect(MySQL(),
                 user = config$username,
                 password = config$password,
                 host = config$host,
                 dbname=config$db)

queryDataPageviews <- getData(con=con,
                              qTemplate=qPageviewTemplate,
                              projectId=projectId,
                              timestampBreaksDf=timestampBreaksDf)

queryDataPageviewsProcessed <- queryDataPageviews %>%
  dplyr::mutate(recordType='pageview') %>%
  processQuery()

queryDataDownloads <- getData(con=con,
                              qTemplate=qDownloadTemplate,
                              projectId=projectId,
                              timestampBreaksDf=timestampBreaksDf)

queryDataDownloadsProcessed <- queryDataDownloads %>%
  dplyr::mutate(recordType='download') %>%
  processQuery()

queryDataFDR <- getData(con=con,
                        qTemplate=qFDRTemplate,
                        projectId=projectId,
                        timestampBreaksDf=timestampBreaksDf)

queryDataFDRProcessed <- queryDataFDR %>%
  dplyr::mutate(recordType='download') %>%
  processQuery()

queryData <- rbind(queryDataPageviewsProcessed,
                   queryDataDownloadsProcessed,
                   queryDataFDRProcessed)

cat(readr::format_csv(queryData))
