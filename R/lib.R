#' @importFrom dplyr %>%
#' @importFrom lubridate %m+%

# SQL queries for each type of GET operation.
# The result of each query must be the same for downstream operations to work.
# The result should have the following columns:
# ENTITY_ID,TIMESTAMP,DATE,USER_ID,NODE_TYPE,NAME
# NAME is entity name
# NODE_TYPE is the type of entity (file, folder, project, table, etc.)
# Getting page views should be considered to be unreliable - use Google Analytics instead.
# The 'download' query is also mostly unused by the Synapse clients.
# REST calls have been converted to the newer 'filedownloadrecord' method.
query_template_strings <- list("pageview" = 'select ENTITY_ID,CONVERT(AR.TIMESTAMP, CHAR) AS TIMESTAMP,DATE,USER_ID,NODE_TYPE,N.NAME from ACCESS_RECORD AR, PROCESSED_ACCESS_RECORD PAR, NODE_SNAPSHOT N, PROJECT_STATS NODE where AR.RESPONSE_STATUS=200 AND AR.TIMESTAMP > unix_timestamp("%s")*1000 AND AR.TIMESTAMP < unix_timestamp("%s")*1000 AND AR.SESSION_ID = PAR.SESSION_ID and AR.TIMESTAMP = PAR.TIMESTAMP and PAR.ENTITY_ID = NODE.ID AND N.ID = NODE.ID and N.TIMESTAMP = NODE.TIMESTAMP and CLIENT IN ("WEB", "UNKNOWN") AND (PAR.NORMALIZED_METHOD_SIGNATURE IN ("GET /entity/#/bundle", "GET /entity/#/version/#/bundle", "GET /entity/#/wiki2", "GET /entity/#/wiki2/#"));',
                               "download" = 'select ENTITY_ID,CONVERT(AR.TIMESTAMP, CHAR) AS TIMESTAMP,DATE,USER_ID,NODE_TYPE,N.NAME from ACCESS_RECORD AR, PROCESSED_ACCESS_RECORD PAR, NODE_SNAPSHOT N, PROJECT_STATS NODE where AR.TIMESTAMP > unix_timestamp("%s")*1000 AND AR.TIMESTAMP < unix_timestamp("%s")*1000 and (AR.RESPONSE_STATUS IN (200, 307)) AND AR.SESSION_ID = PAR.SESSION_ID and AR.TIMESTAMP = PAR.TIMESTAMP and PAR.ENTITY_ID = NODE.ID and N.ID = NODE.ID AND N.TIMESTAMP = NODE.TIMESTAMP and (PAR.NORMALIZED_METHOD_SIGNATURE IN ("GET /entity/#/file", "GET /entity/#/version/#/file"));',
                               "filedownloadrecord" = 'SELECT FDR.ASSOCIATION_OBJECT_ID AS ENTITY_ID, CONVERT(FDR.TIMESTAMP , CHAR) AS TIMESTAMP, DATE_FORMAT(from_unixtime(FDR.TIMESTAMP / 1000), "%%Y-%%m-%%d") AS DATE, FDR.USER_ID, N.NODE_TYPE, N.NAME FROM FILE_DOWNLOAD_RECORD FDR, NODE_SNAPSHOT N, PROJECT_STATS WHERE FDR.TIMESTAMP > unix_timestamp("%s")*1000 AND FDR.TIMESTAMP < unix_timestamp("%s")*1000 AND N.ID = PROJECT_STATS.ID AND PROJECT_STATS.ID = FDR.ASSOCIATION_OBJECT_ID AND FDR.ASSOCIATION_OBJECT_TYPE = "FileEntity" AND N.TIMESTAMP = PROJECT_STATS.TIMESTAMP;')

#' Get the SQL query template string.
#'
#' @param query_type The name of the SQL query to get from the 'query_template_strings' lookup.
#'
#' @return An SQL query string.
#' @export
get_query_template_string <- function(query_type) {
  if (!(query_type %in% c("download", "pageview", "filedownloadrecord"))) {
    stop("Not a valid query type.")
  }

  return(query_template_strings[[query_type]])
}

#' Utility to get cleaned data for a usage report.
#'
#' @param con SQL connection object.
#' @param project_id Synapse Project ID.
#' @param query_type The query type, from 'query_template_strings'.
#' @param start_date Start date of data query range, formatted as YYYY-MM-DD
#' @param end_date End date of data query range, formatted as YYYY-MM-DD
#'
#' @export
report_data_query <- function(con, project_id, query_type, start_date, end_date) {

  message(sprintf("Generating a %s report", query_type))

  # the database stores ID as integers, so remive syn prefix
  project_id <- gsub("syn", "", project_id)

  timestampBreaksDf <- makeDateBreaks(start_date, end_date) %>%
    filter(!is.na(start_date), !is.na(end_date))

  query_template <- get_query_template_string(query_type)

  queryData <- getData(con = con,
                       qTemplate = query_template,
                       projectId = project_id,
                       timestampBreaksDf = timestampBreaksDf)

  queryDataProcessed <- queryData %>%
    dplyr::mutate(recordType = query_type) %>%
    processQuery()


  return(queryDataProcessed)
}

#' Utility function to run all data query types.
report_data_query_all <- function(con, project_id, start_date, end_date) {

  queryDataDownload <- report_data_query(con = con,
                                         project_id = project_id,
                                         query_type = "pageview",
                                         start_date = start_date,
                                         end_date = end_date)

  queryDataPageview <- report_data_query(con = con,
                                         project_id = project_id,
                                         query_type = "download",
                                         start_date = start_date,
                                         end_date = end_date)


  queryDataFDR <- report_data_query(con = con,
                                    project_id = project_id,
                                    query_type = "filedownloadrecord",
                                    start_date = start_date,
                                    end_date = end_date)

  queryDataFDR$recordType <- "download"

  queryData <- rbind(queryDataPageview,
                     queryDataDownload,
                     queryDataFDR)

  return(queryData)
}

#' Run an SQL query using a template from 'query_template_strings'.
#'
#' @param con SQL connection object.
#' @param template The query template string from 'query_template_strings'.
#' @param projectId Synapse Project ID.
#' @param start_date Start date of data query range, formatted as YYYY-MM-DD
#' @param end_date End date of data query range, formatted as YYYY-MM-DD

#' @export
doQuery <- function(con, template, projectId, start_date, end_date) {
  q <- sprintf(template, start_date, end_date)
  message(sprintf("Query: %s", q))
  message(sprintf("Querying %s to %s", start_date, end_date))

  res <- DBI::dbGetQuery(conn = con, statement = q)
  return(res)
}

#' Process the results of the SQL query from the 'doQuery' function.
#'
#' @param data The output from the 'doQuery' function.
#'
#' @export
processQuery <- function(data) {
  queryData <- data %>%
    dplyr::rename(userId = USER_ID, id = ENTITY_ID) %>%
    dplyr::select(userId, id, DATE, TIMESTAMP, NODE_TYPE, NAME, recordType) %>%
    dplyr::mutate(date = as.Date(as.character(DATE)),
                  userId = as.character(userId),
                  id = as.character(id),
                  dateGrouping = lubridate::floor_date(date, unit = "month"),
                  monthYear = paste(lubridate::month(dateGrouping, label = TRUE),
                                    lubridate::year(dateGrouping))) %>%
    # TODO Maybe this can be deprecated
    dplyr::group_by(id, userId, TIMESTAMP, recordType) %>% # Get unique due to name changes, might not be most recent name!
    dplyr::arrange(TIMESTAMP) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  queryData
}

#' Fetch data from a data warehouse database.
#' This first performs a filtering operation to limit the query to entities within the specified project.
#' A temporary table is created and the timestamp of the most recent snapshot is added, which is used for joining.
#'
#' @param con A database connection.
#' @param qTemplate A string of the query template from 'query_template_strings'.
#' @param projectId A Synapse Project ID.
#' @param timestampBreaksDf A data frame of timestamp intervals.
#' @return A data frame of the query results.
#' @export
getData <- function(con, qTemplate, projectId, timestampBreaksDf) {
  q.create_temp <- "CREATE TEMPORARY TABLE PROJECT_STATS (`TIMESTAMP` bigint(20) NOT NULL, `ID` bigint(20) NOT NULL, PRIMARY KEY (`ID`,`TIMESTAMP`)); "
  create <- DBI::dbSendQuery(conn = con,
                             statement = q.create_temp)
  message(sprintf("Created temporary table for entities in project %s", projectId))

  q.insert_temp <- "INSERT INTO PROJECT_STATS (ID, TIMESTAMP) SELECT ID, MAX(TIMESTAMP) AS TIMESTAMP FROM NODE_SNAPSHOT WHERE PROJECT_ID = %s GROUP BY ID;"
  query_statement <- sprintf(q.insert_temp, projectId)
  insert <- DBI::dbSendQuery(conn = con,
                             statement = query_statement)
  message(sprintf("Inserted rows into temporary table for entities in project %s", projectId))

  res <- plyr::ddply(timestampBreaksDf, plyr::.(month, year),
                     function(x) doQuery(con = con,
                                         template = qTemplate,
                                         projectId = projectId,
                              					 start_date = x$start_date,
                              					 end_date = x$end_date
                              					 )
                     )

  foo <- DBI::dbSendQuery(conn = con, statement = 'DROP TABLE PROJECT_STATS;')

  res
}


#' @export
makeDateBreaks <- function(start_date, end_date) {

  start_date_floor <- lubridate::floor_date(start_date, unit = "month")
  end_date_floor <- lubridate::floor_date(end_date, unit = "month")

  date_range <- lubridate::interval(start_date_floor, end_date_floor)

  n_months <- floor(date_range / lubridate::period(1, "months"))

  beginDates <- end_date_floor - (lubridate::period(1, "months") * 0:(n_months))

  tibble::tibble(start_date = beginDates) %>%
    dplyr::mutate(end_date = dplyr::lag(start_date),
                  month=lubridate::month(start_date),
                  year=lubridate::year(start_date)) %>%
    dplyr::arrange(start_date)
}
