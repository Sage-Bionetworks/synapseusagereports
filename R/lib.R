#' @importFrom dplyr %>%
#' @importFrom lubridate %m+%

# Theme for plots
mytheme <- ggplot2::theme_bw() + ggplot2::theme(axis.text = ggplot2::element_text(size = 16),
                                                axis.title.x = ggplot2::element_text(size = 18),
                                                axis.title.y = ggplot2::element_text(size = 18, angle = 90))

# queryDict <- c('downloads'='select CLIENT,NORMALIZED_METHOD_SIGNATURE,PROJECT_ID,BENEFACTOR_ID,PARENT_ID,ENTITY_ID,AR.TIMESTAMP,RESPONSE_STATUS,DATE,USER_ID,NODE_TYPE,N.NAME from ACCESS_RECORD AR, PROCESSED_ACCESS_RECORD PAR, NODE_SNAPSHOT N, (select distinct ID from NODE_SNAPSHOT where PROJECT_ID = "%s") NODE where AR.TIMESTAMP Between %s AND %s and AR.SESSION_ID = PAR.SESSION_ID and AR.TIMESTAMP = PAR.TIMESTAMP and PAR.ENTITY_ID = NODE.ID and N.ID = NODE.ID and (PAR.NORMALIZED_METHOD_SIGNATURE = "GET /entity/#/file" or PAR.NORMALIZED_METHOD_SIGNATURE = "GET /entity/#/version/#/file");',
#                'webAccess'='select NORMALIZED_METHOD_SIGNATURE,PROJECT_ID,BENEFACTOR_ID,PARENT_ID,ENTITY_ID,CONVERT(AR.TIMESTAMP, CHAR) AS TIMESTAMP,RESPONSE_STATUS,DATE,USER_ID,NODE_TYPE,N.NAME from ACCESS_RECORD AR, PROCESSED_ACCESS_RECORD PAR, NODE_SNAPSHOT N, (select distinct ID from NODE_SNAPSHOT where PROJECT_ID = "%s") NODE where AR.TIMESTAMP Between %s AND %s and AR.SESSION_ID = PAR.SESSION_ID and AR.TIMESTAMP = PAR.TIMESTAMP and PAR.ENTITY_ID = NODE.ID and N.ID = NODE.ID and CLIENT = "WEB" AND (PAR.NORMALIZED_METHOD_SIGNATURE = "GET /entity/#/bundle" OR PAR.NORMALIZED_METHOD_SIGNATURE = "GET /entity/#/version/#/bundle" OR PAR.NORMALIZED_METHOD_SIGNATURE = "GET /entity/#/wiki2" OR PAR.NORMALIZED_METHOD_SIGNATURE = "GET /entity/#/wiki2/#");')

# doQuery <- function(con, template, projectId, beginTimestamp, endTimestamp) {
#   q.browse <- sprintf(template, projectId, beginTimestamp, endTimestamp)

#' @export
render_report <- function(project_id, team_order, data_file, reportType = "report") {

  templates <- c("report" = system.file("templates", "report.Rmd",
                                        package = "synapseusagereports"))

  myParams <- list(projectId = project_id,
                   teamOrder = team_order,
                   queryDataFile = data_file)

  htmlFileName <- paste0(myParams[['projectId']], "_", reportType, "_",
                         lubridate::today(), ".html")

  outputFileName <- paste0("/tmp/", htmlFileName)

  cat(rmarkdown::render(input = templates[[reportType]],
                        output_file = outputFileName,
                        params = myParams))

}

query_template_strings <- list("pageview" = 'select ENTITY_ID,CONVERT(AR.TIMESTAMP, CHAR) AS TIMESTAMP,DATE,USER_ID,NODE_TYPE,N.NAME from ACCESS_RECORD AR, PROCESSED_ACCESS_RECORD PAR, NODE_SNAPSHOT N, PROJECT_STATS NODE where AR.RESPONSE_STATUS=200 AND AR.TIMESTAMP > unix_timestamp("%s")*1000 AND AR.TIMESTAMP < unix_timestamp("%s")*1000 AND AR.SESSION_ID = PAR.SESSION_ID and AR.TIMESTAMP = PAR.TIMESTAMP and PAR.ENTITY_ID = NODE.ID AND N.ID = NODE.ID and N.TIMESTAMP = NODE.TIMESTAMP and CLIENT IN ("WEB", "UNKNOWN") AND (PAR.NORMALIZED_METHOD_SIGNATURE IN ("GET /entity/#/bundle", "GET /entity/#/version/#/bundle", "GET /entity/#/wiki2", "GET /entity/#/wiki2/#"));',
                               "download" = 'select ENTITY_ID,CONVERT(AR.TIMESTAMP, CHAR) AS TIMESTAMP,DATE,USER_ID,NODE_TYPE,N.NAME from ACCESS_RECORD AR, PROCESSED_ACCESS_RECORD PAR, NODE_SNAPSHOT N, PROJECT_STATS NODE where AR.TIMESTAMP > unix_timestamp("%s")*1000 AND AR.TIMESTAMP < unix_timestamp("%s")*1000 and (AR.RESPONSE_STATUS IN (200, 307)) AND AR.SESSION_ID = PAR.SESSION_ID and AR.TIMESTAMP = PAR.TIMESTAMP and PAR.ENTITY_ID = NODE.ID and N.ID = NODE.ID AND N.TIMESTAMP = NODE.TIMESTAMP and (PAR.NORMALIZED_METHOD_SIGNATURE IN ("GET /entity/#/file", "GET /entity/#/version/#/file"));',
                               "filedownloadrecord" = 'SELECT FDR.ASSOCIATION_OBJECT_ID AS ENTITY_ID, CONVERT(FDR.TIMESTAMP , CHAR) AS TIMESTAMP, DATE_FORMAT(from_unixtime(FDR.TIMESTAMP / 1000), "%%Y-%%m-%%d") AS DATE, FDR.USER_ID, N.NODE_TYPE, N.NAME FROM FILE_DOWNLOAD_RECORD FDR, NODE_SNAPSHOT N, PROJECT_STATS WHERE FDR.TIMESTAMP > unix_timestamp("%s")*1000 AND FDR.TIMESTAMP < unix_timestamp("%s")*1000 AND N.ID = PROJECT_STATS.ID AND PROJECT_STATS.ID = FDR.ASSOCIATION_OBJECT_ID AND FDR.ASSOCIATION_OBJECT_TYPE = "FileEntity" AND N.TIMESTAMP = PROJECT_STATS.TIMESTAMP;')

#' Get the SQL query template string.
#'
#' @param query_type The name of the SQL query to get.
#'
#' @return An SQL query string.
#' @export
#'
#' @examples
get_query_template_string <- function(query_type) {
  if (!(query_type %in% c("download", "pageview", "filedownloadrecord"))) {
    stop("Not a valid query type.")
  }

  return(query_template_strings[[query_type]])
}

#' @export
report_data_query <- function(con, project_id, query_type, start_date, end_date) {

  message(sprintf("Generating a %s report", query_type))

  project_id <- gsub("syn", "", project_id)

  timestampBreaksDf <- makeDateBreaksStartEnd(start_date, end_date) %>%
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

#' @export
doQuery <- function(con, template, projectId, start_date, end_date) {
  q <- sprintf(template, start_date, end_date)
  message(sprintf("Query: %s", q))
  message(sprintf("Querying %s to %s", start_date, end_date))

  res <- DBI::dbGetQuery(conn = con, statement = q)
  return(res)
}

#' @export
processQuery <- function(data) {
  queryData <- data %>%
    dplyr::rename(userId = USER_ID, id = ENTITY_ID) %>%
    dplyr::select(userId, id, DATE, TIMESTAMP, NODE_TYPE, NAME, recordType) %>%
    # dplyr::ungroup() %>%
    # rename(duplicateCount=n) %>%
    dplyr::mutate(date = as.Date(as.character(DATE)),
                  userId = as.character(userId),
                  id = as.character(id),
                  dateGrouping = lubridate::floor_date(date, unit = "month"),
                  monthYear = paste(lubridate::month(dateGrouping, label = TRUE),
                                    lubridate::year(dateGrouping))) %>%
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
                     function (x) doQuery(con = con,
                                          template = qTemplate,
                                          projectId = projectId,
					  start_date = x$start_date,
					  end_date = x$end_date
					  ))

  foo <- DBI::dbSendQuery(conn = con, statement = 'DROP TABLE PROJECT_STATS;')

  res
}

#' @export
getTeamMemberDF <- function(teamId) {

  foo <- synapser::synGetTeamMembers(teamId)
  foo <- foo$asList()

  foo %>% {
      tibble(teamId = purrr::map_chr(., 'teamId'),
             userId = purrr::map_chr(., c("member", "ownerId")))
    }

}

#' @export
processTeamMemberList <- function(teamIds) {
  # Get users from provided teamIds
  # Assign them in order of the provided team IDs
  userList <- purrr::map_df(teamIds, getTeamMemberDF)

  userList$teamId <- factor(userList$teamId,
                            levels = teamIds,
                            ordered = TRUE)

  userList <- userList %>%
    dplyr::group_by(userId) %>%
    dplyr::arrange(teamId) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  userList
}

chunk <- function(d, n) split(d, ceiling(seq_along(d)/n))

#' @export
getQueryUserProfiles <- function(queryData, useTeamGrouping, userList) {
  # Get user profile info for users in data download records


  accessUsers <- plyr::llply(chunk(unique(queryData$userId), 50),
                             function(x) synapser::synRestGET(sprintf("/userGroupHeaders/batch?ids=%s",
                                                                      paste(x, collapse = ",")))$children)

  accessUsersChildren <- do.call(c, accessUsers)

  allUsersList <- plyr::ldply(accessUsersChildren, as.data.frame) %>%
    dplyr::mutate(userId = ownerId) %>%
    dplyr::select(userId, userName)

  if (useTeamGrouping) {
    allUsers <- dplyr::left_join(allUsersList, userList)
  } else{
    allUsers <- allUsersList
    allUsers$teamId <- "Registered Synapse User"
  }

  allUsers$teamId <- forcats::fct_expand(factor(allUsers$teamId),
                                         "Anonymous",
                                         "Registered Synapse User")
  allUsers$teamId[is.na(allUsers$teamId)] <- "Registered Synapse User"
  allUsers$teamId[allUsers$userId == "273950"] <- "Anonymous"

  if (useTeamGrouping) {
    tmp_all_users <- allUsers %>%
      dplyr::filter(teamId != "Registered Synapse User",
                    teamId != "Anonymous") %>%
      dplyr::select(teamId) %>% dplyr::distinct(.keep_all=TRUE)

    teamInfo <- lapply(as.list(as.character(tmp_all_users$teamId)),
                       function(x) synapser::synGetTeam(x)) %>%
                       {
                         tibble(teamId = tmp_all_users$teamId,
                                teamName = purrr::map_chr(., 'name'))
                       }

    if (nrow(teamInfo) > 0) {
      allUsers <- dplyr::left_join(allUsers, teamInfo, by = "teamId")
    } else {
      allUsers$teamName <- "Registered Synapse User"
    }
  } else {
    allUsers$teamName <- "Registered Synapse User"
  }

  allUsers$teamName <- forcats::fct_expand(factor(allUsers$teamName),
                                           "Registered Synapse User",
                                           "Anonymous")
  allUsers$teamName[allUsers$userId == "273950"] <- "Anonymous"

  naTeamNames <- is.na(allUsers$teamName)

  allUsers$teamName <- forcats::fct_expand(allUsers$teamName,
                                           as.character(allUsers$teamId[naTeamNames]))

  allUsers$teamName[naTeamNames] <- allUsers$teamId[naTeamNames]

  allUsers
}

#' @export
countByMonth <- function(queryData, useTeamGrouping) {
  tmp <- queryData

  if (!useTeamGrouping) {
    tmp <- tmp %>% dplyr::mutate(teamName = 'All')
  }

  tmp %>%
    dplyr::count(teamName, dateGrouping) %>%
    reshape2::dcast(teamName ~ dateGrouping)

}

#' @export
countByDay <- function(queryData, useTeamGrouping) {
  tmp <- queryData

  if (!useTeamGrouping) {
    tmp <- tmp %>% dplyr::mutate(teamName = "All")
  }

  tmp %>%
    dplyr::count(teamName, date) %>%
    dplyr::arrange(n)

}

#' @export
plotByDay <- function(perdayCount, useTeamGrouping) {
  plotdata <- perdayCount %>%
    reshape2::dcast(date ~ teamName, value.var = 'n', fill = 0) %>%
    reshape2::melt(., id.vars = c("date"),
                   variable.name = "teamName", value.name = "n") %>%
    dplyr::rename(group = teamName)

  p <- ggplot2::ggplot(plotdata, ggplot2::aes(x = date, y = n))
  p <- p + ggplot2::geom_line(ggplot2::aes(group = group, color = group), size = 1)

  if (useTeamGrouping) {
    p <- p + ggplot2::scale_color_brewer(palette = "Set1")
  } else {
    p <- p + ggplot2::scale_color_manual(values = "black")
  }

  p  <- p + mytheme + ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                                     axis.text.x = ggplot2::element_text(size = 16, angle = 270),
                                     legend.position = "top")
  p

}

#' @export
uniqueUsersPerMonth <- function(queryData) {
  queryData %>%
    dplyr::select(userName, dateGrouping) %>%
    dplyr::distinct() %>%
    dplyr::filter(userName != "anonymous") %>%
    dplyr::group_by(dateGrouping) %>%
    dplyr::summarize(Users = n_distinct(userName)) %>%
    dplyr::rename(Date = dateGrouping)
}

#' @export
firstMonthToVisit <- function(queryData) {
  firstMonthVisit <- queryData %>%
    dplyr::filter(userName != "anonymous") %>%
    dplyr::select(userName, dateGrouping) %>%
    dplyr::distinct() %>%
    dplyr::group_by(userName)  %>%
    dplyr::arrange(dateGrouping) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(visit = 1) %>%
    dplyr::count(dateGrouping)

  missing <- unique(queryData$dateGrouping[!(queryData$dateGrouping %in% firstMonthVisit$dateGrouping)])

  if (length(missing) > 0) {
    firstMonthVisit <- rbind(firstMonthVisit,
                             data.frame(n = 0,
                                        dateGrouping = missing))
  }

  firstMonthVisit %>%
    dplyr::arrange(dateGrouping) %>%
    dplyr::rename(Date = dateGrouping, Users = n)
}

#' @export
multiMonthVisits <- function(queryData) {
  queryData %>%
    dplyr::group_by(userName) %>%
    dplyr::summarize(monthsVisited = n_distinct(dateGrouping)) %>%
    dplyr::filter(monthsVisited >= 2, userName != 'anonymous')
}

#' @export
makeDateBreaks <- function(nMonths) {
  thisDate <- lubridate::floor_date(lubridate::today(), "month") - lubridate::period(1, "months")

  beginDates <- thisDate - (lubridate::period(1, "months") * 0:(nMonths - 1))

  data.frame(date = beginDates, month = lubridate::month(beginDates), year = lubridate::year(beginDates))
    # # endDates <- beginDates + (lubridate::days_in_month(beginDates)) - lubridate::seconds(1)
  # # beginDates <- beginDates + lubridate::seconds(1)
  #
  # monthBreaksDf <- data.frame(beginDate=beginDates, endDate=endDates)
  #
  # monthBreaksDf %>%
  #   dplyr::mutate(beginTime=as.numeric(beginDate) * 1000,
  #                 endTime=as.numeric(endDate) * 1000)

}

#' @export
makeDateBreaksStartEnd <- function(start_date, end_date) {

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

#' @export
topNEntities <- function(queryData, allUsers, topN = 20) {
  plotdata <- queryData %>%
    dplyr::count(userName) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(allUsers) %>%
    dplyr::mutate(userName = reorder(userName, n, ordered = TRUE))

  plotdata %>%
    dplyr::top_n(topN, n) %>%
    dplyr::arrange(-n) %>%
    dplyr::select(n)
}
