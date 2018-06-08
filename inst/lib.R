qPageviewTemplate <- 'select ENTITY_ID,CONVERT(AR.TIMESTAMP, CHAR) AS TIMESTAMP,DATE,USER_ID,NODE_TYPE,N.NAME from ACCESS_RECORD AR, PROCESSED_ACCESS_RECORD PAR, NODE_SNAPSHOT N, PROJECT_STATS NODE where AR.RESPONSE_STATUS=200 AND AR.TIMESTAMP > unix_timestamp("%s")*1000 AND AR.TIMESTAMP < unix_timestamp("%s")*1000 AND AR.SESSION_ID = PAR.SESSION_ID and AR.TIMESTAMP = PAR.TIMESTAMP and PAR.ENTITY_ID = NODE.ID AND N.ID = NODE.ID and N.TIMESTAMP = NODE.TIMESTAMP and CLIENT IN ("WEB", "UNKNOWN") AND (PAR.NORMALIZED_METHOD_SIGNATURE IN ("GET /entity/#/bundle", "GET /entity/#/version/#/bundle", "GET /entity/#/wiki2", "GET /entity/#/wiki2/#"));'

qDownloadTemplate <- 'select ENTITY_ID,CONVERT(AR.TIMESTAMP, CHAR) AS TIMESTAMP,DATE,USER_ID,NODE_TYPE,N.NAME from ACCESS_RECORD AR, PROCESSED_ACCESS_RECORD PAR, NODE_SNAPSHOT N, PROJECT_STATS NODE where AR.TIMESTAMP > unix_timestamp("%s")*1000 AND AR.TIMESTAMP < unix_timestamp("%s")*1000 and (AR.RESPONSE_STATUS IN (200, 307)) AND AR.SESSION_ID = PAR.SESSION_ID and AR.TIMESTAMP = PAR.TIMESTAMP and PAR.ENTITY_ID = NODE.ID and N.ID = NODE.ID AND N.TIMESTAMP = NODE.TIMESTAMP and (PAR.NORMALIZED_METHOD_SIGNATURE IN ("GET /entity/#/file", "GET /entity/#/version/#/file"));'

qFDRTemplate <- 'SELECT FDR.ASSOCIATION_OBJECT_ID AS ENTITY_ID, CONVERT(FDR.TIMESTAMP, CHAR) AS TIMESTAMP, DATE_FORMAT(from_unixtime(FDR.TIMESTAMP / 1000), "%%Y-%%m-%%d") AS DATE, FDR.USER_ID, N.NODE_TYPE, N.NAME FROM FILE_DOWNLOAD_RECORD FDR, NODE_SNAPSHOT N, PROJECT_STATS WHERE FDR.TIMESTAMP > unix_timestamp("%s")*1000 AND FDR.TIMESTAMP < unix_timestamp("%s")*1000 AND N.ID = PROJECT_STATS.ID AND PROJECT_STATS.ID = FDR.ASSOCIATION_OBJECT_ID AND FDR.ASSOCIATION_OBJECT_TYPE = "FileEntity" AND N.TIMESTAMP = PROJECT_STATS.TIMESTAMP;'

queryTemplates <- list('pageViewTemplate'=qPageviewTemplate,
                       'downloadTemplate'=qDownloadTemplate,
                       'FDRTemplate'=qFDRTemplate)

#' Do a query of project usage stats by month.
#' 
#' This is a limitation of the data warehouse requiring queries to be run within a month.
#'
#' @param conn A database connection
#' @param templateName Name of the template to use
#' @param projectId A Synapse Project ID
#' @param date A date
#' @param verbose 
#'
#' @return
#' @export
#'
#' @examples
doQueryMonth <- function(conn, templateName, projectId, startDate, verbose=FALSE) {
  if (verbose) {
    message(sprintf("%s", startDate))  
  }
  
  template <- queryTemplates[[templateName]]
  
  q.browse <- sprintf(template, startDate, 
                      lubridate::add_with_rollback(startDate, months(1)))

  DBI::dbGetQuery(conn = conn, statement=q.browse)
  
}

processUsageStatsQueryData <- function(data) {

  queryData <- data %>% 
    dplyr::rename(userid=USER_ID, id=ENTITY_ID) %>% 
    dplyr::select(userid, id, DATE, TIMESTAMP, NODE_TYPE, NAME, recordType) %>% 
    dplyr::mutate(date=as.Date(as.character(DATE)),
                  userId=as.character(userid), 
                  dateGrouping=lubridate::floor_date(date, unit="month"),
                  monthYear=paste(lubridate::month(dateGrouping, label=TRUE),
                                  lubridate::year(dateGrouping))) %>%  
    dplyr::group_by(id, userid, TIMESTAMP, recordType) %>% # Get unique due to name changes, might not be most recent name!
    dplyr::arrange(TIMESTAMP) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup()
  
  allUsers <- getQueryUserProfiles(queryData, useTeamGrouping, aclUserList)
  
  queryData <- queryData %>% left_join(., allUsers)
  
  queryData
}

createTempTable <- function(conn, projectId, parentIds=NULL, tableName="PROJECT_STATS") {

  if (is.null(parentIds)) {
    q.create_temp <- "CREATE TEMPORARY TABLE %s SELECT ID, MAX(TIMESTAMP) AS TIMESTAMP FROM NODE_SNAPSHOT WHERE PROJECT_ID = %s GROUP BY ID;"
    statement <- sprintf(q.create_temp, tableName, projectId)
  } else {
    parentIdsSQL <- sprintf("(%s)", paste(parentIds, collapse=","))
    q.create_temp <- "CREATE TEMPORARY TABLE %s SELECT ID, MAX(TIMESTAMP) AS TIMESTAMP FROM NODE_SNAPSHOT WHERE PROJECT_ID = %s AND PARENT_ID IN %s GROUP BY ID;"
    statement <- sprintf(q.create_temp, tableName, projectId, parentIdsSQL)
  }
  
  create <- DBI::dbSendQuery(conn=conn,
                             statement=statement)
}

dropTempTable <- function(conn, tableName="PROJECT_STATS") {
  DBI::dbSendQuery(conn=conn, statement=sprintf('DROP TABLE %s;', tableName))
}

getData <- function(conn, templateName, projectId, timestampBreaksDf, parentIds=NULL,
                    tempTableName="PROJECT_STATS") {
  maxDate <- max(timestampBreaksDf$date)
  
  create <- createTempTable(conn=conn, projectId=projectId, parentIds=parentIds,
                            tableName=tempTableName)

  res <- plyr::ddply(timestampBreaksDf, plyr::.(month, year),
                     function (x) doQueryMonth(conn=conn,
                                               templateName=templateName, 
                                               projectId=projectId, 
                                               startDate=x$date))
  
  # res <- tryCatch(plyr::ddply(timestampBreaksDf, plyr::.(month, year),
  #                             function (x) doQueryMonth(conn=conn,
  #                                                       templateName=templateName, 
  #                                                       projectId=projectId, 
  #                                                       date=x$date)),
  #                 error=function(e) dropTempTable(conn=conn, tableName=tempTableName))
  
  dropTempTable(conn=conn)
  
  res
}

getTeamMemberDF <- function(teamId) {
  totalNumberOfResults <- 1000
  offset <- 0
  limit <- 50
  userListREST <- list()
  
  while(offset<totalNumberOfResults) {
    result <- synapseClient::synRestGET(sprintf("/teamMembers/%s?limit=%s&offset=%s", 
                                                teamId, limit, offset))
    
    totalNumberOfResults <- result$totalNumberOfResults
    
    userListREST <- c(userListREST, result$results)
    
    offset <- offset + limit
  }

  userList <- plyr::ldply(userListREST,
                          function(x) data.frame(userId=as.character(x$member$ownerId),
                                                 teamId=as.character(x$teamId)))
  userList
}

aclToMemberList <- function(acl) {
  aclMemberList <- plyr::ldply(acl@resourceAccess@content, 
                               function(x) data.frame(principalId=as.character(x@principalId),
                                                      teamId=acl@id))
  
  accessUsers <- plyr::llply(chunk(aclMemberList$principalId, 50),
                             function(x) synapseClient::synRestGET(sprintf("/userGroupHeaders/batch?ids=%s",
                                                                           paste(x, collapse=",")))$children)
  
  userGroupHeaders <- do.call(c, accessUsers)
  
  plyr::ldply(userGroupHeaders, as.data.frame)
  
}

aclToUserList <- function(synId) {
  acl <- synapseClient::synGetEntityACL(synId)
  
  aclMemberList <- aclToMemberList(acl)
  aclMemberList$teamId <- synId

  userList <- plyr::ldply(aclMemberList$ownerId, getTeamMemberDF)
  
  userList2 <- aclMemberList %>% 
    dplyr::filter(isIndividual) %>%
    dplyr::rename(userId=ownerId)
    
  rbind(userList2[, c("userId", "teamId")], userList)
  
}

processAclUserList <- function(projectId, aclTeamOrder) {
  # Get users at project level and select the team
  # they are on dependent on the ordering in aclTeamOrder
  aclUserList <- aclToUserList(paste0("syn", projectId))
  aclUserList$teamId <- factor(aclUserList$teamId, 
                               levels=aclTeamOrder,
                               ordered=TRUE)
  
  aclUserList <- aclUserList %>%
    dplyr::group_by(userId) %>% 
    dplyr::arrange(teamId) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup()
  
  aclUserList
}

chunk <- function(d, n) split(d, ceiling(seq_along(d)/n))

getQueryUserProfiles <- function(queryData, useTeamGrouping, aclUserList) {
  # Get user profile info for users in data download records


  accessUsers <- plyr::llply(chunk(unique(queryData$userId), 50),
                             function(x) synapseClient::synRestGET(sprintf("/userGroupHeaders/batch?ids=%s",
                                                                           paste(x, collapse=",")))$children)

  accessUsersChildren <- do.call(c, accessUsers)

  allUsersList <- plyr::ldply(accessUsersChildren, as.data.frame) %>%
    dplyr::mutate(userId=ownerId) %>%
    dplyr::select(userId, userName)

  if (useTeamGrouping) {
    allUsers <- dplyr::left_join(allUsersList, aclUserList)
  } else{
    allUsers <- allUsersList
    allUsers$teamId <- "Registered Synapse User"
  }

  allUsers$teamId <- forcats::fct_expand(factor(allUsers$teamId), 
                                         "Anonymous", "Registered Synapse User")
  allUsers$teamId[is.na(allUsers$teamId)] <- "Registered Synapse User"
  allUsers$teamId[allUsers$userId == "273950"] <- "Anonymous"

  if (useTeamGrouping) {
    teamInfo <- plyr::ddply(allUsers %>%
                              dplyr::filter(teamId != "Registered Synapse User", 
                                            teamId != "Anonymous",
                                            !startsWith(as.character(allUsers$teamId),
                                                        "syn")) %>%
                              dplyr::select(teamId) %>% dplyr::distinct(),
                      plyr::.(teamId),
                      function(x) {
                        tmp <- synapseClient::synRestGET(sprintf("/team/%s", x$teamId));
                        data.frame(teamId=x$teamId, teamName=tmp$name)
                      }
    )
    if (nrow(teamInfo) > 0) {
      allUsers <- dplyr::left_join(allUsers, teamInfo, by="teamId")
    } else {
      allUsers$teamName <- "Registered Synapse User"
    }
  } else {
    allUsers$teamName <- "Registered Synapse User"
  }

  allUsers$teamName <- forcats::fct_expand(factor(allUsers$teamName), "Registered Synapse User")
  naTeamNames <- is.na(allUsers$teamName)

  allUsers$teamName <- forcats::fct_expand(allUsers$teamName,
                                           as.character(allUsers$teamId[naTeamNames]))

  allUsers$teamName[naTeamNames] <- allUsers$teamId[naTeamNames]

  allUsers
}

countByMonth <- function(queryData, useTeamGrouping) {
  tmp <- queryData
  
  if (!useTeamGrouping) {
    tmp <- tmp %>% dplyr::mutate(teamName='All')
  }

  tmp %>%
    dplyr::count(teamName, dateGrouping) %>% 
    reshape2::dcast(teamName ~ dateGrouping)
  
}

countByDay <- function(queryData, useTeamGrouping) {
  tmp <- queryData
  
  if (!useTeamGrouping) {
    tmp <- tmp %>% dplyr::mutate(teamName="All")
  }

  tmp %>%
    dplyr::count(teamName, date) %>% 
    dplyr::arrange(n)
  
}

plotByDay <- function(perdayCount, useTeamGrouping) {
  plotdata <- perdayCount %>% 
    reshape2::dcast(date ~ teamName, value.var='n', fill=0) %>% 
    reshape2::melt(., id.vars=c("date"), 
                   variable.name="teamName", value.name="n") %>% 
    dplyr::rename(group=teamName)
  
  p <- ggplot2::ggplot(plotdata, ggplot2::aes(x=date, y=n))
  p <- p + ggplot2::geom_line(ggplot2::aes(group=group, color=group), size=1)
  
  if (useTeamGrouping) {
    p <- p + ggplot2::scale_color_brewer(palette = "Set1")
  } else {
    p <- p + ggplot2::scale_color_manual(values="black")
  }

  p  <- p + mytheme + ggplot2::theme(axis.title.x=ggplot2::element_blank(),
                                     axis.text.x=ggplot2::element_text(size=16, angle=270),
                                     legend.position="top")
  p
  
}
uniqueUsersPerMonth <- function(queryData) {
  queryData %>%
    dplyr::select(userName, dateGrouping) %>% 
    dplyr::distinct() %>% 
    dplyr::filter(userName != "anonymous") %>% 
    dplyr::group_by(dateGrouping) %>% 
    dplyr::summarize(Users=n_distinct(userName)) %>% 
    dplyr::rename(Date=dateGrouping)
}

firstMonthToVisit <- function(queryData) {
  firstMonthVisit <- queryData %>%
    dplyr::filter(userName != "anonymous") %>% 
    dplyr::select(userName, dateGrouping) %>% 
    dplyr::distinct() %>% 
    dplyr::group_by(userName)  %>% 
    dplyr::arrange(dateGrouping) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(visit=1) %>%
    dplyr::count(dateGrouping)
  
  missing <- unique(queryData$dateGrouping[!(queryData$dateGrouping %in% firstMonthVisit$dateGrouping)])
  
  if (length(missing) > 0) {
    firstMonthVisit <- rbind(firstMonthVisit,
                             data.frame(n=0, 
                                        dateGrouping=missing))
  }
  
  firstMonthVisit %>% 
    dplyr::arrange(dateGrouping) %>% 
    dplyr::rename(Date=dateGrouping, Users=n)
}

multiMonthVisits <- function(queryData) {
  queryData %>%
    dplyr::group_by(userName) %>% 
    dplyr::summarize(monthsVisited=n_distinct(dateGrouping)) %>% 
    dplyr::filter(monthsVisited >= 2, userName != 'anonymous')
}

makeDateBreaks <- function(nMonths) {
  thisDate <- lubridate::floor_date(lubridate::today(), "month")- lubridate::period(1, "months")
  
  beginDates <- thisDate - (lubridate::period(1, "months") * 0:(nMonths - 1))

  data.frame(date=beginDates, 
             month=lubridate::month(beginDates), 
             year=lubridate::year(beginDates))

}

topNEntities <- function(queryData, allUsers, topN=20) {
  plotdata <- queryData %>%
    dplyr::count(userName) %>%
    dplyr::ungroup() %>% 
    dplyr::left_join(allUsers) %>% 
    dplyr::mutate(userName=reorder(userName, n, ordered=TRUE))
  
  plotdata %>% 
    dplyr::top_n(topN, n) %>% 
    dplyr::arrange(-n) %>% 
    dplyr::select(n)
}

REPORT_TEMPLATES <- c("report"="../report.Rmd")

generateAndStore <- function(myParams, reportType) {
  htmlFileName <- paste0(myParams[['projectId']], "_", reportType, "_",
                         lubridate::today(), ".html")
  
  outputFileName <- paste0(tempdir(), "/", htmlFileName)
  
  rmarkdown::render(input=REPORT_TEMPLATES[[reportType]],
                    output_file=outputFileName,
                    params=myParams)
  
  synStore(File(outputFileName,
                name="Usage Statistics", 
                parentId=parentId))
}
