# Theme for plots
mytheme <- ggplot2::theme_bw() + ggplot2::theme(axis.text=ggplot2::element_text(size=16),
                                                axis.title.x=ggplot2::element_text(size=18),
                                                axis.title.y=ggplot2::element_text(size=18, angle=90))

queryDict <- c('downloads'='select CLIENT,NORMALIZED_METHOD_SIGNATURE,PROJECT_ID,BENEFACTOR_ID,PARENT_ID,ENTITY_ID,AR.TIMESTAMP,RESPONSE_STATUS,DATE,USER_ID,NODE_TYPE,N.NAME from ACCESS_RECORD AR, PROCESSED_ACCESS_RECORD PAR, NODE_SNAPSHOT N, (select distinct ID from NODE_SNAPSHOT where PROJECT_ID = "%s") NODE where AR.TIMESTAMP Between %s AND %s and AR.SESSION_ID = PAR.SESSION_ID and AR.TIMESTAMP = PAR.TIMESTAMP and PAR.ENTITY_ID = NODE.ID and N.ID = NODE.ID and (PAR.NORMALIZED_METHOD_SIGNATURE = "GET /entity/#/file" or PAR.NORMALIZED_METHOD_SIGNATURE = "GET /entity/#/version/#/file");',
               'webAccess'='select NORMALIZED_METHOD_SIGNATURE,PROJECT_ID,BENEFACTOR_ID,PARENT_ID,ENTITY_ID,CONVERT(AR.TIMESTAMP, CHAR) AS TIMESTAMP,RESPONSE_STATUS,DATE,USER_ID,NODE_TYPE,N.NAME from ACCESS_RECORD AR, PROCESSED_ACCESS_RECORD PAR, NODE_SNAPSHOT N, (select distinct ID from NODE_SNAPSHOT where PROJECT_ID = "%s") NODE where AR.TIMESTAMP Between %s AND %s and AR.SESSION_ID = PAR.SESSION_ID and AR.TIMESTAMP = PAR.TIMESTAMP and PAR.ENTITY_ID = NODE.ID and N.ID = NODE.ID and CLIENT = "WEB" AND (PAR.NORMALIZED_METHOD_SIGNATURE = "GET /entity/#/bundle" OR PAR.NORMALIZED_METHOD_SIGNATURE = "GET /entity/#/version/#/bundle" OR PAR.NORMALIZED_METHOD_SIGNATURE = "GET /entity/#/wiki2" OR PAR.NORMALIZED_METHOD_SIGNATURE = "GET /entity/#/wiki2/#");')

# doQuery <- function(con, template, projectId, beginTimestamp, endTimestamp) {
#   q.browse <- sprintf(template, projectId, beginTimestamp, endTimestamp)
doQuery <- function(con, template, projectId, month, year) {
  q.browse <- sprintf(template, projectId, month, year)

  data <- DBI::dbGetQuery(conn = con, statement=q.browse) %>% 
    dplyr::rename(userid=USER_ID, id=ENTITY_ID)
  
  data %>% dplyr::filter(RESPONSE_STATUS == 200)  %>% 
    dplyr::count(userid, id, DATE, TIMESTAMP, NODE_TYPE, NAME) %>% 
    dplyr::ungroup()
  
}

getData <- function(con, qTemplate, projectId, timestampBreaksDf) {
  
  #res <- plyr::ddply(timestampBreaksDf, plyr::.(beginTime, endTime),
  res <- plyr::ddply(timestampBreaksDf, plyr::.(month, year),
                                        function (x) doQuery(con=con,
                                          template=qTemplate, 
                                          projectId=projectId, 
                                          month=x$month,
                                          year=x$year))
  # beginTimestamp=x$beginTime, 
  # endTimestamp=x$endTime))

  queryData <- res %>%
    dplyr::mutate(date=as.Date(as.character(DATE)),
                  userId=as.character(userid), 
                  dateGrouping=lubridate::floor_date(date, unit="month"),
                  monthYear=paste(lubridate::month(dateGrouping, label=TRUE),
                                  lubridate::year(dateGrouping)))
  
  # Get unique due to folder, file name changes
  # Might not be most recent name!
  queryData <- queryData %>% 
    dplyr::group_by(id, userid, TIMESTAMP) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup()
  
  queryData
}

getTeamMemberDF <- function(teamId) {
  userListREST <- synapseClient::synRestGET(sprintf("/teamMembers/%s?limit=500", teamId))
  userList <- plyr::ldply(userListREST$results,
                          function(x) data.frame(userId=as.character(x$member$ownerId), 
                                                 teamId=as.character(x$teamId)))
  userList
}

aclToMemberList <- function(acl) {
  aclMemberList <- plyr::ldply(acl@resourceAccess@content, 
                               function(x) data.frame(principalId=as.character(x@principalId),
                                                      teamId=acl@id))
  
  userGroupHeaders <- synapseClient::synRestGET(sprintf("/userGroupHeaders/batch?ids=%s", 
                                                        paste(aclMemberList$principalId, 
                                                              collapse=",")))
  
  plyr::ldply(userGroupHeaders$children, as.data.frame)
  
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

getQueryUserProfiles <- function(queryData, useTeamGrouping, aclUserList) {
  # Get user profile info for users in data download records
  accessUsers <- synapseClient::synRestGET(sprintf("/userGroupHeaders/batch?ids=%s", 
                                                   paste(unique(queryData$userId), 
                                                         collapse=",")))
  
  allUsersList <- plyr::ldply(accessUsers$children, as.data.frame) %>% 
    dplyr::mutate(userId=ownerId) %>% 
    dplyr::select(userId, userName)
  
  if (useTeamGrouping) {
    allUsers <- dplyr::left_join(allUsersList, aclUserList)
  } else{
    allUsers <- allUsersList
    allUsers$teamId <- "None"
  }
  
  allUsers$teamId <- forcats::fct_expand(factor(allUsers$teamId), "Anonymous", "None")
  allUsers$teamId[is.na(allUsers$teamId)] <- "None"
  allUsers$teamId[allUsers$userId == "273950"] <- "Anonymous"
  
  if (useTeamGrouping) {
    teamInfo <- plyr::ddply(allUsers %>% 
                              dplyr::filter(teamId != "None", teamId != "Anonymous",
                                            !startsWith(as.character(allUsers$teamId), 
                                                        "syn")) %>%
                              dplyr::select(teamId) %>% dplyr::distinct(),
                      .(teamId),
                      function(x) {
                        tmp <- synapseClient::synRestGET(sprintf("/team/%s", x$teamId)); 
                        data.frame(teamId=x$teamId, teamName=tmp$name)
                      }
    )
    if (nrow(teamInfo) > 0) {
      allUsers <- dplyr::left_join(allUsers, teamInfo, by="teamId")
    } else {
      allUsers$teamName <- "None"
    }
  } else {
    allUsers$teamName <- "None"
  }
  
  allUsers$teamName <- forcats::fct_expand(factor(allUsers$teamName), "None")
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
    dplyr::rename(Date=dateGrouping, `New Users`=n)
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

  data.frame(date=beginDates, month=lubridate::month(beginDates), year=lubridate::year(beginDates))
    # # endDates <- beginDates + (lubridate::days_in_month(beginDates)) - lubridate::seconds(1)
  # # beginDates <- beginDates + lubridate::seconds(1)
  # 
  # monthBreaksDf <- data.frame(beginDate=beginDates, endDate=endDates)
  # 
  # monthBreaksDf %>% 
  #   dplyr::mutate(beginTime=as.numeric(beginDate) * 1000,
  #                 endTime=as.numeric(endDate) * 1000)
  
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