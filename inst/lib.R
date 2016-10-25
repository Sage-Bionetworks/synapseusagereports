# Theme for plots
mytheme <- theme_bw() + theme(axis.text=element_text(size=16),
                              axis.title.x=element_text(size=18),
                              axis.title.y=element_text(size=18, angle=90))

doQuery <- function(con, template, projectId, beginTimestamp, endTimestamp) {
  q.browse <- sprintf(template, projectId, beginTimestamp, endTimestamp)
  
  data <- dbGetQuery(conn = con, statement=q.browse) %>% 
    dplyr::rename(userid=USER_ID, id=ENTITY_ID)
  
  data %>% filter(RESPONSE_STATUS == 200)  %>% 
    dplyr::count(userid, id, DATE, TIMESTAMP, NODE_TYPE, NAME) %>% 
    ungroup()
  
}

getData <- function(con, qTemplate, projectId, timestampBreaksDf) {
  
  res <- ddply(timestampBreaksDf, .(beginTime, endTime),
               function (x) doQuery(con=con,
                                    template=qTemplate, 
                                    projectId=projectId, 
                                    beginTimestamp=x$beginTime, 
                                    endTimestamp=x$endTime))
  
  queryData <- res %>%
    mutate(date=as.Date(as.character(DATE)),
           userId=as.character(userid), 
           dateGrouping=floor_date(date, unit="month"),
           monthYear=paste(lubridate::month(dateGrouping, label=TRUE),
                           lubridate::year(dateGrouping)))
  
  # Get unique due to folder, file name changes
  # Might not be most recent name!
  queryData <- queryData %>% group_by(id, userid, TIMESTAMP) %>% slice(1) %>% ungroup()
  
  queryData
}

getTeamMemberDF <- function(teamId) {
  userListREST <- synRestGET(sprintf("/teamMembers/%s?limit=500", teamId))
  userList <- ldply(userListREST$results,
                    function(x) data.frame(userId=as.character(x$member$ownerId), 
                                           teamId=as.character(x$teamId)))
  userList
}

aclToMemberList <- function(acl) {
  aclMemberList <- ldply(acl@resourceAccess@content, 
                         function(x) data.frame(principalId=as.character(x@principalId),
                                                teamId=acl@id))
  
  userGroupHeaders <- synRestGET(sprintf("/userGroupHeaders/batch?ids=%s", 
                                         paste(aclMemberList$principalId, 
                                               collapse=",")))
  
  ldply(userGroupHeaders$children, as.data.frame)
  
}

aclToUserList <- function(synId) {
  acl <- synGetEntityACL(synId)
  
  aclMemberList <- aclToMemberList(acl)
  aclMemberList$teamId <- synId

  userList <- ldply(aclMemberList$ownerId, getTeamMemberDF)
  
  userList2 <- aclMemberList %>% 
    filter(isIndividual) %>%
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
    group_by(userId) %>% 
    arrange(teamId) %>% 
    slice(1) %>% 
    ungroup()
  
  aclUserList
}

getQueryUserProfiles <- function(queryData, useTeamGrouping, aclUserList) {
  # Get user profile info for users in data download records
  accessUsers <- synRestGET(sprintf("/userGroupHeaders/batch?ids=%s", 
                                    paste(unique(queryData$userId), 
                                          collapse=",")))
  
  allUsersList <- ldply(accessUsers$children, as.data.frame) %>% 
    mutate(userId=ownerId) %>% 
    select(userId, userName)
  
  if (useTeamGrouping) {
    allUsers <- left_join(allUsersList, aclUserList)
  } else{
    allUsers <- allUsersList
    allUsers$teamId <- "None"
  }
  
  allUsers$teamId <- fct_expand(factor(allUsers$teamId), "Anonymous", "None")
  allUsers$teamId[is.na(allUsers$teamId)] <- "None"
  allUsers$teamId[allUsers$userId == "273950"] <- "Anonymous"
  
  if (useTeamGrouping) {
    teamInfo <- ddply(allUsers %>% 
                        filter(teamId != "None", teamId != "Anonymous",
                               !startsWith(as.character(allUsers$teamId), 
                                           "syn")) %>%
                        select(teamId) %>% unique(),
                      .(teamId),
                      function(x) {
                        tmp <- synRestGET(sprintf("/team/%s", x$teamId)); 
                        data.frame(teamId=x$teamId, teamName=tmp$name)
                      }
    )
    if (nrow(teamInfo) > 0) {
      allUsers <- left_join(allUsers, teamInfo, by="teamId")
    } else {
      allUsers$teamName <- "None"
    }
  } else {
    allUsers$teamName <- "None"
  }
  
  allUsers$teamName <- fct_expand(factor(allUsers$teamName), "None")
  naTeamNames <- is.na(allUsers$teamName)
  
  allUsers$teamName <- fct_expand(allUsers$teamName,
                                  as.character(allUsers$teamId[naTeamNames]))
  
  allUsers$teamName[naTeamNames] <- allUsers$teamId[naTeamNames]
  
  allUsers
}

countByMonth <- function(queryData, useTeamGrouping) {
  if (useTeamGrouping) {
    dateGroupingCount <- queryData %>%
      dplyr::count(teamName, dateGrouping) %>% 
      reshape2::dcast(teamName ~ dateGrouping)
  } else {
    dateGroupingCount <- queryData %>%
      mutate(teamName='All') %>% 
      dplyr::count(teamName, dateGrouping) %>% 
      reshape2::dcast(teamName ~ dateGrouping)
  }
  
  
}

countByDay <- function(queryData, useTeamGrouping) {
  tmp <- queryData

    if (!useTeamGrouping) {
    tmp <- tmp %>% mutate(teamName="All")
  }

  tmp %>%
    dplyr::count(teamName, date) %>% 
    arrange(n)
  
}

plotByDay <- function(perdayCount, useTeamGrouping) {
  plotdata <- perdayCount %>% 
    reshape2::dcast(date ~ teamName, value.var='n', fill=0) %>% 
    reshape::melt(., id.vars=c("date"), 
                  variable.name="teamName", value.name="n") %>% 
    dplyr::rename(group=variable, n=value)
  
  p <- ggplot(plotdata, aes(x=date, y=n))
  p <- p + geom_line(aes(group=group, color=group), size=1)
  
  if (useTeamGrouping) {
    p <- p + scale_color_brewer(palette = "Set1")
  } else {
    p <- p + scale_color_manual(values="black")
  }

  p  <- p + mytheme + theme(axis.title.x=element_blank(),
                            axis.text.x=element_text(size=16, angle=270),
                            legend.position="top")
  p
  
}
uniqueUsersPerMonth <- function(queryData) {
  queryData %>%
    select(userName, dateGrouping) %>% 
    distinct() %>% 
    filter(userName != "anonymous") %>% 
    group_by(dateGrouping) %>% 
    summarize(Users=n_distinct(userName)) %>% 
    dplyr::rename(Date=dateGrouping)
}

firstMonthToVisit <- function(queryData) {
  firstMonthVisit <- queryData %>%
    filter(userName != "anonymous") %>% 
    select(userName, dateGrouping) %>% 
    distinct() %>% 
    group_by(userName)  %>% 
    arrange(dateGrouping) %>% 
    slice(1) %>% ungroup() %>% 
    mutate(visit=1) %>%
    dplyr::count(dateGrouping)
  
  missing <- unique(queryData$dateGrouping[!(queryData$dateGrouping %in% firstMonthVisit$dateGrouping)])
  
  if (length(missing) > 0) {
    firstMonthVisit <- rbind(firstMonthVisit,
                             data.frame(n=0, 
                                        dateGrouping=missing))
  }
  
  firstMonthVisit %>% 
    arrange(dateGrouping) %>% 
    dplyr::rename(Date=dateGrouping, `New Users`=n)
}

multiMonthVisits <- function(queryData) {
  queryData %>%
    group_by(userName) %>% 
    summarize(monthsVisited=n_distinct(dateGrouping)) %>% 
    filter(monthsVisited >= 2, userName != 'anonymous')
}

makeDateBreaks <- function(nMonths) {
  endDate <- floor_date(today(), "month") + seconds(1)
  # endDate <- as.POSIXct(Sys.Date(), origin="1970-01-01", tz="PST")
  endTimestamp <- as.numeric(floor_date(endDate, "second")) * 1000
  
  monthBreaks <- as.POSIXct(endDate - months(0:nMonths),
                            origin="1970-01-01")
  
  monthBreaksDf <- data.frame(beginDate=monthBreaks[2:(nMonths + 1)],
                              endDate=monthBreaks[1:nMonths])
  
  monthBreaksDf %>% 
    mutate(beginTime=as.numeric(beginDate) * 1000,
           endTime=as.numeric(endDate) * 1000)
  
}