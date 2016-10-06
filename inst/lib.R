doQuery <- function(con, template, projectId, beginTimestamp, endTimestamp) {
  q.browse <- sprintf(template, projectId, beginTimestamp, endTimestamp)
  
  data <- dbGetQuery(conn = con, statement=q.browse) %>% 
    dplyr::rename(userid=USER_ID, id=ENTITY_ID)
  
  data %>% filter(RESPONSE_STATUS == 200)  %>% 
    count(userid, id, DATE, TIMESTAMP, NODE_TYPE, NAME) %>% 
    ungroup()
  
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
    count(dateGrouping)
  
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
