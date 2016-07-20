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

aclToUserList <- function(id) {
  acl <- synGetEntityACL(id)
  
  aclUserList <- ldply(acl@resourceAccess@content, 
                       function(x) data.frame(principalId=as.character(x@principalId),
                                              teamId="syn2862345"))
  
  
  userList <- ldply(aclUserList$principalId, getTeamMemberDF)
  
  userList2 <- aclUserList %>% 
    filter(!(principalId %in% userList$userId)) %>% 
    dplyr::rename(userId=principalId)
  
  rbind(userList, userList2)
  
}