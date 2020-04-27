#' @importFrom dplyr %>%
#' @importFrom lubridate %m+%

# Lookup of report templates. Assumes these are in the 'templates' subdirectory of the synapseusagereports package.
# Used by the 'render_report' function.
# It's not safe to use 'system.file' here as of R 3.6 (https://developer.r-project.org/Blog/public/2019/02/14/staged-install/index.html)
templates <- c("report" = "report.Rmd")

#' Utility to generate a report HTML file using the template.
#'
#' @param project_id Synapse Project ID. This should match what is in the data file.
#' @param team_order List of Synapse team IDs in the order to assign users to.
#' @param data_file CSV of data from the output of the 'report_data_query' function.
#' @param reportType The report type to generate.
#' @export
render_report <- function(project_id, team_order, data_file, reportType = "report", outputDir="/tmp/") {

  templateRmdName <- templates[[reportType]]

  templateFile <- system.file("templates", templateRmdName,
                              package = "synapseusagereports")

  myParams <- list(projectId = project_id,
                   teamOrder = team_order,
                   queryDataFile = data_file)

  htmlFileName <- paste0(myParams[['projectId']], "_", reportType, "_",
                         lubridate::today(), ".html")

  outputFileName <- paste0(outputDir, htmlFileName)

  cat(rmarkdown::render(input = templateFile,
                        output_file = outputFileName,
                        params = myParams))

}

#' Get a data frame of user IDs from a Synapse Team.
#'
#' @param teamId Synapse Team ID
#'
#' @return A data frame with teamId and userId columns.
#'
#' @export
getTeamMemberDF <- function(teamId) {

  foo <- synapser::synGetTeamMembers(teamId)
  foo <- foo$asList()

  foo %>% {
    tibble(teamId = purrr::map_chr(., 'teamId'),
           userId = purrr::map_chr(., c("member", "ownerId")))
  }

}

#' Get users from provided teamIds
#' If a user is in multiple teams, get the team provided first in the teamIds list.
#'
#' @param teamIds Vector of Synapse Team IDs.
#'
#' @return Data frame with teamId and userId columns with one team per user ID.
#'
#' @export
processTeamMemberList <- function(teamIds) {
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

#' Split a vector into chuncks of size n
chunk <- function(d, n) split(d, ceiling(seq_along(d)/n))

#' Get user profile info for users in data download records
#'
#' @param queryData Data from the 'report_data_query' function - must have a column 'userId' that has Synapse User IDs.
#' @param useTeamGrouping boolean indicating if the 'userList' should be used for grouping individuals into teams. If FALSE, all users are listed in a team called 'Registered Synapse Users'.
#'
#' @export
getQueryUserProfiles <- function(queryData, useTeamGrouping, userList) {

  # Split call into 50 users, as this is max of the API endpoint.
  accessUsers <- plyr::llply(chunk(unique(queryData$userId), 50),
                             function(x) synapser::synRestGET(sprintf("/userGroupHeaders/batch?ids=%s",
                                                                      paste(x, collapse = ",")))$children)

  accessUsersChildren <- do.call(c, accessUsers)

  allUsersList <- plyr::ldply(accessUsersChildren, as.data.frame) %>%
    dplyr::mutate(userId = ownerId) %>%
    dplyr::select(userId, userName)

  if (useTeamGrouping) {
    allUsers <- dplyr::left_join(allUsersList, userList)
  } else {
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
