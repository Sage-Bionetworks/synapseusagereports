templates <- c("webAccess"="../webAccess.Rmd", 
               "downloads"="../downloads.Rmd")

### MEP-LINCS
projectId <- 'syn2862345'
myParams <- list(projectId=projectId, 
                 nMonths=23, 
                 aclTeamOrder=c(3323597, 3330234, 3332397, projectId), 
                 useTeamGrouping=TRUE)

reportType <- "webAccess"
rmarkdown::render(input=templates[[reportType]],
                  output_file=paste0(myParams[['projectId']], "_", reportType, "_",
                                     lubridate::today(), ".html"),
                  params = myParams)

reportType <- 'downloads'
rmarkdown::render(input=templates[[reportType]],
                  output_file=paste0(myParams[['projectId']], "_", reportType, "_",
                                     lubridate::today(), ".html"),
                  params = myParams)
