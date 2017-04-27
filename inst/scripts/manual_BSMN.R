templates <- c("webAccess"="../webAccess.Rmd", 
               "downloads"="../downloads.Rmd")

projectId <- 'syn7344947'
myParams <- list(projectId=projectId, 
                 nMonths=1, 
                 aclTeamOrder=c(3338598, projectId), 
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
