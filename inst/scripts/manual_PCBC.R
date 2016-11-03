templates <- c("webAccess"="../webAccess.Rmd", 
               "downloads"="../downloads.Rmd")

projectId <- 'syn1773109'
myParams <- list(projectId=projectId, 
                 nMonths=6, 
                 aclTeamOrder=c(2224090, 3319054, 273957, projectId), 
                 useTeamGrouping=FALSE)

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
