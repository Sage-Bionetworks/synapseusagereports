templates <- c("webAccess"="../webAccess.Rmd", 
               "downloads"="../downloads.Rmd")

### MEP-LINCS
projectId <- 'syn2580853'
reportType <- "webAccess"

myParams <- list(projectId=projectId, 
                 nMonths=28, 
                 aclTeamOrder=c(3346847, projectId), 
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
