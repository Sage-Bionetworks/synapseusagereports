templates <- c("webAccess"="../webAccess.Rmd", 
               "downloads"="../downloads.Rmd",
	       "report"="../report.Rmd")

### AMP-AD
projectId <- 'syn2580853'
reportType <- "webAccess"

myParams <- list(projectId=projectId, 
                 nMonths=26,
                 aclTeamOrder=c(3346847, 3320424, projectId), 
                 useTeamGrouping=TRUE)

reportType <- "report"
rmarkdown::render(input=templates[[reportType]],
                  output_file=paste0(myParams[['projectId']], "_", reportType, "_",
                                     lubridate::today(), ".html"),
                  params = myParams)
