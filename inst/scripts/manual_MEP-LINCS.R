templates <- c("webAccess"="./inst/webAccess.Rmd", 
               "downloads"="./inst/downloads.Rmd")

### MEP-LINCS
projectId <- 'syn2862345'
reportType <- "webAccess"
myParams <- list(projectId=projectId, 
                 nMonths=2, 
                 aclTeamOrder=c(3323597, 3330234, 3332397, projectId), 
                 useTeamGrouping=TRUE)

rmarkdown::render(input=templates[[reportType]],
                  output_file=paste0(myParams[['projectId']], "_", reportType, "_",
                                     lubridate::today(), ".html"),
                  params = myParams)

reportType <- 'downloads'
rmarkdown::render(input=templates[[reportType]],
                  output_file=paste0(myParams[['projectId']], "_", reportType, "_",
                                     lubridate::today(), ".html"),
                  params = myParams))

### End AMP-AD
