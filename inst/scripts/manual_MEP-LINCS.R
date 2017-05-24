templates <- c("report"="../report.Rmd")

### MEP-LINCS
projectId <- 'syn2862345'
# Store HTML file here
parentId <- 'syn5578879'

myParams <- list(projectId=projectId, 
                 nMonths=NA,
                 aclTeamOrder=c(3323597, 3330234, 3332397, projectId),
                 useTeamGrouping=TRUE)

reportType <- "report"

htmlFileName <- paste0(myParams[['projectId']], "_", reportType, "_",
                       lubridate::today(), ".html")

rmarkdown::render(input=templates[[reportType]],
                  output_file=htmlFileName,
                  params = myParams)

htmlFile <- synStore(File(paste0("../", htmlFileName), 
                          parentId=parentId))
