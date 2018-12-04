library(synapseClient)
library(synapseusagereports)

synapseLogin()

templates <- c("report"=system.file("templates", "report.Rmd",
                                    package = "synapseusagereports"))
reportType <- "report"

projectId <- 'syn2862345'
parentId <- 'syn5578879'

myParams <- list(projectId=projectId,
                 nMonths=NA,
                 aclTeamOrder=c(3323597, 3330234, 3332397, projectId),
                 useTeamGrouping=TRUE,
                 dataOutput=paste0(projectId, "_", reportType, "_",
                                   lubridate::today(), ".csv"))

htmlFileName <- paste0(myParams[['projectId']], "_", reportType, "_",
                       lubridate::today(), ".html")

outputFileName <- paste0(tempdir(), "/", htmlFileName)

output_file <- rmarkdown::render(input=templates[[reportType]],
                                 output_file=outputFileName,
                                 params=myParams)

htmlFile <- synStore(File(output_file,
                          name="Usage Statistics",
                          parentId=parentId))
