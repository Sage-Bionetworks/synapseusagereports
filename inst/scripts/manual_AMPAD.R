library(synapseClient)
library(synapseProjectUsageStatistics)

synapseLogin()

templates <- c("report"=system.file("templates", "report.Rmd",
                                    package = "synapseProjectUsageStatistics"))
reportType <- "report"

projectId <- 'syn2580853'
parentId <- 'syn8457451'

myParams <- list(projectId=projectId,
                 nMonths=NA,
                 aclTeamOrder=c(3346847, 3320424, projectId),
                 useTeamGrouping=TRUE)

htmlFileName <- paste0(myParams[['projectId']], "_", reportType, "_",
                       lubridate::today(), ".html")

outputFileName <- paste0(tempdir(), "/", htmlFileName)

rmarkdown::render(input=templates[[reportType]],
                  output_file=outputFileName,
                  params=myParams)

htmlFile <- synStore(File(outputFileName,
                          name="Usage Statistics", parentId=parentId))
