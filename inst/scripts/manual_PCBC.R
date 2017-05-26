library(synapseClient)
library(synapseProjectUsageStatistics)

synapseLogin()

templates <- c("report"=system.file("templates", "report.Rmd",
                                    package = "synapseProjectUsageStatistics"))
reportType <- "report"

projectId <- 'syn1773109'
parentId <- 'syn4892835'

myParams <- list(projectId=projectId,
                 nMonths=NA,
                 aclTeamOrder=c(2224090, 3319054, 273957, projectId),
                 useTeamGrouping=FALSE)

htmlFileName <- paste0(myParams[['projectId']], "_", reportType, "_",
                       lubridate::today(), ".html")

outputFileName <- paste0(tempdir(), "/", htmlFileName)

rmarkdown::render(input=templates[[reportType]],
                  output_file=outputFileName,
                  params=myParams)

htmlFile <- synStore(File(outputFileName,
                          name="Usage Statistics", parentId=parentId))
