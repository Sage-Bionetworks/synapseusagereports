library(synapseClient)
synapseLogin()

templates <- c("report"="../report.Rmd")
reportType <- "report"

projectId <- 'syn1773109'
parentId <- 'syn4892835'

myParams <- list(projectId=projectId, 
                 nMonths=NA,
                 aclTeamOrder=c(2224090, 3319054, 273957, projectId), 
                 useTeamGrouping=FALSE)

htmlFileName <- paste0(myParams[['projectId']], "_", reportType, "_",
                       lubridate::today(), ".html")

rmarkdown::render(input=templates[[reportType]],
                  output_file=htmlFileName,
                  params = myParams)

htmlFile <- synStore(File(paste0("../", htmlFileName),
                          name="Usage Statistics",
                          parentId=parentId))
