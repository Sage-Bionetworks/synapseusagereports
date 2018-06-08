library(synapseClient)
synapseLogin()

templates <- c("report"="../report.Rmd")
reportType <- "report"

projectId <- 'syn2580853'
parentId <- 'syn8457451'

myParams <- list(projectId=projectId, 
                 nMonths=NA,
                 aclTeamOrder=c(3346847, 3320424, projectId), 
                 useTeamGrouping=TRUE,
                 dataOutput=paste0(projectId, "_", reportType, "_",
                                   lubridate::today(), ".csv"))

htmlFileName <- paste0(myParams[['projectId']], "_", reportType, "_",
                       lubridate::today(), ".html")

rmarkdown::render(input=templates[[reportType]],
                  output_file=htmlFileName,
                  params = myParams)

htmlFile <- synStore(File(paste0("../", htmlFileName),
                          name="Usage Statistics",
                          parentId=parentId))
