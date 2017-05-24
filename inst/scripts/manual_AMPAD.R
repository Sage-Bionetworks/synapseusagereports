library(synapseClient)
synapseLogin()

templates <- c("webAccess"="../webAccess.Rmd", 
               "downloads"="../downloads.Rmd",
	       "report"="../report.Rmd")

### AMP-AD
projectId <- 'syn2580853'
reportType <- "report"

# Store HTML file here
parentId <- 'syn8457451'

myParams <- list(projectId=projectId, 
                 nMonths=30,
                 aclTeamOrder=c(3346847, 3320424, projectId), 
                 useTeamGrouping=TRUE)

htmlFileName <- paste0(myParams[['projectId']], "_", reportType, "_",
                       lubridate::today(), ".html")

rmarkdown::render(input=templates[[reportType]],
                  output_file=htmlFileName,
                  params = myParams)

htmlFile <- synStore(File(paste0("../", htmlFileName), parentId=parentId))
