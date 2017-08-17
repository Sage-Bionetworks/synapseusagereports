library(synapseClient)
synapseLogin()

templates <- c("report"="../report.Rmd")
reportType <- "report"

res <- synTableQuery("select id from syn10142562")

parentId <- 'syn10322050'

for (id in res@values$id[1:3]) {
  
  message(sprintf("Processing Project %s", id))
  
  projectId <- id
  
  myParams <- list(projectId=projectId, 
                   nMonths=NA,
                   aclTeamOrder=c(),
                   useTeamGrouping=FALSE)
  
  htmlFileName <- paste0(myParams[['projectId']], "_", reportType, "_",
                         lubridate::today(), ".html")
  
  rmarkdown::render(input=templates[[reportType]],
                    output_file=htmlFileName,
                    params = myParams)
  
  htmlFile <- synStore(File(paste0("../", htmlFileName),
                            parentId=parentId))
}