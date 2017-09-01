library(synapseClient)
synapseLogin()
source("../lib.R")

reportType <- "report"
projectId <- 'syn1773109'
parentId <- 'syn4892835'

myParams <- list(projectId=projectId, 
                 nMonths=NA,
                 aclTeamOrder=c(2224090, 3319054, 273957, projectId), 
                 useTeamGrouping=FALSE)

generateAndStore(myParams, reportType)