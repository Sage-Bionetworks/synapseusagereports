config <- yaml.load_file("mysql_config.yml")

projectId <- gsub("syn", "", params$projectId)
nMonths <- params$nMonths
useTeamGrouping <- params$useTeamGrouping
aclTeamOrder <- params$aclTeamOrder

con <- dbConnect(MySQL(),
                 user = config$username,
                 password = config$password,
                 host = config$host,
                 dbname='warehouse')

endDate <- as.POSIXct(Sys.Date(), origin="1970-01-01", tz="PST")
endTimestamp <- as.numeric(endDate) * 1000

monthBreaks <- as.POSIXct(unlist(lapply(0:nMonths, function(x) endDate - months(x))),
                          origin="1970-01-01")

monthBreaksDf <- data.frame(beginTime=monthBreaks[2:(nMonths + 1)],
                            endTime=monthBreaks[1:nMonths])

timestampBreaksDf <- monthBreaksDf %>% 
  mutate(beginTime=as.numeric(beginTime) * 1000,
         endTime=as.numeric(endTime) * 1000)
