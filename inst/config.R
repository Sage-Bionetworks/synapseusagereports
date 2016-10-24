library(lubridate)

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

endDate <- floor_date(today(), "month") + seconds(1)
# endDate <- as.POSIXct(Sys.Date(), origin="1970-01-01", tz="PST")
endTimestamp <- as.numeric(floor_date(endDate, "second")) * 1000

monthBreaks <- as.POSIXct(endDate - months(0:nMonths),
                          origin="1970-01-01")

monthBreaksDf <- data.frame(beginTime=monthBreaks[2:(nMonths + 1)],
                            endTime=monthBreaks[1:nMonths])

timestampBreaksDf <- monthBreaksDf %>% 
  mutate(beginTime=as.numeric(beginTime) * 1000,
         endTime=as.numeric(endTime) * 1000)
