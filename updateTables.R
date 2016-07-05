library(redshift)
library(yaml)

config <- yaml.load_file("redshift_config.yml")

conn <- redshift.connect(sprintf("jdbc:postgresql://%s:%s/%s", config$host, config$port, config$db),
                         config$username, config$password)

# Create tables for October
dbSendUpdate(conn, statement=paste("CREATE TABLE get_repoentity_201510",
                                   "AS SELECT DISTINCT * FROM raw_access_record",
                                   "WHERE timestamp >= 1443657600000", 
                                   "AND timestamp <= 1446336000000",
                                   "AND requesturl LIKE '/repo/v1/entity/syn%'",
                                   "AND method='GET'"))

# Create tables for September
dbSendUpdate(conn, statement=paste("CREATE TABLE get_repoentity_201509",
                                   "AS SELECT DISTINCT * FROM raw_access_record",
                                   "WHERE timestamp >= 1441090800000", 
                                   "AND timestamp <= 1443682800000",
                                   "AND requesturl LIKE '/repo/v1/entity/syn%'",
                                   "AND method='GET'"))


# Create tables for August
dbRemoveTable(conn, "get_repoentity_201508")

dbSendUpdate(conn, statement=paste("CREATE TABLE get_repoentity_201508",
                                   "AS SELECT DISTINCT * FROM raw_access_record",
                                   "WHERE timestamp >= 1438412400000", 
                                   "AND timestamp <= 1441090800000",
                                   "AND requesturl LIKE '/repo/v1/entity/syn%'",
                                   "AND method='GET'"))


# Create tables for July
dbSendUpdate(conn, statement=paste("CREATE TABLE get_repoentity_201507",
                                   "AS SELECT DISTINCT * FROM raw_access_record",
                                   "WHERE timestamp >= 1435734000000", 
                                   "AND timestamp < 1438412400000",
                                   "AND requesturl LIKE '/repo/v1/entity/syn%'",
                                   "AND method='GET'"))

## Remove old tables
dbRemoveTable(conn, "get_repoentity_2015")

dbSendUpdate(conn, statement=paste("CREATE TABLE get_repoentity_2015",
                                   "AS SELECT DISTINCT * FROM raw_access_record",
                                   "WHERE timestamp >= 1420099200000",
                                   "AND requesturl LIKE '/repo/v1/entity/syn%'",
                                   "AND method='GET'"))
