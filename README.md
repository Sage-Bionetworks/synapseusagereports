## Information

This R package generates HTML usage reports for Synapse projects. 

This tool uses the Synapse Data Warehouse database to run queries and pull REST API access logs for Synapse Projects. To gain access to the data warehouse, please file an IT Jira ticket.

The tool can be used to query for three distinct types of user interactions: downloads (using simple single file REST API endpoints), file download records (using batch and bulk download REST API endpoints), and page views (viewing of Wiki pages). **The use of this tool for counting page views is highly discouraged, as changes to how page loads are optimized, cached, and changed over time can affect the query results, making comparisons over time difficult.**. The use of Google Analytics is a better solution, but is only available on a per-page basis.

Since Synapse entities can be constantly changing, including things like their names, a temporary table is [created](https://github.com/Sage-Bionetworks/synapseusagereports/blob/master/R/lib.R#L145) that retrieves the Synapse IDs of the most recent snapshot of Synapse. This is limited by the Synapse Project ID specified. The queries that are run against the data warehouse are defined [here](https://github.com/Sage-Bionetworks/synapseusagereports/blob/master/R/lib.R#L36-L38).

## Installation

```
devtools::install_github("Sage-Bionetworks/synapseusagereports")
```

## Usage

Scripts are in the [inst/scripts](inst/scripts) directory.

See [example-db-config.yml](example-db-config.yml) for an example configuration file to connect to the warehouse database.

### Query the warehouse

```
report_data_query.R --project_id=syn1234567 --start_date=2018-01-01 --end_date=2018-03-01 --config_file=example-db-config.yml > output.csv
```

### Generate a report

```
render_report.R  --project_id=syn1234567 --team_order='273957' output.csv
```
