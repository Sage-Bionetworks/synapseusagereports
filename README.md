## Information

Generate HTML usage reports for Synapse projects.

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
