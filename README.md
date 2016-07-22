## Information

Generate HTML usage reports for Synapse projects.

## Requirements

1. R >= 3.3.0
1. RStudio

## Installation

Clone this repository and install the following packages:

```
install.packages("shiny", "rmarkdown", "plyr", "dplyr", "reshape", "data.table", "xtable", "knitr", "ggplot2", "scales", "stringr", "synapseClient", "RMySQL", "yaml", "lubridate")
```

## Usage

Set your working directory to the root of the project and run the Shiny app at `inst/shiny-apps/UsageStats/app.R`:

```
shiny::runApp('inst/shiny-apps/UsageStats/')
```
