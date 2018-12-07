#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(synapser))
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(synapseusagereports))

option_list <- list(
  make_option(c("--project_id"), type = "character",
              help = "Synapse Project ID.",
              dest = "project_id",
              metavar = "synapseid"),
  make_option(c("--team_order"), type = "character",
              help = "comma separated list of teams.",
              dest = "team_order",
              default = ''),
  make_option(c("--synapse_config"), type = "character",
              help = "Synapse configuration file.",
              dest = "synapse_config",
              default="~/.synapseConfig")
)

arguments <- parse_args(OptionParser(usage = "%prog [options] file",
                                     option_list = option_list),
                        positional_arguments = 1)

opts <- arguments$options
data_file <- arguments$args

login <- capture.output(synLogin())

render_report(project_id = opts$project_id,
              team_order = opts$team_order,
              data_file = data_file)
