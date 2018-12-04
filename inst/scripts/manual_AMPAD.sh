#!/usr/bin/env bash

n_months=$1

project_id="syn2580853"
# parentId="syn8457451"

# 6 months ago
nmonthago='TZ="UTC" -$6 months'
start_date=$(date --date='TZ="UTC" -$n_months months' -I)

# today
end_date=`date --date='TZ="UTC"' -I`

./report_data_query.R --project_id=${project_id} --start_date=${start_date} --end_date=${end_date} --config_file=~/datawarehouse_config.yml
