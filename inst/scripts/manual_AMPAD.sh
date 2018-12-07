#!/usr/bin/env bash

project_id="syn2580853"
team_order='273957,3377637,3346847,3320424'

# 6 months ago
start_date=$(date --date='-6 months' -I -u)

# today
end_date=`date -u -I`

./report_data_query.R --project_id=${project_id} --start_date=${start_date} --end_date=${end_date} --config_file=~/datawarehouse-before-aug.yml > /tmp/${project_id}_${end_date}.csv
./render_report.R  --project_id=${project_id} --team_order=${team_order} /tmp/${project_id}_${end_date}.csv
