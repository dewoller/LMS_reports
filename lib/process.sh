#!/bin/sh   
firefox "https://lms.latrobe.edu.au/grade/export/txt/index.php?id=56433"
sleep 20
firefox "https://lms.latrobe.edu.au/grade/export/txt/index.php?id=57107"
firefox "https://lms.latrobe.edu.au/report/log/index.php?chooselog=1&showusers=0&showcourses=0&id=57107&group=&user=&date=&modid=&modaction=&edulevel=-1&logreader=logstore_standard"
firefox "https://lms.latrobe.edu.au/report/log/index.php?chooselog=1&showusers=0&showcourses=0&id=56433&group=&user=&date=&modid=&modaction=&edulevel=-1&logreader=logstore_standard"

sleep 60

cd  /mnt/sda5/code/R/teaching/processLMSLogs

Rscript -e 'workflowr::wflow_publish("analysis/processIHBLogs.Rmd")'
Rscript -e 'workflowr::wflow_publish("analysis/processHSILogs.Rmd")'


