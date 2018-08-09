#!/usr/bin/zsh
course_id=54317
firefox "https://lms.latrobe.edu.au/course/view.php?id=$course_id" &
sleep 30
firefox "https://lms.latrobe.edu.au/report/log/index.php?chooselog=1&showusers=0&showcourses=0&id=$course_id&group=&user=&date=&modid=&modaction=&edulevel=-1&logreader=logstore_standard" &
sleep 20
firefox "https://lms.latrobe.edu.au/grade/export/txt/index.php?id=$course_id"&






