#!/bin/sh

echo -n "Post name > "
read -r REPLY
title=${REPLY}
clean_title=`echo $title | tr "[:upper:]" "[:lower:]"]` #Lower Case
clean_title=`echo $clean_title | iconv -f utf-8 -t ascii//translit` #Remove accents
clean_title=`echo $clean_title | tr -dc "[a-z0-9 ]"` #Keep spaces, letters and numbers
clean_title=`echo $clean_title | tr " " "-"` #Replace spaces by dashes

filename=$clean_title.md
author=`(git config --get user.name)`
date_pattern=`(date "+%Y-%m-%d")`
cur_date=$date_pattern
cat > "posts/"$filename <<EOF
---
title: $title
author: $author
date: $cur_date
tags:
---
EOF

# vim "posts/"$filename