library(tidyverse)
library(OECD)

oecd_datasets <-get_datasets()

#LFS
unemploy <- search_dataset('health')

browse_metadata('DUR_I')

#useful source:  http://www.oecd.org/els/emp/LFS%20Definitions%20-%20Tables.pdf

#Health
health <- search_dataset('health')

browse_metadata('HEALTH_STAT')

#http://stats.oecd.org/wbos/fileview2.aspx?IDFile=3627d99d-fb9f-40e1-98a0-20f19c80ac5f

#Income
income <- search_dataset('income')

browse_metadata('IDD')
#http://www.oecd.org/els/soc/IDD-Metadata.pdf

#business
business <- search_dataset('business')

browse_metadata('MEI_BTS_COS')

#https://stats.oecd.org/Index.aspx?DataSetCode=MEI_BTS_COS

