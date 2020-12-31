library(tidyverse)

temp <- tempfile()
download.file("https://www.pbs.gov.au/downloads/2020/12/2020-12-01-v3extracts.zip", temp)
unzip(temp, exdir = "data")

files <- dir("data/")

biologics_list <- c("tocilizumab", "infliximab", "adalimumab",
                   "etanercept", "abatacept", "rituximab",
                   "baricitinib", "tofacitinib", "upadacitinib",
                   "ustekinumab", "ixekizumab", "secukinumab")

streamline <- read_tsv("data/streamlined_20201201.txt")

drug <- read_delim("data/drug_20201201.txt", delim = "!")

link <- read_tsv("data/LinkExtract_20201201.txt")

restriction <- read_tsv("data/RestrictionExtractDelimited_20201201.txt")

biologics <- drug %>% 
  filter(str_detect(`mp-pt`, str_c(biologics_list, collapse = "|")))

biologics %>% 
  select(`item-code`, atc,`brand-name`, `mp-pt`, `tpuu-or-mpp-pt`, mq, repeats, `pack-size`) %>% 
  inner_join(link, by = "item-code") %>% 
  inner_join(restriction)

//*[@id="1.1251"]/ul[1]/li[1]/a[1]
//*[@id="1.1251"]/ul[1]/li[1]/a