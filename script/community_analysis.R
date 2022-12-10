library(tidyverse)
library(readxl)

dat <- readxl::read_excel("data/community/community_listening_data.xlsx")

huh <- dplyr::filter(dat, INDIV_ID %in% c(7, 12))

