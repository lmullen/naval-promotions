#!/usr/bin/env Rscript --vanilla

library(lubridate)
library(ggplot)
library(plyr)

options(stringsAsFactors = FALSE)

data <- read.csv("data/naval-promotions.csv")
