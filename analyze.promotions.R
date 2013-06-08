#!/usr/bin/env Rscript --vanilla

library(lubridate)
library(ggplot2)
library(plyr)

options(stringsAsFactors = FALSE)

# Load and calculate derivative data
# -------------------------------------------------------------------
data <- read.csv("data/naval-promotions.csv")

# Calculate the time to promotion from midshipman to lieutenant and from
# midshipman to captain. The time to promotion is recorded as the number of
# days. Assumption: we are not going to worry about people who come into the
# navy above the rank of midshipman, since they are not relevant to our
# historical question anyway.
promotions <- transform(data,
  year.midshipman = year(ymd(date.midshipman)),
  time.to.lt = as.double(ymd(date.lieutenant) - ymd(date.midshipman)),
  time.to.capt = as.double(ymd(date.captain) - ymd(date.midshipman)))


# Question: how did length of time to promotion change over time?
# -------------------------------------------------------------------
# Select just the people who became captains
captains <- subset(promotions, time.to.capt != "NA")

png(filename = "outputs/time.to.captain.png",
 height=1200, width=2000, res = 300)
captain.plot <- ggplot(data = captains, 
  aes(x = cut(year.midshipman,15), y = time.to.capt)) +
  geom_boxplot()
print(captain.plot)
dev.off()