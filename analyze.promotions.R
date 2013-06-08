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
# years. Assumption: we are not going to worry about people who come into the
# navy above the rank of midshipman, since they are not relevant to our
# historical question anyway.
promotions <- transform(data,
  year.midshipman = year(ymd(date.midshipman)),
  time.to.lt = as.double(ymd(date.lieutenant) - ymd(date.midshipman))/365.25,
  time.to.capt = as.double(ymd(date.captain) - ymd(date.midshipman))/365
  )



# Question: how did length of time to promotion change over time?
# -------------------------------------------------------------------
# Select just the people who became captains
captains <- subset(promotions, time.to.capt != "NA")

# Plot the time to captain by year of being commissioned a midshipman
png(filename = "outputs/time.to.captain.png",
 height=1200, width=2000, res = 300)
captain.plot <- ggplot(data = captains, 
  aes(x = cut(year.midshipman, pretty(year.midshipman, 12)), y = time.to.capt)) +
  geom_boxplot() +
  ggtitle("Time to Promotion to Captain by Generation") +
  xlab("Year commissioned as midshipman") +
  theme(axis.text.x=element_text(angle=25, hjust=1)) +
  ylab("Years till promotion")
print(captain.plot)
dev.off()

# Select just the people who became captains
lieutenants <- subset(promotions, time.to.lt != "")

# Plot the time to captain by year of being commissioned a midshipman
png(filename = "outputs/time.to.lieutenant.png",
 height=1200, width=2000, res = 300)
lieutenant.plot <- ggplot(data = lieutenants, 
  aes(x = cut(year.midshipman, pretty(year.midshipman, 12)), y = time.to.lt)) +
  geom_boxplot() +
  ggtitle("Time to Promotion to Lieutenant by Generation") +
  xlab("Year commissioned as midshipman") +
  theme(axis.text.x=element_text(angle=25, hjust=1)) +
  ylab("Years till promotion")
print(lieutenant.plot)
dev.off()