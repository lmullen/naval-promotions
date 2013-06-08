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
  time.to.capt = as.double(ymd(date.captain) - ymd(date.midshipman))/365.25
  )

# Remove people who did not enter as midshipmen 
promotions <- subset(promotions, !is.na(promotions$year.midshipman))

# Question: How did length of time to promotion change over time?
# -------------------------------------------------------------------
# Select just the people who became captains
captains <- subset(promotions, !is.na(promotions$time.to.capt))

# Plot the time to captain by year of being commissioned a midshipman
png(filename = "outputs/time.to.captain.png",
 height=1200, width=2000, res = 300)
time.captain.plot <- ggplot(data = captains, 
  aes(x = cut(year.midshipman, pretty(year.midshipman, 12)), y = time.to.capt)) +
  geom_boxplot() +
  ggtitle("Time to Promotion to Captain by Cohort") +
  xlab("Year commissioned as midshipman") +
  theme(axis.text.x=element_text(angle=25, hjust=1)) +
  ylab("Years till promotion")
print(time.captain.plot)
dev.off()

# Select just the people who became lieutenants
lieutenants <- subset(promotions, !is.na(promotions$time.to.lt))

# Plot the time to lieutenant by year of being commissioned a midshipman
png(filename = "outputs/time.to.lieutenant.png",
 height=1200, width=2000, res = 300)
time.lieutenant.plot <- ggplot(data = lieutenants, 
  aes(x = cut(year.midshipman, pretty(year.midshipman, 12)), y = time.to.lt)) +
  geom_boxplot() +
  ggtitle("Time to Promotion to Lieutenant by Cohort") +
  xlab("Year commissioned as midshipman") +
  theme(axis.text.x=element_text(angle=25, hjust=1)) +
  ylab("Years till promotion")
print(time.lieutenant.plot)
dev.off()

# Question: how did likelihood of promotion change over time?
# -------------------------------------------------------------------
# Bar chart of how many became captains and who did not
png(filename = "outputs/likelihood.captain.png",
 height=1200, width=2000, res = 300)
likelihood.captain.plot <- ggplot(data = promotions,
  aes(x = cut(year.midshipman, pretty(year.midshipman, 12)),
    fill = factor(is.na(time.to.capt)))) +
  geom_bar() +
  scale_fill_discrete(labels = c("Captain","Not captain")) +
  ggtitle("Midshipmen Promoted to Captain by Cohort") +
  xlab("Year commissioned as midshipman") +
  ylab(NULL) +
  theme(axis.text.x=element_text(angle=35, hjust=1)) +
  theme(legend.title=element_blank())
print (likelihood.captain.plot)
dev.off()

# Bar chart of how many became lieutenants and who did not
png(filename = "outputs/likelihood.lieutenant.png",
 height=1200, width=2000, res = 300)
likelihood.lieutenant.plot <- ggplot(data = promotions,
  aes(x = cut(year.midshipman, pretty(year.midshipman, 12)),
    fill = factor(is.na(time.to.lt)))) +
  geom_bar() +
  scale_fill_discrete(labels = c("Lieutenant","Not lieutenant")) +
  ggtitle("Midshipmen Promoted to Lieutenant by Cohort") +
  xlab("Year commissioned as midshipman") +
  ylab(NULL) +
  theme(axis.text.x=element_text(angle=35, hjust=1)) +
  theme(legend.title=element_blank())
print (likelihood.lieutenant.plot)
dev.off()
