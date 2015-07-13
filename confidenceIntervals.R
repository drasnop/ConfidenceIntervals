library("ggplot2")

# require helpers functions
source("helpers.R")

# import data from csv
data <- read.csv("C:/Users/Antoine/programmation/web/stencil-analysis/1-20-pax/trials-1-20-pax.csv")

# log-transform durations, to obtain a normal distribution
data$shortDuration <- log(1+data$shortDuration)

# make sure the independent variable is a factor
data$interface <- factor(data$interface)

# data is already in the "long" format
# if it were in the "wide" format, use tidyr gather() or reshape2 melt()

# aggreagate the data, and compute SD, SE and 95% CI
datac <- summarySE(data, "shortDuration", "interface")

# compute explicitely the lower and upper of the CI
datac$lci <- datac$shortDuration - datac$ci
datac$uci <- datac$shortDuration + datac$ci

# de-log transform the data, computing the lower and upper bounds of the CI
delog <- function(x) {exp(x)-1}
datac[c("shortDuration")] <- lapply(datac[c("shortDuration")], delog)
datac[c("lci")] <- lapply(datac[c("lci")], delog)
datac[c("uci")] <- lapply(datac[c("uci")], delog)

print(datac)

# plot 95% Confidence Intervals
# of course, we need to de-log transform the data
plot <- ggplot(datac, aes(x=interface, y=shortDuration, fill=interface)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=lci, ymax=uci),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
print(plot)

