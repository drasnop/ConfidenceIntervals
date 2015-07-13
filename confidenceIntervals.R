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
print(datac)

# plot 95% Confidence Intervals
# of course, we need to de-log transform the data
plot <- ggplot(datac, aes(x=interface, y=exp(shortDuration)-1, fill=interface)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=exp(shortDuration-ci)-1, ymax=exp(shortDuration+ci)-1),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
print(plot)

