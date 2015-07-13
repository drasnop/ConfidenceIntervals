library("ggplot2")

# require helpers functions
source("helpers.R")

# import data from csv
data <- read.csv("C:/Users/Antoine/programmation/web/stencil-analysis/1-20-pax/trials-1-20-pax.csv")

# make sure the independent variable is a factor
data$interface <- factor(data$interface)

# log-transform durations, to obtain a normal distribution
data$shortDuration <- log(1+data$shortDuration)

# data is already in the "long" format
# if it were in the "wide" format, use tidyr gather() or reshape2 melt()

# compute the mean and CI iteratively, increasing the number of trials every step
evolution <- data.frame(interface=numeric(),subset=numeric(),shortDuration=numeric(),ci=numeric())

for(n in 2:10) {
  partial_data <- subset(data, trialNumber < n)
  
  # aggreagate the data, and compute SD, SE and 95% CI
  datac <- summarySE(partial_data, "shortDuration", "interface")
  
  # put this data into a dataframe, indicating which subset it was computed from
  for(i in 1:4){
    evolution[nrow(evolution)+1,] <- c(datac[i,"interface"],n,datac[i,"shortDuration"],datac[i,"ci"])
  }
}

evolution$interface <- factor(evolution$interface)
print(evolution)

# plot mean + 95% Confidence Intervals as the size of the subset increases
# of course, we need to de-log transform the data
plot <- ggplot(evolution, aes(x=subset, y=exp(shortDuration)-1, colour=interface)) + 
  geom_errorbar(aes(ymin=exp(shortDuration-ci)-1, ymax=exp(shortDuration+ci)-1), width=.1, position=position_dodge()) +
  geom_line(position=position_dodge()) +
  geom_point(position=position_dodge(), size=3)

print(plot)

# plot the size of the confidence intervals as the size of the subset increases
plot2 <- ggplot(evolution, aes(x=subset, y=ci, colour=interface)) +
  geom_line(position=position_dodge()) +
  geom_point(position=position_dodge(), size=3)
print(plot2)
