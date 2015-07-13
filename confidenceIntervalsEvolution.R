library("ggplot2")

# require helpers functions
source("helpers.R")

# import data from csv
data <- read.csv("C:/Users/Antoine/programmation/web/stencil-analysis/1-20-pax/trials-1-20-pax.csv")

# make sure the independent variable is a factor
data$interfaceType <- factor(data$interfaceType)

# log-transform durations, to obtain a normal distribution
data$shortDuration <- log(1+data$shortDuration)

# data is already in the "long" format
# if it were in the "wide" format, use tidyr gather() or reshape2 melt()

# compute the mean and CI iteratively, increasing the number of trials every step
evolution <- data.frame(interfaceType=numeric(),subset=numeric(),shortDuration=numeric(),ci=numeric())

for(n in 2:10) {
  partial_data <- subset(data, trialNumber < n)
  
  # aggreagate the data, and compute SD, SE and 95% CI
  datac <- summarySE(partial_data, "shortDuration", "interfaceType")
  
  # put this data into a dataframe, indicating which subset it was computed from
  for(i in 1:nlevels(data$interfaceType)){
    evolution[nrow(evolution)+1,] <- c(datac[i,"interfaceType"],n,datac[i,"shortDuration"],datac[i,"ci"])
  }
}

evolution$interfaceType <- factor(evolution$interfaceType)

# compute explicitely the lower and upper of the CI
evolution$lci <- evolution$shortDuration - evolution$ci
evolution$uci <- evolution$shortDuration + evolution$ci

# de-log transform the data, computing the lower and upper bounds of the CI
delog <- function(x) {exp(x)-1}
evolution[c("shortDuration")] <- lapply(evolution[c("shortDuration")], delog)
evolution[c("lci")] <- lapply(evolution[c("lci")], delog)
evolution[c("uci")] <- lapply(evolution[c("uci")], delog)

print(evolution)

# plot mean + 95% Confidence Intervals as the size of the subset increases
# of course, we need to de-log transform the data
plot <- ggplot(evolution, aes(x=subset, y=shortDuration, colour=interfaceType)) + 
  geom_errorbar(aes(ymin=lci, ymax=uci), width=.1, position=position_dodge()) +
  geom_line(position=position_dodge()) +
  geom_point(position=position_dodge(), size=3)

print(plot)

# plot the size of the confidence intervals as the size of the subset increases
# NB: this is the de-log transformed confidence interval
plot2 <- ggplot(evolution, aes(x=subset, y=(uci-lci), colour=interfaceType)) +
  geom_line(position=position_dodge()) +
  geom_point(position=position_dodge(), size=3) +
  expand_limits(y=0)

print(plot2)

# fit non-linear model to curves (so far, only control)
control <- subset(evolution, interfaceType==1)
f <- function(x,a,b) {a * exp(b * log(x))}

model <- nls((uci-lci) ~ f(subset,a,b), data=control, start = c(a = 1, b = 1))
print(model)

# predict CI for larger numbers of trials
predicted <- predict(model, newdata = data.frame(subset=seq(1,20,1)))
cat("CI for 10 trials: ", predicted[10], "\n")
cat("CI for 15 trials: ", predicted[15], "\n")
cat("CI for 20 trials: ", predicted[20], "\n")

# Plot the original points
plot(control$subset, control$uci-control$lci, xlim=c(2,20), ylim=c(0,18))

# Add a line representing the predicted data
lines(1:20, predicted)
