library(tidyverse)

population_data <- read_csv("population_data.csv")
glimpse(population_data)

#SAMPLE 1
set.seed(1)
sample1_analytic_data <- sample_n(population_data, size = 200)
glimpse(sample1_analytic_data)

#actual regression
sample1_lm_results <- lm(performance~IQ, data = sample1_analytic_data)
summary(sample1_lm_results)
library(apaTables)
apa.reg.table(sample1_lm_results)
#IQ slope is .24, CI [.19, .29]

#SAMPLE 2, confidence interval - predicted value for Y(i.e. performance) for a given SINGLE value on X (i.e. IQ)
#want to know the predicted value for an IQ of 120

x_axis_range <- data_frame(IQ = c(120))
CI_data <- predict(sample1_lm_results, newdata = x_axis_range, interval = "confidence", level = .95)
CI_data <- as.data.frame(cbind(x_axis_range, CI_data))
print(CI_data)
#the predicted value is 79.67 (under fit), and the CI is 78.42, 80.93

#SAMPLE 3, confidence interval - predicted value of Y for a given RANGE of values on X
min_predictor <- min(sample1_analytic_data$IQ)
max_predictor <- max(sample1_analytic_data$IQ)
x_axis_range <- data.frame(IQ=seq(min_predictor, max_predictor, by=0.5))
CI_data <- predict(sample1_lm_results, newdata = x_axis_range, interval = "confidence", level = .95)
CI_data <- as.data.frame(cbind(x_axis_range, CI_data))
print(CI_data)

#SAMPLE 4, prediction interval - predicted value for Y for a given value of X
x_axis_range <- data_frame(IQ = c(120))
PI_data <- predict(sample1_lm_results, newdata = x_axis_range, interval = "prediction", level = .95)
PI_data <- as.data.frame(cbind(x_axis_range, PI_data))
print(PI_data)
#predicted value is 79.68, CI[69.23, 90.13]

#SAMPLE 5, prediction interval - predicted value for Y given a RANGE of values on X
min_predictor <- min(sample1_analytic_data$IQ)
max_predictor <- max(sample1_analytic_data$IQ)
x_axis_range <- data.frame(IQ=seq(min_predictor, max_predictor, by=0.5))
PI_data <- predict(sample1_lm_results, newdata = x_axis_range, interval = "prediction", level = .95)
PI_data <- as.data.frame(cbind(x_axis_range, PI_data))
print(PI_data)

#GRAPHING
reg_plot <- ggplot(sample1_analytic_data, aes(x=IQ, y=performance))
reg_plot <- reg_plot + geom_smooth(method = "lm", se = FALSE, color="black")
reg_plot <- reg_plot + geom_point()
reg_plot <- reg_plot + theme_classic()
reg_plot <- reg_plot + coord_cartesian(xlim = c(50, 150), ylim = c(0, 100))
reg_plot <- reg_plot + scale_x_continuous(breaks = seq(50, 150, by = 10))
print(reg_plot)

#with CI (just changed it to se=TRUE)
reg_plot <- ggplot(sample1_analytic_data, aes(x=IQ, y=performance))
reg_plot <- reg_plot + geom_smooth(method = "lm", se = TRUE, color="black")
reg_plot <- reg_plot + geom_point()
reg_plot <- reg_plot + theme_classic()
reg_plot <- reg_plot + coord_cartesian(xlim = c(50, 150), ylim = c(0, 100))
reg_plot <- reg_plot + scale_x_continuous(breaks = seq(50, 150, by = 10))
print(reg_plot)

#alternative approach to CI graph
reg_plot <- ggplot(sample1_analytic_data, aes(x=IQ, y=performance))
reg_plot <- reg_plot + geom_smooth(data=CI_data, aes(x=IQ, y=fit, ymin=lwr, ymax=upr),stat="identity")
reg_plot <- reg_plot + geom_point()
reg_plot <- reg_plot + theme_classic()
reg_plot <- reg_plot + coord_cartesian(xlim = c(50, 150), ylim = c(0, 100))
reg_plot <- reg_plot + scale_x_continuous(breaks = seq(50, 150, by = 10))
print(reg_plot)
#now we do the same thing but for a PI
reg_plot <- reg_plot + geom_smooth(data=PI_data, aes(x=IQ, y=fit, ymin=lwr, ymax=upr),stat="identity")
reg_plot <- reg_plot + geom_point()
reg_plot <- reg_plot + theme_classic()
reg_plot <- reg_plot + coord_cartesian(xlim = c(50, 150), ylim = c(0, 100))
reg_plot <- reg_plot + scale_x_continuous(breaks = seq(50, 150, by = 10))
print(reg_plot)
#one more time for both
reg_plot <- reg_plot + geom_smooth(data=CI_data, aes(x=IQ, y=fit, ymin=lwr, ymax=upr),stat="identity")
reg_plot <- reg_plot + geom_smooth(data=PI_data, aes(x=IQ, y=fit, ymin=lwr, ymax=upr),stat="identity")
reg_plot <- reg_plot + geom_point()
reg_plot <- reg_plot + theme_classic()
reg_plot <- reg_plot + coord_cartesian(xlim = c(50, 150), ylim = c(0, 100))
reg_plot <- reg_plot + scale_x_continuous(breaks = seq(50, 150, by = 10))
print(reg_plot)



