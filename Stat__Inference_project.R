#Step1: simulate the distribution of means of 40 exponential distributions with mean = 1/lambda & stddev = 1/lambda
nosim <- 1000
n <- 40
lambda <- 0.2
set.seed(8180)
#
#create nosim x n matrix to hold simulated random variables
simdata <- matrix(rexp(nosim * n, rate = lambda), nosim, n)
sim_means <- rowMeans(simdata)
hist(sim_means, col = "red", main = "Distribution of averages of exponential 
     distribution samples, lambda = 0.2")
#
#create matrix to store simulated and theoretical means
df_summary <- data.frame(mean(sim_means), 1/lambda)
colnames(df_summary) <- c("simulated", "theoretical")
print(round(df_summary,3))
#
#compare simulated & theoretical means - uncomment lines to add lines to histogram
#abline(v = mean(sim_means), col = "green", lwd = "3")
##theoretical
#abline(v = 1/lambda, col = "blue", lwd = "3")
#
#Step2: compare variance 
#
df_summary[2,] <- c(var(sim_means), (1/lambda)^2/n) 
rownames(df_summary) <- c("mean", "variance")
print(round(df_summary,3))
#
#Step3: show the variable approximently follows a normal distribution
#
library(ggplot2)
sim_means_g <- data.frame(sim_means)
g <- ggplot(sim_means_g, aes(x=sim_means))
g <- g + geom_histogram(aes(y=..density..), col = "black", fill = "red")
g <- g + geom_density(col = "blue", size = 1)
print (g)
#
#construct simulated confidence interval and compare to theoretical CI
#
sim_CI <- mean(sim_means) + c(-1,1)*1.96*sd(sim_means)/sqrt(n)
theo_CI <- 1/lambda + c(-1,1)*1.96*((1/lambda^2)/n)/sqrt(n)
CI_summary <- data.frame(rbind(sim_CI, theo_CI), row.names = c("simulated", "theoretical"))
colnames(CI_summary) <- c("Lower Bound", "Upper Bound")
print(CI_summary)
#
#normal quintile plot
qqnorm(sim_means); qqline(sim_means)
