nosim <- 1000
n <- 40
lambda <- 0.2
set.seed(818)
#
#create nosim x n matrix to hold simulated random variables
simdata <- matrix(rexp(nosim * n, rate = lambda), nosim, n)
sim_means <- rowMeans(simdata)
hist(sim_means, col = "blue", main = "Distribution of averages of exponential 
     distribution samples, lambda = 0.2")
#
#compare simulated & theoretical means
abline(v = mean(sim_means), col = "green")
#theoretical
abline(v = 1/lambda, col = "red")
#
#create matrix to store simulated and theoretical means
meansum <- data.frame(mean(sim_means), 1/lambda)
