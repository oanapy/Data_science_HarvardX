# this script focuses on several R basic commands to use when working with a dataset modified according to what I find interesting in this dataset

#Write a line of code that calculates the standard error se of a sample average when you poll 25 people in the population. Generate a sequence of 100 proportions of Democrats p that vary from 0 (no Democrats) to 1 (all Democrats). Plot se versus p for the 100 different proportions. Generates three plots of p versus se when the sample sizes equal N=25, N=100, and N=1000.
N <- 25 #the number of people polled
p <- seq(0, 1, length = 100) #contains 100 proportions ranging from 0 to 1 using the `seq` function
se <- sqrt(p*(1-p)/N) #contains the standard error of each sample average
plot(p, se) #`p` on the x-axis and `se` on the y-axis
sample_sizes <- c(25, 100, 1000) #contains the three sample sizes
for(N in sample_sizes){
  se <- sqrt(p*(1-p)/N)
  plot(p, se, ylim = c(0,0.5/sqrt(25)))
} #calculates the standard error `se` for every value of `p` for each of the three samples sizes `N` in the vector `sample_sizes`. Plots the three graphs, using the `ylim` argument to standardize the y-axis across all three plots.

#Write a function called take_sample that takes the proportion of Democrats p and the sample size N as arguments and returns the sample average of Democrats (1) and Republicans (0). Calculate the sample average if the proportion of Democrats equals 0.45 and the sample size is 100.
take_sample <- function(p, N){
    X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1 - p, p))
    mean(X)
} #returns the average value of a randomly sampled population.
p <- 0.45 #proportion of Democrats in the population being polled
N <- 100 #the number of people polled
take_sample(p,N) #etermine the sample average of `N` randomly selected people from a population containing a proportion of Democrats equal to `p`

#The standard error is related to the typical size of the error we make when predicting. The typical error is 0. For mathematical reasons related to the central limit theorem, we actually use the standard deviation of errors rather than the average of the absolute values. The standard error is the square root of the average squared distance (X¯−p)2. The standard deviation is defined as the square root of the distance squared.
#Calculate the standard deviation of the spread.
p <- 0.45 #he proportion of Democrats in the population being polled
N <- 100 #the number of people polled
B <- 10000 #the number of times we want the sample to be replicated
errors <- replicate(B, p - take_sample(p, N)) # generate`errors` by subtracting the estimate from the actual proportion of Democratic voters
sqrt(mean(errors^2)) #the standard deviation of `errors`

#Estimate the standard error given an expected value of 0.45 and a sample size of 100.
p <- 0.45 #he proportion of Democrats in the population being polled
N <- 100 #the number of people polled
sqrt(p*(1-p)/N) # the standard error

#n practice, p is unknown, so we construct an estimate of the theoretical prediction based by plugging in X¯ for p. Calculate the standard error of the estimate: SE^(X¯)
p <- 0.45 #he proportion of Democrats in the population being polled
N <- 100 #the number of people polled
X <- sample(c(0,1), size=N, replace=TRUE, prob=c(1-p, p)) #`X` as a random sample of `N` voters with a probability of picking a Democrat ('1') equal to `p`
X_bar <- mean(X) #the average sampled proportion
sqrt(X_bar*(1-X_bar)/N) #the standard error of the estimate.

#Make a qq-plot of the errors to see if they follow a normal distribution.
p <- 0.45 #the proportion of Democrats in the population being polled
N <- 100 #the number of people polled
B <- 10000 #the number of times we want the sample to be replicated
errors <- replicate(B, p - take_sample(p, N)) ## Generates `errors` by subtracting the estimate from the actual proportion of Democratic voters
qqnorm(errors) # Generate a qq-plot of `errors` with a qq-line showing a normal distribution
qqline(errors)

#If p=0.45 and N=100, use the central limit theorem to estimate the probability that X¯>0.5.
p <- 0.45 #the proportion of Democrats in the population being polled
N <- 100 #the number of people polled
1 - pnorm(0.5, p, sqrt(p*(1-p)/N))#the probability that the estimated proportion of Democrats in the population is greater than 0.5

#Assume there are only two candidates and construct a 95% confidence interval for the election night proportion p. This works with the dataset polls_us_election_2016 from the dslabs package
library(dslabs)
data("polls_us_election_2016")
polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-31" & state == "U.S.")  #Generate an object `polls` that contains data filtered for polls that ended on or after October 31, 2016 in the United States
N <- polls$samplesize[1] # Assigns the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.
X_hat <- polls$rawpoll_clinton[1]/100 # For the first poll in `polls`, convert the percentage to a proportion of Clinton voters and assign it to a variable called `X_hat`. Print this value to the console.
se_hat <- sqrt(X_hat*(1-X_hat)/N) # Calculate sthe standard error of `X_hat` and save it to a variable called `se_hat`
ci<- c(X_hat - qnorm(0.975)*se_hat, X_hat + qnorm(0.975)*se_hat)
plot(ci) # Use `qnorm` to calculate the 95% confidence interval for the proportion of Clinton voters. Save the lower and then the upper confidence interval to a variable called `ci`.

#Create a new object called pollster_results that contains the pollster's name, the end date of the poll, the proportion of voters who declared a vote for Clinton, the standard error of this estimate, and the lower and upper bounds of the confidence interval for the estimate.
pollster_results <- polls %>% 
                    mutate(X_hat = polls$rawpoll_clinton/100, se_hat = sqrt(X_hat*(1-X_hat)/samplesize), lower = X_hat - qnorm(0.975)*se_hat, upper = X_hat + qnorm(0.975)*se_hat) %>% 
                    select(pollster, enddate, X_hat, se_hat, lower, upper)

#The final tally for the popular vote was Clinton 48.2% and Trump 46.1%. Add a column called hit to pollster_results that states if the confidence interval included the true proportion p=0.482 or not. What proportion of confidence intervals included p?
avg_hit <- pollster_results %>% mutate(hit = lower<=0.482 & upper>=0.482) %>% summarize(mean(hit)) # Adds a logical variable called `hit` that indicates whether the actual value exists within the confidence interval of each poll. Summarize the average `hit` result to determine the proportion of polls with confidence intervals include the actual value.

#The random sample is taken from a population and the urn serves as an analogy for the population. Consider x to be the heights of all males in the data set. Mathematically speaking, x is the population. What are the population average and standard deviation of our population?
library(dslabs)
data(heights)
x <- heights %>% filter(sex == "Male") %>%
  .$height #vector of heights from all males in the population
mean(x)
sd(x) #the population standard deviation.
