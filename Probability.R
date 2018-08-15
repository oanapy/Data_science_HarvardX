#One ball will be drawn at random from a box containing: 3 cyan balls, 5 magenta balls, and 7 yellow balls. What is the probability that the ball will be cyan?
cyan <- 3
magenta <- 5
yellow <- 7
p <- cyan / (cyan + magenta + yellow)

#What is the probability that the ball you draw from the box will NOT be cyan?
q <- 1-p

#Instead of taking just one draw, consider taking two draws. You take the second draw without returning the first draw to the box = sampling without replacement.
#What is the probability that the first draw is cyan and that the second draw is not cyan?
p_1 <- cyan / (cyan + magenta + yellow) #the probability of choosing a cyan ball from the box on the first draw.
p_2 <- 1 - (cyan - 1) / (cyan + magenta + yellow - 1) #the probability of not choosing a cyan ball on the second draw without replacement.
p_1 * p_2 #the probability that the first draw is cyan and the second draw is not cyan.

#Now repeat the experiment, but this time, after taking the first draw and recording the color, return it back to the box and shake the box = sampling with replacement.
#What is the probability that the first draw is cyan and that the second draw is not cyan?
p_1 <- cyan / (cyan + magenta + yellow) #the probability of choosing a cyan ball from the box on the first draw.
p_2 <- 1 - (cyan) / (cyan + magenta + yellow) #the probability of not choosing a cyan ball on the second draw with replacement.
p_1 * p_2 #the probability that the first draw is cyan and the second draw is not cyan.

#Say you’ve drawn 5 balls from the a box that has 3 cyan balls, 5 magenta balls, and 7 yellow balls, with replacement, and all have been yellow.
#What is the probability that the next one is yellow?
p_yellow <- yellow/(cyan+magenta+yellow) #the probability that a yellow ball is drawn from the box.

#If you roll a 6-sided die six times, what is the probability of not seeing a 6?
p_no6 <- 1 - (1/6) #the probability of not seeing a 6 on a single roll.
q_no6 <- p_no6^6 #the probability of not seeing a 6 on six rolls.

#Two teams, say the Celtics and the Cavs, are playing a seven game series. The Cavs are a better team and have a 60% chance of winning each game. 
#What is the probability that the Celtics win at least one game? Remember that the Celtics must win one of the first four games, or the series will be over!
p_cavs_win4 <- 0.6^4 ##the probability that the Cavs will win the first four games of the series.
q_cavs_win4 <- 1 - p_cavs_win4 #the probability that the Celtics win at least one game in the first four games of the series.

#Create a Monte Carlo simulation to confirm your answer to the previous problem. Use B <- 10000 simulations
# This line of sample code simulates four random games where the Celtics either lose or win. Each game is independent of other games.
simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))

# The variable 'B' specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000
celtic_wins <- replicate(B, {
  simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))
  any(simulated_games=="win")
}) ##replicates the sample code generating the variable called `simulated_games` for `B` iterations and then tallies the number of simulated series that contain at least one win for the Celtics.
mean(celtic_wins) # the frequency out of B iterations that the Celtics won at least one game. Print your answer to the console. 

#Assume the distribution of female heights is approximated by a normal distribution with a mean of 64 inches and a standard deviation of 3 inches. If we pick a female at random, what is the probability that she is 5 feet or shorter?
female_avg <- 64 #the average female height.
female_sd <- 3 #the standard deviation for female heights.
pnorm(5*12, female_avg, female_sd) #the probability that a randomly selected female is shorter than 5 feet.
w <- 1 - pnorm(6*12, female_avg, female_sd) #the probability that a randomly selected female is 6 feet or taller.

#Compute the probability that the height of a randomly chosen female is within 1 SD from the average height.
taller <- female_avg+female_sd #height that is one SD taller than average. 
shorter <- female_avg-female_sd #height that is one SD shorter than average
pnorm(taller, female_avg, female_sd) - pnorm(shorter, female_avg, female_sd) #the probability that a randomly selected female is between the desired height range

#Imagine the distribution of male adults is approximately normal with an expected value of 69 inches and a standard deviation of 3 inches. How tall is a male in the 99th percentile?
male_avg <- 69 #the average female height.
male_sd <- 3 #the standard deviation for female heights.
qnorm(0.99, male_avg, male_sd) #the height of a man in the 99th percentile of the distribution.

#The distribution of IQ scores is approximately normally distributed. The expected value is 100 and the standard deviation is 15. Suppose you want to know the distribution of the person with the highest IQ in your school district, where 10,000 people are born each year.
#Generate 10,000 IQ scores 1,000 times using a Monte Carlo simulation. Make a histogram of the highest IQ scores.
B <- 1000 #the number of times we want the simulation to run.
highestIQ <- replicate(B, {
  simulated_data <- rnorm(10000, 100, 15)
  max(simulated_data)
}) #he highest IQ score from each random distribution of 10,000 people.
hist(highestIQ) #Makes a histogram of the highest IQ scores.