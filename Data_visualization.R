# this script focuses on several R basic commands to use when working with a dataset modified according to what I find interesting in this dataset

# this script focuses on several Data visualization basic commands to use when working with a dataset modified according to what I find interesting in this dataset

install.packages("dslabs") #install the package specifically written for this course 
install.packages("dplyr") #install dplyr - a grammar of data manipulation
library(dslabs) #load the packages
library(dplyr)
library(ggplot2)
data(heights) #load the data with which the following script will work
data(murders)

male <- heights$height[heights$sex=="Male"] #call out the heights only for the males
female <- heights$height[heights$sex=="Female"]

male_percentiles <- quantile(male, seq(0.1, 0.9, 0.2)) #Suppose we can't make a plot and want to compare the distributions side by side. If the number of data points is large, listing all the numbers is inpractical. A more practical approach is to look at the percentiles. We can obtain percentiles using the quantile function like this
female_percentiles <- quantile(female, seq(0.1, 0.9, 0.2)) #Creates a five row vector showing the 10th, 30th, 50th, 70th, and 90th percentiles for the heights 

df <- data.frame(female = female_percentiles, male = male_percentiles) #creates a data frame with 2 columns 

#Often data visualization is needed to confirm that our data follows a normal distribution. One way the normal distribution is useful is that it can be used to approximate the distribution of a list of numbers without having access to the entire list.
#Suppose all you know about the data is the average and the standard deviation and that its distribution is approximated by the normal distribution.
x <- heights$height[heights$sex=="Male"]
avg <- mean(x)
stdev <- sd(x)
pnorm(72, mean = avg, sd = stdev, lower.tail=TRUE)- pnorm(69, mean = avg, sd = stdev, lower.tail=TRUE) #Uses the normal approximation to estimate the proportion the proportion of the data that is between 69 and 72 inches.

exact <- mean(x > 79 & x <= 81)
mean_x <- mean(x)
stdev_x <- sd(x)
approx <- pnorm(81, mean = mean_x, sd = stdev_x, lower.tail = TRUE) - pnorm(79, mean = mean_x, sd = stdev_x, lower.tail = TRUE)
exact/approx #Reports how many times bigger the actual proportion is compared to the approximation. Notice that the approximation is very close to the exact calculation. The normal distribution was a useful approximation for this case.

####SCATTER PLOTS####
p <- ggplot(heights) #equivalent to p <- heights %>% ggplot()
murders %>% ggplot(aes(x = population , y = total)) + geom_point() #To create a scatter plot, we add a layer with the function geom_point. The aesthetic mappings require us to define the x-axis and y-axis variables respectively.
murders %>% ggplot(aes(population, total, label = abb)) + geom_label(color = "blue") #creates a scatter plot with the abbreviation names coloured in blue for the state instead of points
p <- murders %>% ggplot(aes(population, total, label = abb, color = region)) +  geom_label() #creates a scatter plot with the abbreviation names coloured depending on the states
q <- p + scale_x_log10() + scale_y_log10() #change the axes to log scales to account for the fact that the population distribution is skewed
q + ggtitle("Gun murder data") #adds a title to this plot

#####HISTOGRAMS#####
a <- heights %>% ggplot(aes(height)) #loads the height on the X-axis
b + geom_histogram() #adds the layer to create the histogram

#####SMOOTH DENSITY PLOTS#####
heights %>% ggplot(aes(height)) +  geom_density()
heights %>% ggplot(aes(height, group = sex)) + geom_density() #two smooth density plots
heights %>% ggplot(aes(height, color = sex)) + geom_density() #two smooth density plots colored based on sex
heights %>% ggplot(aes(height, fill = sex)) + geom_density(alpha = 0.2) #assign groups using the fill argument. When using the geom_density geometry, color creates a colored line for the smooth density plot while fill colors in the area under the curve


####Practice Exercise. National Center for Health Statistics####
library(NHANES)
data(NHANES)
library(dslabs)
data(na_example)
mean(na_example) #observe the NA values
sd(na_example) #observe the NA values
mean(na_example, na.rm = TRUE) #calculate the mean after removing the NA values
sd(na_example, na.rm = TRUE) #calculate the sd after removing the NA values
tab <- NHANES %>% filter(AgeDecade == " 20-29" & Gender == "female") #filters the dataset and creaes a new object with the age interval 20-29 and all females
ref <- NHANES %>% filter(AgeDecade == " 20-29" & Gender == "female") %>% summarize(average = mean(BPSysAve, na.rm = TRUE), 
            standard_deviation = sd(BPSysAve, na.rm=TRUE)) # determines the average and standard deviation of systolic blood pressure, which are stored in the BPSysAve variable in the NHANES dataset
ref_avg <- NHANES %>%
      filter(AgeDecade == " 20-29" & Gender == "female") %>%
      summarize(average = mean(BPSysAve, na.rm = TRUE)) %>%
      .$average # generates only the average blood pressure for 20-29 year old females. The last element is the place holder . in dplyr and allows us to store the numeric value from the data frame into a variable
NHANES %>% filter(AgeDecade == " 20-29"  & Gender == "female") %>%
      summarize(min = min(BPSysAve, na.rm = TRUE), 
                max = max(BPSysAve, na.rm = TRUE)) #alculates two other data summaries: the minimum and the maximum
NHANES %>% filter(Gender == "female") %>% group_by(AgeDecade) %>%
      summarize(average = mean(BPSysAve, na.rm = TRUE), 
                standard_deviation = sd(BPSysAve, na.rm=TRUE)) 
NHANES %>% filter(Gender == "male") %>% group_by(AgeDecade) %>%
      summarize(average = mean(BPSysAve, na.rm = TRUE), 
                standard_deviation = sd(BPSysAve, na.rm=TRUE)) #splits the data table into groups and then compute summary statistics for each group. computes the average and standard deviation of systolic blood pressure for females for each age group separately
NHANES %>% group_by(AgeDecade, Gender) %>% summarize(average = mean(BPSysAve, na.rm = TRUE),                standard_deviation = sd(BPSysAve, na.rm=TRUE)) # combines both of these summaries into a single line of code

NHANES %>% filter(Gender == "male" & AgeDecade==" 40-49") %>%
      group_by(Race1) %>% summarize(average = mean(BPSysAve, na.rm = TRUE), 
                standard_deviation = sd(BPSysAve, na.rm=TRUE)) %>%
      arrange(average) #arranges the values by the average of the blood presure in ascending order


#####Life expectancy vs fertility - the GAPMINDER FOUNDATION#####
library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)
gapminder %>% filter(continent=="Africa" & year == 2012) %>%
  ggplot(aes(fertility, life_expectancy, color = region)) +
  geom_point()
df <- gapminder %>% 
  filter(continent=="Africa" & year == 2012 & fertility <=3 & life_expectancy>=70) %>% select(country, region) #Creates a table showing the country and region for the African countries that in 2012 had fertility rates of 3 or less and life expectancies of at least 70
years <- 1960:2010
countries <- c("United States", "Vietnam")
tab <- gapminder %>% filter(year %in% years & country %in% countries) #The Vietnam War lasted from 1955 to 1975. Do the data support war having a negative effect on life expectancy? We will create a time series plot that covers the period from 1960 to 2010 of life expectancy for Vietnam and the United States, using color to distinguish the two countries.
p <- tab %>% ggplot(aes(x=year,y=life_expectancy,color=country)) + geom_line() #plots the data for the two countries.
daydollars <- gapminder %>% mutate(dollars_per_day = gdp/population/365) %>% filter(continent == "Africa" & year == 2010 & !is.na(dollars_per_day)) #calculates and plots dollars per day for African countries in 2010 using GDP data.
daydollars %>% ggplot(aes(dollars_per_day)) + geom_density() + scale_x_continuous(trans = "log2") #plots the smooth density plot using a log (base 2) x axis.
gapminder %>% 
  mutate(dollars_per_day = gdp/population/365) %>%
  filter(continent == "Africa" & year %in% c(1970,2010) & !is.na(dollars_per_day)) %>%
 ggplot(aes(dollars_per_day, fill = region)) + 
  geom_density(bw=0.5, position = "stack") + 
  scale_x_continuous(trans = "log2") + 
  facet_grid(year ~ .) #combines multiple plotting tools to create density plots for multiple years.
  gapminder %>% 
  mutate(dollars_per_day = gdp/population/365) %>%
  filter(continent == "Africa" & year %in% c(1970, 2010) & !is.na(dollars_per_day) & !is.na(infant_mortality)) %>%
  ggplot(aes(dollars_per_day, infant_mortality, color = region, label = country)) +
  geom_text() + 
  scale_x_continuous(trans = "log2") +
  facet_grid(year~.) #shows changes in the infant mortality and dollars per day patterns African countries between 1970 and 2010.  