install.packages("tidyverse")
library(ggplot2)
View(diamonds)


## a)	A histogram of carat using ggplot2
ggplot(data= diamonds) + geom_histogram(aes(x = carat), fill = "grey50")





## b)	A scatter plot mapping carat (x) and price (y) using ggplot2. Use the color aesthetic.
ggplot(diamonds, aes(x = carat , y =price)) + geom_point()

#Save into variable, call variable to add color
g<- ggplot(diamonds, aes(x = carat , y =price)) + geom_point()
g+ geom_point(aes(color = color))

##There is an upward trend. When carat increases, price increases as well. 
## we can see that high carat diamonds (3 to 5 carats) are rarer with higher prices too.
##Larger diamonds have a darker tones, they fall in higher grade (IF, VVS1, VVS2). 
##There is also a dense cluster if diamonds below 1.5 carats with prices varying widely.
##There is an overlap in price across different clarity level showing that at lower carats, higher prices are not guaranteed.
##There are some outlines in the 4 to 5 carat range with wide high prices.
## Carat drives the price when clarity and rarity are important with bigger size carats.


## c)	A boxplot of diamond carats using ggplot2. explain findings
ggplot(diamonds, aes(y = carat, x = 1)) +   geom_boxplot(fill = "skyblue") 
## Despite some outliers in carat range of 4 to 5, we can see that most of the diamonds are within 1 carat.
## the median is around 0.7.
## ---- Multiple boxplots for different cuts 
ggplot(diamonds, aes(y = carat, x = cut)) +   geom_boxplot(fill = "skyblue") 
##Fair cut diamonds have a higher median carat value than all other cuts. 
## It means that the larger diamonds have lower cut quality. 
## There is a higher number of outliers in the fair cut box which means that the diamonds are larger.
## Ideal cuts have smaller median carats, so diamonds are smaller and finely trimmed.
##The good, very good, premium cuts vary between 2 extremes with median carats and variability between fair and ideal.
## As cut quality increases, the carat decreases. Larger diamonds are common with low quality cuts.



## d)	A violin plot of diamond carats by cut using ggplot2.
ggplot(diamonds, aes(y = carat, x = cut)) + geom_violin(fill = "skyblue") 
## we can see the distinction in data density, ideal cuts represents the highest quality of cut in the dataset.
## it shows a hight density of diamonds concentrated below 0.5 carats.
# It means that most ideal diamonds are smaller in size, so the diamonds with price cuts
## have lower carat weight. 
## The fair cut are the lowest in quality, but it shows a broader carat distribution.
## It has the a density extending to 1 carat. It shows that diamonds with lower precision cut have higher carat weight.
## it means that the diamonds will not be perfectly symmetrical. 
## Premium, very good and good cuts have moderate spread densities with both small and medium carats.
##we can say that as the cut quality increase the carat decreases in size.


## e)	Using summary() and head() functions, provide an overview of the dataset.
summary(diamonds)
head(diamonds)
##It shows the minimum, the 1st quartile, median, mean, 3rd quartile, and maximum values.
# carat ranges from 0.2 to 5.01, the median its 0.7, price ranges from $326 to $18,823, the mean is around $3,933
#There is a wide range od diamond sizes and value. The most common cut is ideal with frequent clarity grade of SI1.
# The dataset has 53,940 observations.



require(ggthemes)
View(economics)

## b)	Explore the dataset using the head() and summary() functions. What are your observations? 
head(economics)

head(economics$pop)

## we have monthly data from July 1967
## we have a growing expenditure in July 1967 starting at 507 billion to 525 billion
## The population starts at 198712 to 200,000
## The Personal saving rate fluctuates between 11.8% and 12.8% showing a high consumer saving.
## Median during unemployment ranges around 4.5 to 4.9 weeks
## Unemployed is below 3 million people.


summary(economics)
## statistical overview from 1967 to 2015.
## There is a steady growth in population ranging from  198 million to 321 million.
## With the increase of population, there is an increase in consumption expenditure from 507billion to 12.2 trillion.

## Time series plot

ggplot(economics, aes(x = date, y = unemploy)) + geom_line(color = "skyblue") + labs(x = "Year",
                                                                                     y = "# of Unemployed") + theme_minimal()
## HISTORGRAM
ggplot(economics, aes(x = unemploy)) +geom_histogram(fill = "grey50", bins = 30) +
  labs(x = "# of Unemployed",y = "Frequency") +
  theme_minimal()

## SCATTER PLOT
ggplot(economics, aes(x = pop, y = unemploy)) + geom_point(color = "skyblue") +labs(
    x = "Population",
    y = "# of Unemployed") +
  theme_minimal()


## BAR CHART

library(dplyr)

# we have to add decade column and calculate average unemployment per decade
economics_summary <- economics %>%
  mutate(decade = paste0(substr(format(date, "%Y"), 1, 3), "0s")) %>%
  group_by(decade) %>%
  summarise(avg_unemploy = mean(unemploy))

# Bar chart
ggplot(economics_summary, aes(x = decade, y = avg_unemploy)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
   
    x = "Decade",
    y = "Avg # of Unemployed"
  ) +
  theme_minimal()


## we can see in the time-series plot, there are moments of economic downturn in the early 1980s, 2000s.
## there is a financial crisis leading to unemployment. it was all due to recessions, financial instability, and policy shifts directly impact job availability.
## Viewing the histogram, unemployment ranged between 6,000 and 10,000 showing a moderate level of unemployment.
## AS population increased, so did the number of unemployed people. Average unemployment was highest in the 2000s corresponding with the great recession.
##There was an economic recovery by the 2010s.

## What does the distribution of personal savings rates tell you about economic behavior?
##### Saving rates often correlate with income levels since people who make more money save larger portion of their income.
## Some families save more money while other save less, or even end up using their saving due to emergencies.
## On one hand the fact that people save more indicates caution and uncertainty about the future.
## On the other hand when people save less, it could also indicate a greater confidence leading to more spending.
## ## Saving influences consumption patterns because families are not spending the money, and it directly affects economic growth.
### With this distribution, policymakers can come up with strategies like tax incentive to make people spend more but wisely to promote economic stability.


## •	Discuss any visible trends between unemployment and personal consumption expenditures.

## We can see an inverse relationship between unemployment and personal consumption.
## When unemployment rises, personal consumption expenditures decrease.
## It could grow slowly as well because some families have steady income. 
## When unemployment falls, people are more employed with more money to spend, which boost consumption.
## It shows that job availability affects consumer confidence and spending habits. 


## •	How does the average personal savings rate vary by decade? What historical events might correlate with these variations?

## The average personal savings rate vary by decade with the change of economic conditions and consumer behavior over time.
## From the 1970s to the 1980s there is a higher saving rate potentially caused by recession, or oil crisis forcing families to save more money.
## From the 1990s to early 2000s there is a lower average saving rate due to a higher consumer confidence.
## People had access to credit, and spent more.
## The 2008 recession caused a spike in savings rate because families were more reluctant to spend.

















