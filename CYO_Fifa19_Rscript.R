# Let's first install the required packages if they are needed on our systems.
# -------------
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(colorspace)) install.packages("colorspace", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(measurements)) install.packages("measurements", repos = "http://cran.us.r-project.org")
if(!require(purrr)) install.packages("purrr", repos = "http://cran.us.r-project.org")
if(!require(gghighlight)) install.packages("gghighlight", repos = "http://cran.us.r-project.org")
if(!require(caTools)) install.packages("caTools", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(stringr)
library(readr)
library(dplyr)
library(data.table)
library(ggplot2)
library(measurements)
library(gghighlight)
library(caTools)
library(stats)
library(purrr)
library(plotly)
library(scales)
library(gridExtra)

# ------------ extra info -----
# In this dataset, there are 2 columns for wage and value. The correct column to use is Wage and Height. Those have
# already been cleaned and convert. They show the Values and Wages in correct format (complete number). 


# First let's load the data:
fifa <- read.csv("datafifa.csv")
fifa <- read_delim("datafifa.csv", ",", escape_double = FALSE, trim_ws = TRUE)
# View(fifa)
str(fifa)

# when checking the datatypes, "value" and "wage" are set to be "characteristics" when actually they need to be numeric. 
  # let's first change that and remove the "lbs" from weight: 

fifa$Weight <-  str_remove(fifa$Weight, "lbs") # as I will convert the weight later on, I will remove the "lbs" already here

fifa <- as.data.frame(fifa) %>% 
  mutate(Value = as.numeric(as.character(Value)),
         Wage = as.numeric(as.character(Wage)),
         Weight = as.numeric(as.character(Weight)))  

  # Just in case when some NAs would appear, let's remove them before continueing. 
fifa <- fifa[complete.cases(fifa), ]
# head(fifa)

# Being from the Netherlands, inches and pounds don't ring a bell. 
    # So I want to convert them to something I do understand: cm and kg. First I remove the ' from the notation, 
    # after which I make sure both numbers in the array are in fact numbers. Than I multiply the first part
    # of the arrary by 12, which will retrun only inches in the end instead of feet and inches. And add the inches after '
temp <-  str_split(fifa$Height, "'")

for(i in 1:length(temp)){
  temp[[i]] <- as.numeric(temp[[i]])
}

for (i in 1:length(temp)) {
  temp[[i]] <-  (temp[[i]][1] *12)+temp[[i]][2]
}
temp2 <- as.numeric(unlist(temp))
fifa$Height <- temp2

# By using the "measurement" package, I can easily convert pounds to kg and inches to centimeters without a lot of calculations.
fifa$Weight <- conv_unit(fifa$Weight, "lbs", "kg")
fifa$Height <- conv_unit(fifa$Height, "inch", "cm")

# ------------------ Analyzing the data -----------------
# Before starting the predictions and clustering, I would like to understand the data a bit better. So let's analyse it:

# Check the distribution of the player's ages:
fifa %>% ggplot(aes(Age)) + 
  geom_histogram() + 
  labs(x = "Age", y="Number of players") + 
  geom_vline(aes(xintercept = mean(fifa$Age)), color = "#0099FF", size = 1) +
  ggtitle("Distribution of player's age", subtitle = "The highlighted line represents the average age of 25")

# Let's also check if players of a certain age get rated better:
      # fifa %>% select(Overall, Age) %>% group_by(Age) %>%
      #   summarize(n = mean(Overall, na.rm = TRUE)) %>%
      #   ggplot(aes(x=Age, y = n)) + geom_bar(stat = "identity", position = "identity") +
      #   labs(x = "Mean overall rating", y = "Age") + geom_text(aes(label = round(n, 2)), nudge_x = 1 )
# As I first applied no Age binning, the chart is not very clear to read, so I will apply age-bins.
#   As the previous distribution showed there are more players between 20-30, I will take this into account when creating the bins

for(i in 1:nrow(fifa)){
  binning <- fifa[i,]
  
  if(binning$Age <20){
    fifa[i, 'Age_bin'] <- "< 20 years"
  } else if(binning$Age >= 20 & binning$Age <= 23){
    fifa[i, 'Age_bin'] <- "20 - 23 years"
  }else if (binning$Age > 23 & binning$Age <= 27){
    fifa[i, 'Age_bin'] <- "24 - 27 years"
  }else if (binning$Age > 27 & binning$Age <= 31){
    fifa[i, 'Age_bin'] <-  "28 - 31 years"
  } else if (binning$Age > 31 & binning$Age <=35){
    fifa[i, 'Age_bin'] <- "32 - 35 years"
  }else {
    fifa[i, 'Age_bin'] <- "35+ years"
  }
}

fifa %>% 
  select(Overall, Age_bin) %>% 
  group_by(Age_bin) %>%
  summarize(n = mean(Overall, na.rm = TRUE)) %>%
  ggplot(aes(x=Age_bin, y = n)) + 
  geom_bar(stat = "identity", position = "identity") +
  labs(x = "Mean Overall Rating", y = "Age bin") + 
  geom_text(aes(label = round(n, 2)), position = position_stack(vjust = 0.5), color = "#FFFFFF") + 
  ggtitle("Mean Overall Rating per Age Bin")

# Let's also look at the potential rating of the players per bin:
fifa %>% 
  select(Potential, Age_bin) %>% 
  group_by(Age_bin) %>%
  summarize(n = mean(Potential, na.rm = TRUE)) %>%
  ggplot(aes(x=Age_bin, y = n)) + 
  geom_bar(stat = "identity", position = "identity") +
  labs(x = "Mean Potential Rating", y = "Age bin") + 
  geom_text(aes(label = round(n, 2)), position = position_stack(vjust = 0.5), color = "#FFFFFF") +
  ggtitle("Mean Potential Rating per Age Bin")

# Let's also take a quick look at the value of players per age-Bin
fifa %>% 
  ggplot(aes(x = Age_bin, y = Value)) + 
  geom_boxplot() +
  scale_y_log10(labels = dollar_format(prefix = "Euro ")) 

fifa %>% 
  select(Name, Age_bin, Potential, Value) %>% 
  group_by(Age_bin) %>% 
  filter(Value == max(Value)) %>% 
  arrange(Age_bin) 

# Let's also take a look at the best potential player per age bin, to cover full:
fifa %>% 
  select(Name, Age_bin, Potential, Value) %>% 
  group_by(Age_bin) %>% 
  filter(Potential == max(Potential)) %>% 
  arrange(Age_bin) 

fifa %>% 
  select(Name, Age_bin, Overall, Value) %>% 
  group_by(Age_bin) %>% 
  filter(Overall == max(Overall)) %>% 
  arrange(Age_bin) 
# We looked at the potential of the players, but based on their age I suppose the younger players have a bigger
    # difference between their potential and their overall score at this point. Let's create a chart to see if my idea
    # is supported by the data: 

full_potential_reached <-   fifa[(fifa$Potential > fifa$Overall), ]
full_potential_reached %>% 
  select(Name, Club, Overall, Potential, Age_bin, Value) %>%  
  arrange(desc(Potential)) %>%  top_n(10)

# And let's see if nationality has something to do with the high potential of a player
fifa %>% 
  select(Nationality, Potential) %>% 
  group_by(Nationality) %>% 
  summarize(n = mean(Potential)) %>% 
  arrange(-n) %>% 
  head(10) %>% 
  ggplot(aes(x = reorder(Nationality, -n), y = n)) + 
  geom_bar(stat = "identity") + 
  labs(x = "Nationality", y = "Mean Potential Rating") +
  geom_text(aes(label = round(n, 2)), position = position_stack(vjust = 0.5), color = "#FFFFFF") +
  theme(axis.text = element_text(angle = 45, hjust = 1)) +
  ggtitle("10 Nationalities that score the best Potential Rating", 
          subtitle = "The top 4 is rather surprising as they are all not countries with teams in the World Cup")
  # The Dominican Replublic has players with the highest average potential

# And let's also look at the Value per Nationality and see which nationalities are the biggest 
fifa %>% 
  select(Nationality, Value) %>% 
  group_by(Nationality) %>% 
  summarize(n = sum(Value)) %>% 
  arrange(-n) %>% 
  head(10) %>% 
  ggplot(aes(x = reorder(Nationality, -n), y = n)) + 
  geom_bar(stat = "identity") + 
  labs(x = "Nationality", y = "Value") +
  theme(axis.text = element_text(angle = 45, hjust = 1)) +
  ggtitle("10 most valuable nationalities", 
          subtitle = "Top 10 exists of mainly European nationalities including indeed the Netherlands" )
  # But still, neither of those previously stated countires have the highest value, as this is Spain. 
  # This shows European players tend to have a higher value regarding this data. 

# Comparing Overall rating with the value of the players
fifa %>% 
  ggplot(aes(x = Overall, y = Value)) + 
  geom_point(alpha = 0.3) + 
  labs(x = "Overall Rating", y = "Value") + 
  ggtitle("Comparing Value with Overall Ratings", 
          subtitle = "It seems with a rating over 75 a player will get more valuable. With a rating of 85 the value starts to spread widely") +
  gghighlight(Value > 90000000, label_key = Name)
  # This the highlight function, I made sure the top valueble players immediately stand out with a value over 90.000.000
  # Neymar Jr appears to be the most valuable player in this dataset. 

# Now we know about the Value, but which players actually have the highest wages:
  # First create the list with players stated to be the most valuable. 
Top4 <- c("L. Messi", "Neymar Jr", "K. De Bruyne", "E. Hazard")

fifa %>% 
  select(Name, Club, Wage) %>% 
  group_by(Club) %>% 
  filter(Wage == max(Wage)) %>% 
  arrange(-Wage) %>% 
  head(10) %>% 
  ggplot(aes(x =reorder(Name, -Wage), y = Wage)) + 
  geom_bar(stat = "identity") +
  labs(x = "Name", y = "Wage") +
  theme(axis.text = element_text(angle = 45, hjust = 1), legend.text = element_text(size = 7), legend.title = element_text(size = 8)) + 
  ggtitle("10 Players with highest wage", subtitle = "Highlighted are the players valued at over 90 million") +
  gghighlight(Name %in% Top4 )
  # Messi (Barcelona_ is the highest paid player, followed by Modric (Real Madrid) and Ronaldo (Juventus)

# Ok now we know the player with the highest wage but does the "total wage payments" of the clubs support this conclusion?
fifa %>% 
  group_by(Club) %>% 
  summarise(TotalWage = round(sum(Wage)/1000000, digits = 2)) %>% 
  arrange(-TotalWage) %>% 
  head(10) %>% 
  ggplot(aes(x = reorder(as.factor(Club), -TotalWage), y = TotalWage, label = TotalWage)) +
  geom_bar(stat = "identity") + labs(x = "Club", y = "Total Club's Wages in Millions") +
  geom_text(aes(label = round(TotalWage, 2)), position = position_stack(vjust = 0.5), color = "#FFFFFF") + 
  theme(axis.text = element_text(angle = 45, hjust = 1)) +
  ggtitle("Top 10 Clubs with the highest Wages paid", 
          subtitle = "Top 3 Clubs pay the wages of the Top 5 players")
  # Barcelona and Real are number 1 and 2, so are Messi and Modric even though positions of clubs are switched. 
  # But Juventus, the club of Ronaldo is only 5th in line of "wages paid". 

# Does this mean that the clubs who have the players with the higest wages and the highest wages overall also have the highest value?
fifa %>% 
  group_by(Club) %>% 
  summarise(TotalValue = round(sum(Value)/1000000, digits = 2)) %>% 
  arrange(-TotalValue) %>% 
  head(10) %>% 
  ggplot(aes(x = reorder(as.factor(Club), -TotalValue), y = TotalValue, label = TotalValue)) +
  geom_bar(stat = "identity") + labs(x = "Club", y = "Total Club's Value in Millions") +
  geom_text(aes(label = round(TotalValue, 2)), position = position_stack(vjust = 0.5), color = "#FFFFFF") + 
  theme(axis.text = element_text(angle = 45, hjust = 1)) +
  ggtitle("Top 10 Clubs with the highest Value", 
          subtitle = "Top 3 clubs with the highest value, are equal to those with the highest wages")
  # The data seems to support the hypothesis as Real Madrid and Barcelona also have the highest value. With Juventus on a 4th place. 

# As is stated by the FIFA association, Messi is seen as a Super Star. So is Ronaldo. This means in this dataset that their
# Overall rating is over 86. We have seen the top 10 of clubs with the highest value and the highest wages, does this 
# mean they also have more super stars playing for them. The logical answer would be "yes" but let's see if the data supports it. 
fifa %>% 
  mutate(SuperStar = ifelse(Overall > 85, "SuperStar", "Normal")) %>% 
  group_by(Club) %>% 
  filter(SuperStar == "SuperStar") %>% 
  summarize(PlayerCount = n()) %>% 
  arrange(-PlayerCount) %>% 
  ggplot(aes(x = reorder(as.factor(Club), -PlayerCount), y = PlayerCount, label = PlayerCount)) +
  geom_bar(stat = "identity") + 
  labs(x = "Club", y = "Number of SuperStars") +
  geom_text(aes(label = PlayerCount), position = position_stack(vjust = 0.5), color = "#FFFFFF") + 
  theme(axis.text = element_text(angle = 45, hjust = 1)) +
  ggtitle("Clubs and number SuperStars in their teams", 
          subtitle = "Top 3 Clubs in highest value & wage also have the most Superstars in their teams") 
  # So based on all the analyses I did, Value and Wage go hand in hand regarding the clubs. So is the Overall and Potential. 
  # As can be seen, Barcelona and Real Madrid have the highest paid players, the highest wages paid and the highest value. 
  # This is reflecting in the amount of superstars they have on their teams (9 for both, compared to 5 for the number 3 in the list. )

# So are the these teams also the teams with the highest overall rating? Or do they just seem to have more superstars because they have the money?
top_20_overall <- fifa %>% 
  group_by(Club) %>% 
  summarize(AverageRating = mean(Overall, na.rm = T)) %>% 
  arrange(desc(AverageRating)) %>% 
  head(20) %>% 
  pull(Club)
top_20_overall
  # This top 20 show that the Italian teams are actually have the best overall ratings. But with the superstars it was
  # Barcelona, Real and PSG. Let's see where they end up in the overall rating by highlighting them in the next chart.

fifa %>% 
  filter(Club %in% top_20_overall) %>% 
  mutate(Top3 = ifelse(Club %in% c("FC Barcelona", "Real Madrid", "Manchester City"), "Yes", "No")) %>% 
  ggplot(aes(x = reorder(Club, Overall), y = Overall, fill = Top3)) +
  geom_boxplot() +
  scale_fill_manual(values = c("lightgrey", "#0099FF")) +
  coord_flip() +
  labs(x = "Clubs", y = "Player's Overall Rating") +
  theme(legend.position = "none") +
  ggtitle("Clubs with the highest overall rating", 
          subtitle = "Highlighted in blue, are the clubs with the most superstars in their team and have the highest wage & value")        
  # They still are in the top 10, but not in the top 3 as they were before. You can see that the teams with the most
  # superstars have a wider range of player's rating (so more difference in quality) as the Italian teams have. This
  # might become a problem when the superstars are moving to different clubs or when they quit their carreer...

# A lot of more analyses can be done on this dataset in order to come to different conclusions. But for now this will sufice. 
# I will now move on to some different correlations and 


# ------------------ Correlations & Clustering -------------------
# Before we can draw the correlation between being a SuperStar and the expected value, I need to make sure I can address 
# everything of the players being stated to be a Superstar. Let's take a look at all the players stated to be a SuperStar:

for(i in 1:nrow(fifa)){
  super <- fifa[i,]
  
  if(super$Overall <=85){
      fifa[i, 'Superstar'] <- "No Superstar"
    } else if(super$Overall > 85){
      fifa[i, 'Superstar'] <- "Superstar"
    }else {
      fifa[i, 'Superstar'] <- "Unknown"
    }
}
  # First I make sure all players have a Superstar (or not) label added to them

# Let's find out if there is a linear distribution of Player's values and see if this would be the right approach:
y <-  fifa$Value
set.seed(2)
test_index <- createDataPartition(y, time = 1, p = 0.5, list = FALSE)
train_set <- fifa %>% slice(-test_index)
test_set <- fifa %>% slice(test_index)

# estimating the average and standard deviation of the data
params <- train_set %>% 
  group_by(Superstar) %>% 
  summarize(avg = mean(Value), sd = sd(Value))

# estimating the prevalence of the data
pi <-  train_set %>% 
  summarize(pi = mean(Superstar == "Superstar")) %>% 
  .$pi

# getting the actual rule while using the average and standard deviation
f0 <-  dnorm(test_set$Value, params$avg[2], params$sd[2])
f1 <-  dnorm(test_set$Value, params$avg[1], params$sd[1])

p_hat_bayes <- f1*pi / (f1 *pi + f0*(1 - pi))

head(p_hat_bayes)
# visualizing the Naive Bayes estimate 
fifa %>% 
  mutate(x = round(Value)) %>% 
  group_by(x) %>% 
  summarize(prop = mean(Superstar == "Superstar")) %>% 
  ggplot(aes(x, prop)) + 
  geom_point() +
  scale_x_continuous(labels = dollar_format(prefix = "Euro ")) +
  labs(x = "Value", y = "Mean of Superstar") +
  ggtitle("Distribution of players based on Superstar status and value", 
          subtitle = "Superstar status is reached with an overall rating of over 85")

  # From this chart we can only concluded that the closer a player is to being a superstar, the higher their value is,
  # something that is actually rather obvious. Though also in the "non superstar" section, there are players earning more. 
  # This might be due to potential, as from this chart it is impossible to tell what the reason is. So let's dive a bit deeper

# Let's find out if there is a correlation between the position the player plays and their value, before we go on to 
# clustering based on their skillset. In order to analyse the positions, I created 4 groups as all forwards should be compared with 
# one another, so should the other groups. I applied these groups, as there are also used in the award-ceremonies of the FIFA itself. 
positions <-  unique(fifa$Position)

gk <- "GK" # goalkeepers
def <- positions[str_detect(positions, "B$")] # defenders
mid <- positions[str_detect(positions, "M$")] # midfielders
fw1 <- positions[str_detect(positions, "F$")] # 4 types of forwards
fw2 <- positions[str_detect(positions, "S$")]
fw3 <- positions[str_detect(positions, "T$")]
fw4 <- positions[str_detect(positions, "W$")]
fwd <- c(fw1, fw2, fw3, fw4)      # all forwards combined as also they are just 1 group. 

fifa <- fifa %>% 
  mutate(PositionGroup = ifelse(Position %in% gk, "GK", 
                                ifelse(Position %in% def, "DEF", 
                                       ifelse(Position %in% mid, "MID", 
                                              ifelse(Position %in% fwd, "FWD", "Unknown")))))

# Let's check what their values are based on the position the players play.
fifa %>% 
  filter(PositionGroup != "Unknown") %>% 
  ggplot(aes(x = PositionGroup, y = Value)) + 
  geom_boxplot() + 
  scale_y_log10(labels = dollar_format(prefix = "Euro "))
  # first I looked at the distribution within the PostionGroups I just created

fifa %>% 
  filter(PositionGroup != "Unknown") %>% 
  ggplot(aes(x = Position, y = Value)) + 
  geom_boxplot() +
  scale_y_log10(labels = dollar_format(prefix = "Euro ")) + 
  coord_flip() + 
  facet_wrap(~ PositionGroup, scales = "free")
  # Followed by checking the best valued positions within a positiongroup

# As is kind of logical, forwards and midfielders are more expensive than a goalkeep for instance. When looking
  # at the most expensive positions in the forward and midfielders, a attacking midfielder and right or left forwards are 
  # the most expensive. Of course there are certain players who make those positions more valuable, which is something
  # I have to keep in the back of my head.

# Though it is strange that only 2 out of 4 most valuable players actually play on those positions
fifa %>% 
  select(Name, Position, PositionGroup, Value) %>% 
  filter(Name %in% Top4) %>% 
  arrange(desc(Value))

# Let's check if there is a correlation between rating and validations:
fifa %>% 
  filter(PositionGroup != "Unknown") %>% 
  ggplot(aes(x = Overall, y = Value)) +
  geom_point(position = "jitter") +
  scale_y_log10(labels = dollar_format(prefix = "Euro ")) +
  annotate("text", x = 60, y = 97000000, 
           label = paste0("Spearman Correlation: ", round(cor(fifa$Overall, fifa$Value, method = "spearman"),4)), 
           color = "#0099FF", size = 6)
  # I used the spearman method as there are a lot of large outliers, like previous charts also showed (Neymar, Messie etc.). 

# Let's look at the possible other correlations relating to the overall rating of a player. 
# Let's do this with the Pearson as well as the Spearman method to see the differences:
gk_players <- fifa %>% 
  select(contains("GK")) %>% 
  names()

spearman_cor_overall <- fifa %>% 
  filter(Position != "GK") %>% 
  select_if(is.numeric) %>% 
  select(-gk_players, -ID, -`Jersey Number`) %>% 
  as.matrix() %>% 
  na.omit() %>% 
  cor(method = "spearman")

pearson_cor_overall <- fifa %>% 
  filter(Position != "GK") %>% 
  select_if(is.numeric) %>% 
  select(-gk_players, -ID, -`Jersey Number`) %>% 
  as.matrix() %>% 
  na.omit() %>% 
  cor()

cor_colnames <- colnames(spearman_cor_overall)

spearman_cor_overall <-  spearman_cor_overall[,2] %>% 
  data.frame()
spearman_cor_overall <- cbind(cor_colnames, spearman_cor_overall) %>% 
  arrange(desc(`.`))

pearson_cor_overall <-  pearson_cor_overall[,2] %>% 
  data.frame()
pearson_cor_overall <- cbind(cor_colnames, pearson_cor_overall) %>% 
  arrange(desc(`.`))

spearman_cor_overall %>% 
  left_join(pearson_cor_overall, by = "cor_colnames") %>% 
  rename(Feature = cor_colnames, Spearman = `..x`, Pearson = `..y`) %>% 
  filter(Feature != "Overall") %>% 
  head(10)

# So now we have see there are several high correlations between the overall rating and the skillset/wage/value. 
# Now let's try to find players that a fairly similar. To do so, I will use K-Means: 

fifa_cluster <-  fifa %>% 
  filter(PositionGroup != "Unknown") %>% 
  filter(PositionGroup != "GK") %>% 
  mutate(RoomToGrow = Potential - Overall) %>% 
  select_if(is.numeric) %>% 
  select(-ID, -`Jersey Number`, -starts_with("Value"), -starts_with("Wage"), - Overall, -Potential, -starts_with("GK"))
  # I deleted the columns I don't want to use in my comparison, as I want to compare the players on their skills, not
  # on their rating, wage or value as those can give a wrongful image.
fifa_scaled <- scale(fifa_cluster)

set.seed(1)
# Initialize total sum of squares error: wss
wss <- 0
# For 1 to 30 cluster centers
for (j in 1:20) {
  km_out <- kmeans(fifa_scaled, centers = j, nstart = 10)
  wss[j] <- km_out$tot.withinss  # save the total tot the tsse (total sum of squares error)
}

wss_df <- data.frame(num_cluster = 1:20, wgss = wss)
# Let's quickly make a chart so we can determine the optimal number of clusters (k)
ggplot(data = wss_df, aes(x = num_cluster, y = wgss)) +
  geom_line() +
  geom_point(color = "#0099FF", size = 4) +
  geom_curve(x = 14, xend=8, y = 3000000, yend = 290500, color = "red") +
  ggtitle("Determining the optimal k")

# The red line in the above chart points out the best k: 8. So let's set k to 8
k <-  8
wisc.km <- kmeans(scale(fifa_cluster), centers = k, nstart = 20)
  # creating a k-means model on the data appointing it to wisc.km

fifa_cluster <-  fifa %>% 
  filter(PositionGroup != "Unknown") %>% 
  filter(PositionGroup != "GK") %>% 
  mutate(Cluster = wisc.km$cluster)
  # with this I make sure the clustergroup is assigned back to the dataframe for all players other than goalies and "unknown

cluster_analysis <- fifa_cluster %>% 
  select(Name, Club, Age, PositionGroup, Overall, Potential, Cluster, Value, Wage, Superstar)

table(cluster_analysis$Cluster, cluster_analysis$PositionGroup)

# let's create an arranged dataframe so we can use it easily for analysis:
arranged_cluster <- cluster_analysis %>% 
  mutate(Cluster = as.character(Cluster)) %>% 
  arrange(desc(Overall)) %>% 
  group_by(Cluster)

# now let's look at the highest rated players for each cluster, and plot them:
cluster_analysis %>% 
  mutate(Cluster = paste("Cluster: ", Cluster, sep = "")) %>% 
  arrange(desc(Overall)) %>% 
  group_by(Cluster) %>% 
  slice(1:20) %>% 
  ggplot(aes(x = Overall, y = Value)) +
  geom_point(position = "jitter") +
  scale_fill_manual(values = c("grey", "#0099FF")) +
  scale_y_continuous(labels = dollar_format(prefix = "Euro ")) +
  facet_wrap(~ factor(Cluster), scales = "free", ncol = 2) +
  theme(legend.position = "none") +
  ggtitle("20 plotted players with the higest rating for each cluster")

# If we look at the different clusters, you can see the 1rst Cluster has the higest ratings, let's see who is in it:
cluster_analysis %>% 
  select(Name, Club, Cluster, Value, PositionGroup) %>%  
  filter(Cluster == 4) %>% 
  head(20)

# ---- Results ----
# It would be great if now we could, based on the clustering, find a cheaper player with the same set of qualities but
# for a lower salary. Let's see if we can write a function to do so:

similar_players <- function(player, num_results, return_fraction){
 
  cluster_filter <- cluster_analysis$Cluster[cluster_analysis$Name == player]
  player_value <- cluster_analysis$Value[cluster_analysis$Name == player]
  
  cluster_analysis %>% 
    filter(Cluster == cluster_filter, 
           Value >= (player_value * (1 - return_fraction)) & Value <= player_value * (1 + return_fraction)) %>% 
    head(num_results)
}

# Now we can try to buy players that are comparable with other players, only for a different Value. Let's check if there is 
  # someone who can be compared with Ronaldo:
similar_players("Cristiano Ronaldo", 30, .05)

# Now let's see if we can predict the value of the players, as we now also can see the similar players:
fifa_int <- fifa[ , map_lgl(fifa, is.numeric)]
mcor <-  as.data.frame(cor(fifa_int, use = "complete.obs"))

# Check where the correlation is the highest so those can be used for the fitting
temp7 <- mcor["Value"]
temp8 <- subset(temp7, Value > 0.30)
rownames(temp8)
  
  # Let's create a train and test set. I will base this on 60% for the training.
set.seed(101)
sample = sample.split(fifa, SplitRatio = 0.6)
train <- subset(fifa, sample == TRUE)
test <- subset(fifa, sample == FALSE)

  # Let's fit the outcome of temp8 to the linear model using lm(). 
fit <- lm(Value ~ Overall + Potential + Wage + `Skill Moves` + `International Reputation` + ShortPassing + 
            LongPassing + BallControl + Reactions + Vision + Composure
            , data = train, na.action = na.omit) 
summary(fit)

# Based on the model, I now will try to predict the value of a player based on only the 8 parameters used in lm()
test_fit <-  predict(fit, newdata = test)
test_fit <-  round(test_fit, 0)

test$Predicted.Value <- test_fit

# i am only interested in seeing the name, value and the prediction so I can see if I was right or not.
Correct <- test[c("Name", "Value", "Predicted.Value")]
Correct <- Correct %>% 
  mutate(Difference = Value - Predicted.Value)

# Depending on the value of the value of the player, I think I am allowed to be max 20% off
Correct$Accurate <-  ifelse(Correct$Difference > 0.20 * Correct$Value, "No", 
                           ifelse(Correct$Difference < -(0.20 * Correct$Value), "No", "Yes"))

table(Correct$Accurate)

# Looking at the top 10 players, there are only 2 yess's, which shows it is not easy to predict the player's value
# as the market is rather strange. 
Correct %>% head(10)

# Now to finish up, let's find the bargains in the data:
# fifa %>% select(Name, Value, PositionGroup, Potential, Age, Overall)
fifa %>% 
  filter(Value == 0, PositionGroup != "Unknown") %>% 
  mutate(PotentialOver82 = ifelse(Potential >=82, "Yes", "No")) %>%
  ggplot(aes(x = Age, y = Overall)) +
  geom_vline(xintercept = 30, color = "black", linetype = 2) +
  geom_hline(yintercept = 65, color = "black", linetype = 2) +
  geom_text(aes(label = Name, color = PotentialOver82), check_overlap = T) +
  scale_color_manual(values = c("lightgrey", "#0099FF")) +
  theme(axis.text = element_text(face = "bold"), legend.position = "none") +
  xlim(15, 45) +
  ylim(50, 90) +
  ggtitle("Players who can be picked up for free", 
          subtitle = "Highlighted in blue are the ones with a potential of 82 and more")
  # 82 is not set to be a Superstar, though they are pretty good. That is why I set the limit at an potential score of 82. 
  # You can see in the chart, that all potential bargains are younger than 25, which makes it obvious they can still grow. 


# !! Conclusion will be drawn in the RMarkdown report only. 

