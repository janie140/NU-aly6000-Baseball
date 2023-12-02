#Trang Tran, 21 Jan, ALY6000, Project 2 fixed version

cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
options(scipen = 100) # disables scientific notion for entire R session

library(pacman)
p_load(tidyverse)

#Assignment Part 1

data_2015 <- read_csv("2015.csv")
head(data_2015)

names(data_2015)

glimpse(data_2015)

p_load(janitor)
data_2015 <- clean_names(data_2015)
data_2015

happy_df <- select(data_2015, country, region, happiness_score, freedom)
happy_df

top_ten_df <- slice(happy_df, 1:10)
top_ten_df

no_freedom_df <- filter(happy_df, freedom < .2)

best_freedom_df <- arrange(happy_df, desc(freedom))

data_2015 <- mutate(data_2015, gff_stat = family + freedom + generosity)

happy_summary <- summarise(happy_df, mean_happiness = mean(happiness_score),
  max_happiness = max(happiness_score), mean_freedom = mean(freedom),
  max_freedom = max(freedom))

regional_stats_df <- group_by(happy_df, region) |> summarise(country_count = n(),
        mean_happiness = mean(happiness_score), mean_freedom = mean(freedom))
regional_stats_df

#Assignment Part 2

baseball <- read_csv("baseball.csv")

class(baseball)

age_stats_df <- group_by(baseball, Age) |> summarise(Count = n(), HR = mean(HR),
                                     H = mean(H), R = mean(R))
age_stats_df

baseball <- filter(baseball, AB > 0)

baseball <- mutate(baseball, BA = H/AB)

baseball <- mutate(baseball, BA = round(BA, digits = 3))

baseball <- mutate(baseball, OBP =  (H + BB) / (AB + BB))

baseball <- mutate(baseball, OBP = round(OBP, digits = 3))

strikeout_artist <- baseball |> arrange(desc(SO)) |> head(n = 10)

ggplot(baseball, aes(HR, RBI)) + geom_point()

eligible_df <- filter(baseball, AB >= 300 | G >= 100)

ggplot(eligible_df, aes(x=BA)) + geom_histogram(binwidth = .025, fill = "green",
                                             color = "blue")

eligible_df <- eligible_df |> mutate(RankHR =rank(-1 * HR, ties.method = "min"))
eligible_df

eligible_df <- eligible_df |> mutate(RankRBI=rank(-1*RBI, ties.method = "min"),
                                     RankOBP=rank(-1*OBP, ties.method = "min"))

eligible_df <- mutate(eligible_df, TotalRank = RankHR + RankRBI + RankOBP)

eligible_df <- arrange(eligible_df, TotalRank)

mvp_candidates <- head(eligible_df, n = 20)
mvp_candidates

mvp_candidates_abbreviated <- select(mvp_candidates, First, Last,
                                     RankHR, RankRBI, RankOBP, TotalRank)
mvp_candidates_abbreviated



