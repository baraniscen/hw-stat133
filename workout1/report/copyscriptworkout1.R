#---
#  title: "Data preparation"
#---

getwd()
setwd()

dataset_workout <- read.csv("desktop/hw-stat133/workout1/data/nba2018.csv", header=TRUE)

str(dataset_workout)
dataset_workout$experience <- as.integer(dataset_workout$experience) #experience as integers
str(dataset_workout)

dataset_workout$salary <- (dataset_workout$salary)*10^-6 #salaries to million

levels(dataset_workout$position)[levels(dataset_workout$position)=="C"] <- "center" #rename to center
levels(dataset_workout$position)[levels(dataset_workout$position)=="PF"] <- "power_fd" #rename to power_fd
levels(dataset_workout$position)[levels(dataset_workout$position)=="PG"] <- "point_guard" #rename to point_guard
levels(dataset_workout$position)[levels(dataset_workout$position)=="SF"] <- "small_fwd" #rename to small_fwd
levels(dataset_workout$position)[levels(dataset_workout$position)=="SG"] <- "shoot_guard" #rename to shoot_guard
dataset_workout

#new variables created 
dataset_workout <-  mutate(dataset_workout, missed_fg = field_goals_atts - field_goals, missed_ft = points1_atts - points1, rebounds = off_rebounds + def_rebounds, efficiency = (points + total_rebounds + assists + steals + blocks - missed_fg - missed_ft - turnovers)/(dataset_workout$games)
)
dataset_workout

summary(dataset_workout$efficiency) #check the sink
sink('efficiency-summary.txt')
summary(dataset_workout$efficiency)
sink()

#creating nba2018-teams.csv


sink('teams-summary.txt')  #check the sink
summarise(
  group_by(dataset_workout, team),
  experience_total = sum(experience),
  total_salary = sum(salary),
  points_3 = sum(points3),
  points_2 = sum(points2),
  points_1 = sum(points1),
  total_points = sum(points1) + sum(points2) + sum(points3),
  total_off_rebounds = sum(off_rebounds),
  total_def_rebounds = sum(def_rebounds),
  total_assists = sum(assists),
  total_steals = sum(steals),
  total_blocks = sum(blocks),
  total_turnovers = sum(turnovers),
  total_fouls = sum(fouls),
  total_efficiency = sum(efficiency)
)
sink()

summary_teams <- summarise(
  group_by(dataset_workout, team),
  experience_total = sum(experience),
  total_salary = sum(salary),
  points_3 = sum(points3),
  points_2 = sum(points2),
  points_1 = sum(points1),
  total_points = sum(points1) + sum(points2) + sum(points3),
  total_off_rebounds = sum(off_rebounds),
  total_def_rebounds = sum(def_rebounds),
  total_assists = sum(assists),
  total_steals = sum(steals),
  total_blocks = sum(blocks),
  total_turnovers = sum(turnovers),
  total_fouls = sum(fouls),
  total_efficiency = sum(efficiency)
)

write.csv(summary_teams <- summarise(
  group_by(dataset_workout, team),
  experience_total = sum(experience),
  total_salary = sum(salary),
  points_3 = sum(points3),
  points_2 = sum(points2),
  points_1 = sum(points1),
  total_points = sum(points1) + sum(points2) + sum(points3),
  total_off_rebounds = sum(off_rebounds),
  total_def_rebounds = sum(def_rebounds),
  total_assists = sum(assists),
  total_steals = sum(steals),
  total_blocks = sum(blocks),
  total_turnovers = sum(turnovers),
  total_fouls = sum(fouls),
  total_efficiency = sum(efficiency)
), 'nba2018-teams.csv')

