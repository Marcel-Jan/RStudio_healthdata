# variable
v <- 3

# Load data
setwd("D:\\bestanden\\R")
weight_data <- read.csv("gewicht_en_vet_shift.csv", header=TRUE)

# Type data
colnames(weight_data) <- c("datumstr", "weight", "bodyfat_nocorr", "weight_delta",
                           "BMI", "bodyfat", "bodyfat_delta", "TBW", "muscle", "energy",
                           "bone", "walked_km", "biked_km", 
                           "kcal_burned", "training_load", "pushups",
                           "sleep_start", "sleep_quality", "sleep_duration", 
                           "sleep_minutes","steps", "stairs" , "pulse", "bloodpress_low",
                           "bloodpress_high", "commute_hours", "remarks", "food")

# What is in this variable?
str(weight_data)
summary(weight_data)

weight_data$date <- as.Date(weight_data$datumstr, "%d-%h-%y")
summary(weight_data$date)
weight_data$weight <- as.numeric(as.character(weight_data$weight))
summary(weight_data$weight)

weight_data$weight_delta <- as.numeric(as.character(weight_data$weight_delta))
weight_data$bodyfat_nocorr <- as.numeric(as.character(weight_data$bodyfat_nocorr))
weight_data$bodyfat <- as.numeric(as.character(weight_data$bodyfat))
weight_data$bodyfat_delta <- as.numeric(as.character(weight_data$bodyfat_delta))
weight_data$kcalsburned <- as.numeric(as.character(weight_data$kcal_burned))
weight_data$steps <- as.numeric(as.character(weight_data$steps))
weight_data$sleep_quality <- as.numeric(as.character(weight_data$sleep_quality))
weight_data$sleep_minutes <- as.numeric(as.character(weight_data$sleep_minutes))
weight_data$sleep_hours <- as.numeric(as.character(weight_data$sleep_minutes/60))
weight_data$bike_distance <- as.numeric(as.character(weight_data$biked_km))
weight_data$walking_distance <- as.numeric(as.character(weight_data$walked_km))
weight_data$stairs_climbed <- as.numeric(as.character(weight_data$stairs))

summary(weight_data)


# Let's graph
# ------------------------------------------------

library(ggplot2)

# Weight over time
weightgraph <- ggplot(data=weight_data, aes(x=date, y=weight))
weightgraph + geom_point()

# Let's zoom in
weightgraph + geom_line() +
  scale_x_date(limits = c(Sys.Date() - 200, NA))

# With smooth
weightgraph + geom_line() +
  scale_x_date(limits = c(Sys.Date() - 200, NA))  + geom_smooth(fill=NA)


# Different scales give different body fat percentages
bodyfatgraph <- ggplot(data=weight_data, aes(x=date, y=bodyfat_nocorr))
bodyfatgraph + geom_point() + geom_smooth(fill=NA)

# Fat after correction
bodyfatgraph <- ggplot(data=weight_data, aes(x=date, y=bodyfat))
bodyfatgraph + geom_point() + geom_smooth(fill=NA)

# Zoom in on last year
bodyfatgraph <- ggplot(data=weight_data, aes(x=date, y=bodyfat_nocorr))
bodyfatgraph + geom_point() + 
  scale_x_date(limits = c(Sys.Date() - 365, NA))



# kcals burned
kcalgraph <- ggplot(data=weight_data, aes(x=date, y=kcalsburned))
kcalgraph + geom_point()




# Let's see some correlations
# ------------------------------------------------

wkcal_graph <- ggplot(data=weight_data, aes(x=weight, y=kcalsburned))
wkcal_graph + geom_point()+ geom_smooth(fill=NA)

wdkcal_graph <- ggplot(data=weight_data, aes(x=weight_delta, y=kcalsburned))
wdkcal_graph + geom_point()  + geom_smooth(fill=NA)

bfkcal_graph <- ggplot(data=weight_data, aes(x=bodyfat, y=kcalsburned))
bfkcal_graph + geom_point()

bfdkcal_graph <- ggplot(data=weight_data, aes(x=bodyfat_delta, y=kcalsburned))
bfdkcal_graph + geom_point() + geom_smooth(fill=NA)




# Aggregation
# ------------------------------------------------

weight_data$month <- as.Date(cut(weight_data$date,
                                breaks = "month"))

weight_data$month
summary(weight_data$month)

# Graph with kcals burned aggregated by month vs weight
month_graph = ggplot(data = weight_data,
       aes(month, kcalsburned)) +
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "line") + 
  scale_x_date(date_breaks = "1 years", date_labels = "%Y-%m-%d")

month_graph
month_graph + geom_point(aes(month, weight))
month_graph + geom_point(aes(month, (weight*1000)-70000))


# Graph with kcals burned aggregated by month vs body fat
month_bf_graph = ggplot(data = weight_data,
                     aes(month, kcalsburned)) +
  stat_summary(fun.y = sum, # adds up all observations for the week
               geom = "line") + # or "line"
  scale_x_date(date_breaks = "1 years", date_labels = "%Y-%m-%d")

# month_bf_graph
month_bf_graph + geom_point(aes(month, (bodyfat*1000) ))





# ----------------------------------------------------------------------



# Aggregate per week

weight_data$week <- as.Date(cut(weight_data$date,
                                breaks = "week",
                                start.on.monday = FALSE))

weight_data$week
weight_data$weight_delta
weight_data$kcalsburned

weightgraph <- ggplot(data=weight_data, aes(x=week, y=kcalsburned))
weightgraph + geom_point()



ggplot(data = weight_data,
       aes(week, kcalsburned)) +
  stat_summary(fun.y = sum, # adds up all observations for the week
               geom = "line") + # or "line"
  scale_x_date(date_breaks = "1 years", date_labels = "%Y-%m-%d")

ggplot(data = weight_data,
       aes(week, weight_delta)) +
  stat_summary(fun.y = sum, # adds up all observations for the week
               geom = "line") + # or "line"
  scale_x_date(date_breaks = "1 years", date_labels = "%Y-%m-%d")

