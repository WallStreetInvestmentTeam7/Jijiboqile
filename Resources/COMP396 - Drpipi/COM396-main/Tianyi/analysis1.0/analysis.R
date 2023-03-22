data <- read.csv("DATA/PART1/01.csv")
data <- data[,5]
timeseries <- ts(data, frequency = 365, start = c(2069,12,8), end = c(2072,9,2))
timeseriescomponents <- decompose(timeseries)
plot(timeseriescomponents)