#' ---
#' title: "Introduction to R"
#' author: "Kevin  Shook"
#' date: November 23, 2017
#' output: pdf_document
#' ---

#' basic arithmetic: + - / *
1 + 1
2 * 2
4 / 3

#' data types
a <- 5
a
b <- a + 1
b
b <- "hello, world"
b

#' vectors
a <- c(1,2,3,4,5)
a
b <- a/2
b

#' character vectors
a <- c('1', '2', 'dog')
a

#' combining characters
paste('dog', 'cat')

#' works with vectors - vectors are recycled if too short
a <- c(1,2,3,4,5)
b <- "o'clock"
paste(a,b)

#' subsetting vectors
a <- seq(10,20)
a
#' subset by location
a[1:3]
a[-1]

#' subset by value
a > 15
a[ a > 15]
evens <- a[(a %% 2) == 0]
evens

#' commands
mean(a)
var(a)

#' get help on command
?var

#' data frames
#' loading data frame from a text file
CalgaryDailyPrecip <- read.csv("CalgaryDailyPrecip.csv", 
                               header = TRUE, stringsAsFactors = FALSE)


#' get info about a data frame
head(CalgaryDailyPrecip)
summary(CalgaryDailyPrecip)
nrow(CalgaryDailyPrecip)   # number of rows
ncol(CalgaryDailyPrecip)   # number of columns
names(CalgaryDailyPrecip)  # names inside the data frame

#' convert from 0.1 mm to mm
CalgaryDailyPrecip$precip <- CalgaryDailyPrecip$precip/10
summary(CalgaryDailyPrecip)

#' calculate mean
mean(CalgaryDailyPrecip$precip)
mean(na.omit(CalgaryDailyPrecip$precip))

#' convert date string to a real date
CalgaryDailyPrecip$realdate <- as.Date(CalgaryDailyPrecip$date, 
                                       format = "%Y-%m-%d")
head(CalgaryDailyPrecip)
summary(CalgaryDailyPrecip)

#' remove all missing values
CalgaryDailyPrecip <- na.omit(CalgaryDailyPrecip)
summary(CalgaryDailyPrecip)

#' get year
CalgaryDailyPrecip$year <- as.numeric(format(CalgaryDailyPrecip$realdate, "%Y"))
summary(CalgaryDailyPrecip)

#' subset by year
y2007 <- CalgaryDailyPrecip[CalgaryDailyPrecip$year == 2007,]
head(y2007)

#' or
y2005 <- subset(CalgaryDailyPrecip, year == 2005)
head(y2005)

#' aggregate by year
CalgaryYearlyPrecip <- aggregate(CalgaryDailyPrecip$precip, 
                                 by = list(CalgaryDailyPrecip$year), FUN = "sum")
head(CalgaryYearlyPrecip)

#' rename variables
names(CalgaryYearlyPrecip)
names(CalgaryYearlyPrecip) <- c('year', 'totalprecip')
head(CalgaryYearlyPrecip)

#' saving data frame to a csv file
write.csv(CalgaryYearlyPrecip, file = 'CalgaryYearlyPrecip.csv', 
          row.names = FALSE)

#' Statistics
#' plot histogram
hist(CalgaryYearlyPrecip$totalprecip)

#' fit normal distribution
library(MASS)
?fitdistr
fit <- fitdistr(CalgaryYearlyPrecip$totalprecip, "normal")
fit

#' t-test
t <- t.test(CalgaryYearlyPrecip$totalprecip)
t

#' plot autocorrelation function (ACF)
acf(CalgaryDailyPrecip$precip)
acf(CalgaryYearlyPrecip$totalprecip)

#' Mann-Kendall test for trends
library(Kendall)
?MannKendall
mk <- MannKendall(CalgaryYearlyPrecip$totalprecip)
summary(mk)

#' linear regression model
model <- lm(totalprecip~year, CalgaryYearlyPrecip)
summary(model)
coef(model)

#' ggplot2 graphing
annual <- read.csv("PrarieAnnualPrecip.csv")
summary(annual)
head(annual)

#' load library
library(ggplot2)

#' create basic xy graph
p <- ggplot(annual, aes(year, precipitation))
p <- p + geom_point()
p

#' change titles & replot
p <- p + xlab('Year')
p <- p + ylab('Annual precipitation (mm)')
p

#' add colour to points and change size
p <- p + geom_point(colour = "red", size = 3)
p


#' add regression curve
p <- p + stat_smooth(method = "lm")
p

#' replot, mapping colours to variables
p2 <- ggplot(annual, aes(year, precipitation, colour = site))
p2 <- p2 + geom_point(size = 3)
p2


#' add regression curve to each category
p2 <- p2 + stat_smooth(method = "lm", size = 2)
p2

#' change theme font sizes
p2 <- p2 + theme_grey(base_size = 18)
p2

#' do faceting
p3 <- ggplot(annual, aes(year, precipitation))
p3 <- p3 + geom_point() + facet_grid(site ~ .)
p3 <- p3 + stat_smooth(method = "lm")
p3 <- p3 + xlab('Year')
p3 <- p3 + ylab('Annual precipitation (mm)')
p3

#' box plot
p4 <- ggplot(annual, aes(site, precipitation, fill = site))
p4 <- p4 + geom_boxplot()
p4

#' histograms
p5 <- ggplot(annual, aes(x = precipitation, fill = site))
p5 <- p5 + geom_histogram(position = 'dodge')
p5

#' faceting
p5 <- p5 + facet_grid(. ~ site)
p5

#' density plots
p6 <- ggplot(annual, aes(x = precipitation, fill = site))
p6 <- p6 + geom_density(alpha = 0.4)
p6

#' save plot
ggsave('DensityPlot.png')

#' Final slides

