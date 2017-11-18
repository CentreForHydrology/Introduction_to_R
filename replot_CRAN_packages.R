
## plot trend
library(ggplot2)
pkgs <- read.csv(file = "pkgs.csv", header = TRUE, stringsAsFactors = FALSE)
pkgs <- na.omit(pkgs)
ggplot(pkgs, aes(as.POSIXct(first_release, format = "%Y-%m-%d %H:%M:%S"), index)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = '2 year', date_labels = '%Y') +
  scale_y_continuous(breaks = seq(0, 15000, 1000)) +
  xlab('') + ylab('') + 
  ggtitle('Number of R packages ever published on CRAN')

