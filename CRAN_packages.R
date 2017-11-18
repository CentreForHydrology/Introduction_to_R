# This code from ??


CRAN_page <- function(...) {
  file.path('https://cran.rstudio.com/src/contrib', ...)
}

## get list of currently available packages on CRAN
library(XML)
pkgs <- readHTMLTable(readLines(CRAN_page()),
                      which = 1, stringsAsFactors = FALSE)

## we love data.table
library(data.table)
setDT(pkgs)

## drop directories
pkgs <- pkgs[Size != '-']
## drop files that does not seem to be R packages
pkgs <- pkgs[grep('tar.gz$', Name)]

## package name should contain only (ASCII) letters, numbers and dot
pkgs[, name := sub('^([a-zA-Z0-9\\.]*).*', '\\1', Name)]

## grab date from last modified timestamp
pkgs[, date := as.POSIXct(`Last modified`, format = '%d-%b-%Y %H:%M')]
pkgs[, date := as.character(date)]

## keep date and name
pkgs <- pkgs[, .(name, date)]

## list of packages with at least one archived version
archives <- readHTMLTable(readLines(CRAN_page('Archive')),
                          which = 1, stringsAsFactors = FALSE)
setDT(archives)

## keep directories
archives <- archives[grep('/$', Name)]

## add packages not found in current list of R packages
archives[, Name := sub('/$', '', Name)]
pkgs <- rbind(pkgs,
              archives[!Name %in% pkgs$name, .(name = Name)],
              fill = TRUE)

## reorder pkg in alphabet order
setorder(pkgs, name)

## number of versions released is 1 for published packages
pkgs[, versions := 0]
pkgs[!is.na(date), versions := 1]

## mark archived pacakges
pkgs[, archived := FALSE]
pkgs[name %in% archives$Name, archived := TRUE]

## NA date of packages with archived versions
pkgs[archived == TRUE, date := NA]

## lookup release date of first version & number of releases
pkgs[is.na(date), c('date', 'versions') := {
  
  cat(name, '\n')
  
  ## download archive page
  page <- readLines(CRAN_page('Archive', name))
  
  ## extract date with regexp as HTML parsing can be slow :)
  date <- sub('.*([0-9]{2}-[A-Za-z]{3}-[0-9]{4} [0-9]{2}:[0-9]{2}).*', '\\1', page[10])
  
  ## convert to YYYY-mm-dd format
  date <- as.POSIXct(date, format = '%d-%b-%Y %H:%M')
  
  ## number of previous releases
  archived_versions <- length(page) - 9 - 4
  
  ## return
  list(as.character(date), versions + archived_versions)
  
}, by = name]

## rename cols
setnames(pkgs, 'date', 'first_release')

## order by date & alphabet
setorder(pkgs, first_release, name)
pkgs[, index := .I]
pkgs[c(250, 500, (1:9)*1000)]

## plot trend
library(ggplot2)
ggplot(pkgs, aes(as.Date(first_release), index)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = '2 year', date_labels = '%Y') +
  scale_y_continuous(breaks = seq(0, 15000, 1000)) +
  xlab('') + ylab('') + theme_bw() +
  ggtitle('Number of R packages ever published on CRAN')
ggsave("CRAN_packages.png")
write.csv(pkgs, 'pkgs.csv', row.names = FALSE)