# this script allows developers to test their module easily

library(devtools)

src = (function() {
  attr(body(sys.function()), 'srcfile')
})()$filename
if (is.null(src) || src == '') src = '.'
moddir = 'C:/Users/njones/Documents/Postgrad/iNZight/addons'
owd = setwd(dirname(src))


load_all('C:/Users/njones/Documents/Postgrad/iNZight/gWidgets2RGtk2')
load_all('C:/Users/njones/Documents/Postgrad/iNZight/iNZight')
load_all('C:/Users/njones/Documents/Postgrad/iNZight/iNZightRegression')
load_all('C:/Users/njones/Documents/Postgrad/iNZight/iNZightTS')
load_all('C:/Users/njones/Documents/Postgrad/iNZight/iNZightModules')
load_all('C:/Users/njones/Documents/Postgrad/longZight')
setwd(owd)

try(ui$close(), TRUE)
ui <- iNZGUI$new()
wd <- getwd()
chns.data <- haven::read_sas("C:/Users/njones/Documents/Postgrad/stats-781/data/rst_12.sas7bdat")
chns.data %<>% dplyr::mutate(A5E = factor(A5E))
ui$initializeGui(chns.data, addonDir = moddir)

wage_data2 <- brolgar::wages %>%
  tibble::as_tibble() %>%
  dplyr::mutate(black = as.logical(black),
                hispanic = as.logical(hispanic),
                ged = as.logical(ged),
                high_grade = as.factor(high_grade))

ui$initializeGui(wage_data2, addonDir = moddir)
