# this script allows developers to test their module easily

library(devtools)
load_all('../iNZightModules')
load_all('../iNZight')

try(ui$close(), TRUE)
ui <- iNZGUI$new()
wd <- getwd()
# ui$initializeGui(iris, addonDir = wd)

# demest module testing
remotes::install_github('StatisticsNZ/demdata')
nzincome <- demdata::nz.income

ui$initializeGui(nzincome, addonDir = wd)

# dataset > aggregate > [reorder and select]



ui$initializeGui(gapminder, addonDir = wd)