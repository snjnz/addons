# this script allows developers to test their module easily

library(devtools)
load_all('../iNZightModules')
load_all('../iNZight')

try(ui$close(), TRUE)
ui <- iNZGUI$new()
wd <- getwd()
ui$initializeGui(iris, addonDir = wd)
