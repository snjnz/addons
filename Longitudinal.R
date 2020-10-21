#' @name Longitudinal Module
#' @author Sophie Jones
#' @desc A module used to demonstrate iNZight's add-on functionality
#'
#' @import longZight
#'
#' @export LongModule
#' @exportClass LongModule
LongModule <- setRefClass(
    "LongModule",
    contains = "CustomModule",
    fields = list(
        GUI = "ANY",
        activeData  = "data.frame",
        loadedData  = "ANY",
        timeVar = "ANY",
        timeVarSelect = "ANY",
        indivVar = "ANY",
        indivVarSelect = "ANY",
        responseVar = "ANY",
        responseVarSelect = "ANY",
        completeCases = "logical",
        completeCasesSelect = "ANY",
        timeFactor = "logical",
        timeFactorSelect = "ANY",
        colourVar = "ANY",
        colourVarSelect = "ANY",
        popoutVar = "ANY",
        popoutVarSelect = "ANY",
        plotType = "ANY",
        plotTypeSelect = "ANY",
        reversePopoutVar = "logical",
        mainGrp = "ANY",
        modwin = "ANY",
        g3 = "ANY",
        g4 = "ANY",
        # The User Interface (UI) should modify these "fields",
        # which can be used by other components of the module
        # (for example, plotting)
        colour = "character"
    ),
    methods = list(
        initialize = function(gui, name) {
            callSuper(gui,
                name = name,
                embedded = TRUE
            )

            ## you must specify any necessary packages for the module:
            install_dependencies("longZight")#, optional = nonessential_packages)

            activeData <<- GUI$getActiveData()

            frameFont = list(weight = "bold")

            modwin <<- GUI$initializeModuleWindow(.self,
                                                 title = "Longitudinal Visualisation", scroll = TRUE)
            mainGrp <<- modwin$body

            g1 = gframe("Individual Information", pos = 0.5, horizontal = FALSE,
                        container = mainGrp)
            g2 = gframe("Time Information", pos = 0.5, horizontal = FALSE,
                        container = mainGrp)
            g3 <<- gframe("Variable Information", pos = 0.5, horizontal = FALSE,
                        container = mainGrp)
            g4 <<- gframe("Plot Options", pos = 0.5, horizontal = FALSE,
                        container = mainGrp)
            visible(g3) <<- FALSE
            enabled(g4) <<- FALSE

            g1$set_borderwidth(8)
            g2$set_borderwidth(8)
            g3$set_borderwidth(8)
            g4$set_borderwidth(8)

            g1_layout = glayout(container = g1)
            indivVarSelect<<- gcombobox(names(activeData), selected = 0,
                                       handler = function(h, ...) {
                                           indivVar <<- svalue(h$obj)
                                           updateLZObject()
                                           updatePlot()
                                       })

            g1_layout[1, 1:2, expand = TRUE] = indivVarSelect

            g2_layout = glayout(container = g2)
            timeVarSelect<<- gcombobox(names(activeData), selected = 0,
                                       handler = function(h, ...) {
                                           timeVar <<- svalue(h$obj)
                                           if (sapply(activeData, class)[timeVar] %in% c("logical", "character")) {
                                               visible(timeFactorSelect) <<- FALSE
                                               timeFactor <<- TRUE
                                           } else {
                                               visible(timeFactorSelect) <<- TRUE
                                               timeFactor <<- svalue(timeFactorSelect)
                                           }
                                           updateLZObject()
                                           updateResponse()
                                           updatePlot()
                                       })

            timeFactor <<- FALSE
            timeFactorSelect <<- gcheckbox("Treat as Waves/Categorical", checked = FALSE,
                                              handler = function(h, ...) {
                                                  timeFactor <<- svalue(h$obj)
                                                  updateLZObject()
                                                  updateResponse()
                                                  updatePlot()
                                              })
            visible(timeFactorSelect) <<- FALSE

            g2_layout[1, 1:2, expand = TRUE] = timeVarSelect
            g2_layout[2, 1:2, expand = TRUE] = timeFactorSelect

            g3_layout = glayout(container = g3)
            responseVarSelect <<- gcombobox(names(activeData), selected = 0,
                                         handler = function(h, ...) {
                                             responseVar <<- svalue(h$obj)
                                             updateResponse()
                                             updatePlot()
                                         })

            #time_plot_group <- ggroup(horizontal = FALSE, container = g3_layout)

            colourVarSelect <<- gcombobox(character(0), selected = 0,
                                          handler = function(h, ...) {
                                              colourVar <<- svalue(h$obj)
                                              updatePlot()
                                          })
            enabled(colourVarSelect) <<- FALSE

            popoutVarSelect <<- gcombobox(character(0), selected = 0,
                                          handler = function(h, ...) {
                                              popoutVar <<- svalue(h$obj)
                                              updatePlot()
                                          })
            enabled(popoutVarSelect) <<- FALSE

            g3_layout[1, 1:2, expand = TRUE] = responseVarSelect
            lbl <- glabel("Colour By: ")
            g3_layout[2, 1] = lbl
            g3_layout[2, 2, expand = TRUE] = colourVarSelect
            lbl <- glabel("Focus on: ")
            g3_layout[3, 1] = lbl
            g3_layout[3, 2, expand = TRUE] = popoutVarSelect


            g4_layout = glayout(container = g4)
            lbl <- glabel("Plot type: ")
            g4_layout[1, 1] = lbl

            plotTypeSelect <<- gcombobox(character(0), selected = 0,
                                         handler = function(h, ...) {
                                             plotType <<- svalue(h$obj)
                                             if (plotType == "Time Plot") {
                                                 enabled(colourVarSelect) <<- TRUE
                                                 enabled(popoutVarSelect) <<- TRUE
                                             } else {
                                                 enabled(colourVarSelect) <<- FALSE
                                                 enabled(popoutVarSelect) <<- FALSE
                                             }
                                             updatePlot()
                                         })
            g4_layout[1, 2, expand = TRUE] = plotTypeSelect

            completeCases <<- TRUE
            completeCasesSelect <<- gcheckbox("Complete Cases Only", checked = TRUE,
                                              handler = function(h, ...) {
                                                  completeCases <<- svalue(h$obj)
                                                  updatePlot()
                                              })
            g4_layout[2, 1:2, expand = TRUE] = completeCasesSelect

            reversePopoutVar <<- FALSE
            #swapPopoutVar <- gbutton("Swap Focus",
            #                         hander = function(h, ...) {
            #                             reversePopoutVar <<- !reversePopoutVar
            #                             updatePlot()
            #                             })
            swapPopoutVar <- gcheckbox("Swap Focus", checked = reversePopoutVar,
                                       handler = function(h, ...) {
                                           reversePopoutVar <<- svalue(h$obj)
                                           updatePlot()
                                       })
            g4_layout[2, 1:2, expand = TRUE] = completeCasesSelect
            g4_layout[3, 1:2, expand = TRUE] = swapPopoutVar


            addHandlerSelectionChanged(indivVarSelect, function(h, ...) {
                updateLZObject()
            })

            addHandlerSelectionChanged(timeVarSelect, function(h, ...) {
                updateLZObject()
            })



            ## Footer
            btmGrp <- modwin$footer

            helpButton <- gbutton("Help",
                                  expand = TRUE,
                                  fill = TRUE,
                                  cont = btmGrp,
                                  handler = function(h, ...) {
                                      browseURL(
                                          "https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/add_ons/?topic=time_series"
                                      )
                                  }
            )
            homeButton <- gbutton("Home",
                                  expand = TRUE,
                                  fill = TRUE,
                                  cont = btmGrp,
                                  handler = function(h, ...) {
                                      close()
                                  }
            )

            cat("Running new module\n")
        },
        updateLZObject = function() {
            warning(class(indivVar))
            if (length(indivVar) == 0 || length(timeVar) == 0 ||
                (!any(indivVar %in% names(activeData))) ||
                (!any(timeVar %in% names(activeData))))
                return()
            if (timeFactor)
                loadedData <<- longZight::refactor_wave(longZight::load_long_data(activeData,
                                                         id_col = indivVar,
                                                         time_col = timeVar,
                                                         ts.args = FALSE))
            else
                loadedData <<- longZight::load_long_data(activeData,
                                                         id_col = indivVar,
                                                         time_col = timeVar,
                                                         ts.args = FALSE)
            visible(g3) <<- TRUE
            enabled(g4) <<- TRUE
            updatePlot()
        },
        updateResponse = function() {
            responseType = sapply(activeData, class)[responseVar]
            if (responseType %in% c("factor", "character")) {
                plotTypeSelect[]<- c("Alluvial", "Fettuccine")
                svalue(plotTypeSelect) <- "Alluvial"
                enabled(completeCasesSelect) <<- TRUE
            } else {
                plotTypeSelect[]<- c("Time Plot")
                svalue(plotTypeSelect) <- "Time Plot"
                enabled(completeCasesSelect) <<- FALSE
            }
            colourVarSelect[] <- names(activeData)[
                (sapply(activeData, class) %in% c("logical", "factor", "categorical") &
                     !(names(activeData) %in% c(indivVar, timeVar, responseVar)))
            ]
            popoutVarSelect[] <- names(activeData)[
                (sapply(activeData, class) %in% c("logical") &
                     !(names(activeData) %in% c(indivVar, timeVar, responseVar)))
            ]

        },
        ## add new methods to simplify your code
        updatePlot = function() {
            # This function is linked to the "refresh plot" button.
            # It should generate a plot purely from the values
            # of variables stored in "fields".
            # The UI then alters the values of the fields and calls this function.
            if (inherits(loadedData, "longZight") &&
                responseVar %in% names(activeData))
                if (plotType == "Alluvial") {
                    longZight::iz_plot_alluvial(loadedData, responseVar, completecases = completeCases)
                } else if (plotType == "Fettuccine") {
                    longZight::iz_plot_fettucine(loadedData, responseVar, completecases = completeCases)
                } else if (plotType == "Time Plot") {
                    warning(reversePopoutVar)
                    print(longZight::plot_long_line(loadedData, responseVar,
                                                    subsetvar1 = if (colourVar != 0) colourVar else NULL,
                                                    subsetvar2 = if (popoutVar != 0) popoutVar else NULL,
                                                    subsetvar2rev = reversePopoutVar,
                                                    ts.arg = FALSE))
                }
        },
        close = function() {
            cat("Closing module\n")

            callSuper()
        }
    ),
    ## This is currently required to get around namespace locking
    where = e
)
