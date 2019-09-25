DemoModule <- setRefClass(
    "Demo Module",
    contains = "CustomModule",
    fields = list(
        GUI = "ANY",
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

            ## something about installing packages
            # install_dependencies(list_of_packages)

            ## The main code for your module goes here,
            ## inside a top-level container called "mainGrp"
            label <- glabel("This is a demo module",
                container = mainGrp)

            ## The UI provides a way for users to change values of "fields"
            colour <<- "cyan"
            colBox <- gcombobox(c("cyan", "orangered", "purple"),
                selected = 1,
                container = mainGrp,
                handler = function(h, ...) {
                    ## This handler gets called when the user selects
                    ## a different option.
                    ##
                    ## Assigning values to "fields" requires the double 
                    ## assignment operator, like so
                    colour <<- svalue(colBox)

                    ## since this value affects the plot, we need to call
                    ## the updatePlot function so the changes take effect
                    updatePlot()
                }
            )

            ## if you can do it without any user input, draw a basic plot
            updatePlot()

            cat("Running new module\n")
        },
        ## add new methods to simplify your code
        updatePlot = function() {
            # This function is linked to the "refresh plot" button.
            # It should generate a plot purely from the values
            # of variables stored in "fields".
            # The UI then alters the values of the fields and calls this function.
            plot(iris$Sepal.Length, iris$Sepal.height,
                col = colour)
        },
        close = function() {
            cat("Closing module\n")

            callSuper()
        }
    ),
    ## This is currently required to get around namespace locking
    where = e
)
