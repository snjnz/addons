#' @name Demographic Modelling Module
#' @author Tom Elliott
#' @version 0.0.1
#' @desc Bayesian small domain estimation
DemestModule <- setRefClass(
    "Demographic Modelling Module",
    contains = "CustomModule",
    fields = list(
        GUI = "ANY",
        raw_data = "ANY",
        arr_data = "ANY",
        tab_data = "ANY",
        response_var = "ANY",
        response_type = "ANY",
        exposure_var = "ANY", exposure = "ANY"
    ),
    methods = list(
        initialize = function(gui, name) {
            callSuper(gui,
                name = name,
                embedded = TRUE
            )

            initFields(
                raw_data = GUI$getActiveData()
            )

            install_dependencies(
                c("tidyr")
                # github = c(
                #     "StatisticsNZ/dembase",
                #     "StatisticsNZ/demest"
                # )
            )

            ## The main code for your module goes here,
            ## inside a top-level container called "mainGrp"
            mainGrp$set_borderwidth(5)

            ### -------------------------------------- Response
            g_response <- gexpandgroup("Response options",
                container = mainGrp
            )
            font(g_response) <- list(weight = "bold")

            g_response$set_borderwidth(5)
            tbl_response <- glayout(container = g_response,
                expand = TRUE)
            ii <- 1

            varnames <- colnames(raw_data)
            vartypes <- sapply(raw_data, iNZightTools::vartype)
            response_var <<- gcombobox(varnames,
                selected = 0,
                handler = function(h, ...) {
                    r <- svalue(response_var)
                    vn <- varnames[varnames != r]
                    arr_data <<- tapply(raw_data[[r]], raw_data[vn], c)
                    visible(response_type) <<- TRUE
                }
            )
            lbl <- glabel("Response variable: ")
            tbl_response[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl_response[ii, 2:3, expand = TRUE] <- response_var
            ii <- ii + 1

            exposure_var <<- gcombobox(c("None", varnames),
                selected = 1L,
                handler = function(h, ...) updatePlot()
            )
            lbl <- glabel("Exposure variable: ")
            tbl_response[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl_response[ii, 2:3, expand = TRUE] <- exposure_var
            ii <- ii + 1

            response_type <<- gcombobox(c("Counts", "Values"),
                selected = 0,
                handler = function(h, ...) {
                    # this will move to a separate method shortly
                    f <- switch(svalue(response_type),
                        Counts = dembase::Counts,
                        Values = dembase::Values
                    )
                    # optional specification of dimscales ?
                    tab_data <<- f(arr_data)
                    visible(g_vars) <- TRUE
                    visible(g_response) <- FALSE
                    updatePlot()
                }
            )
            visible(response_type) <<- FALSE
            lbl <- glabel("Response type: ")
            tbl_response[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl_response[ii, 2:3, expand = TRUE] <- response_type
            ii <- ii + 1

            ### -------------------------------------- Variable info
            g_vars <- gexpandgroup("Variable information",
                container = mainGrp
            )
            visible(g_vars) <- FALSE
            font(g_vars) <- list(weight = "bold")

            g_vars$set_borderwidth(5)
            tbl_vars <- glayout(container = g_vars,
                expand = TRUE)
            ii <- 1

            vars_ok <- gbutton("All good")
            tbl_vars[ii, 3, expand = TRUE] <- vars_ok
            ii <- ii + 1





        },
        ## add new methods to simplify your code
        updatePlot = function() {
            if (is.null(tab_data)) return()

            # This function is linked to the "refresh plot" button.
            # It should generate a plot purely from the values
            # of variables stored in "fields".
            # The UI then alters the values of the fields and calls this function.

            dtypes <- dembase::dimtypes(tab_data)
            tvars <- c("age", "time")
            tvar <- which(tvars %in% dtypes)
            if (length(tvar)) tvar <- tvar[1]
            else return() # no time variable?
            vars <- names(dtypes)
            ovar <- vars[-tvar]
            tvar <- vars[tvar]
            evar <- svalue(exposure_var, index = TRUE) - 1L
            evar <- if (evar > 0) vars[evar] else NULL

            tmp <- tab_data
            if (length(ovar) > 3) {
                # collapse down dimensions; requires counts
            }

            df <- as.data.frame(tmp,
                direction = "long",
                midpoints = tvar)

            p <- ggplot2::ggplot(df,
                ggplot2::aes_(
                    as.name(tvar),
                    ~value,
                    colour = if (length(ovar)) as.name(ovar[1]) else NULL
                )
            ) +
                ggplot2::geom_path() +
                ggplot2::ylab(svalue(response_var)) +
                ggplot2::theme_minimal()

            if (length(ovar) == 2) {
                p <- p +
                    ggplot2::facet_wrap(
                        ggplot2::vars(!!rlang::sym(ovar[2]))
                    )
            } else {
                p <- p +
                    ggplot2::facet_grid(
                        ggplot2::vars(!!rlang::sym(ovar[2])),
                        ggplot2::vars(!!rlang::sym(ovar[3]))
                    )
            }

            print(p)

            # gvar <- ovar[1] # grouping variable
            # if (length(ovar) > 2L) # subsetting variable(s)
            #     svar <- paste(" |", paste(ovar[-(1L:2L)], collapse = " + "))
            # else
            #     svar <- ""

            # fmla <- sprintf("~ %s%s", tvar, svar)
            # print(fmla)
            # dembase::dplot(eval(parse(text = fmla)),
            #     data = tab_data,
            #     groups = eval(rlang::ensym(gvar))
            # )

        },
        close = function() {
            cat("Closing module\n")

            callSuper()
        }
    ),
    ## This is currently required to get around namespace locking
    where = e
)
