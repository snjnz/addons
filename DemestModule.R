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
        response_var = "ANY",
        response_type = "ANY",
        exposure_var = "ANY",
        tab_data = "ANY",
        exposure = "ANY",
        used_vars = "ANY"
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
                    set_vars()
                    visible(response_type) <<- TRUE
                }
            )
            lbl <- glabel("Response variable: ")
            tbl_response[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl_response[ii, 2:3, expand = TRUE] <- response_var
            ii <- ii + 1

            exposure_var <<- gcombobox(c("None", varnames),
                selected = 1L,
                handler = function(h, ...) set_vars()
            )
            lbl <- glabel("Exposure variable: ")
            tbl_response[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl_response[ii, 2:3, expand = TRUE] <- exposure_var
            ii <- ii + 1

            response_type <<- gcombobox(c("Counts", "Values"),
                selected = 0,
                handler = function(h, ...) {
                    set_vars()
                    visible(g_vars) <- TRUE
                    visible(g_response) <- FALSE
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

            used_vars <<- gtable(varnames, multiple = TRUE)
            addHandlerSelectionChanged(used_vars,
                handler = function(h, ...) updatePlot()
            )
            tbl_vars[ii, 3, expand = TRUE] <- used_vars
            size(used_vars) <<- c(-1, 200)
            ii <- ii + 1


        },
        set_vars = function() {
            if (response_var$get_index() == 0) return()
            if (response_type$get_index() == 0) return()

            var_y <- svalue(response_var)
            var_n <- if (exposure_var$get_index() > 1L) svalue(exposure_var) else NULL
            r_type <- svalue(response_type)

            vars_all <- names(raw_data)
            vars <- vars_all[vars_all %notin% c(var_y, var_n)]

            chosen_vars <- svalue(used_vars)
            blockHandlers(used_vars)
            if (length(chosen_vars)) {
                chosen_vars <- chosen_vars[chosen_vars %in% vars]
                used_vars$set_items(vars)
                used_vars$set_value(chosen_vars)
            } else {
                used_vars$set_items(vars)
            }
            unblockHandlers(used_vars)

            d_types <- NULL
            d_scales <- NULL

            tab_data <<- make_dem_array(raw_data, var_y, vars, r_type, d_types, d_scales)
            exposure <<- make_dem_array(raw_data, var_n, vars, "Counts", d_types, d_scales)

            updatePlot()
        },
        make_dem_array = function(data, y, x, type, dimtypes, dimscales) {
            if (is.null(y)) return(NULL)
            arr <- tapply(raw_data[[y]], raw_data[x], c)
            f <- switch(type,
                Counts = dembase::Counts,
                Values = dembase::Values
            )
            f(arr, dimtypes, dimscales)
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
            vars <- c(tvar, ovar)

            tmp <- tab_data
            etmp <- exposure
            if (length(svalue(used_vars)) && !is.null(exposure)) {
                if (length(svalue(used_vars)) < length(vars)) {
                    cvar <- vars[vars %notin% svalue(used_vars)]
                    vars <- vars[vars %in% svalue(used_vars)]
                    tmp <- dembase::collapseDimension(
                        tmp,
                        dimension = cvar,
                        weights = exposure
                    )
                    etmp <- dembase::collapseDimension(exposure, dimension = cvar)
                }
            }
            if (!is.null(exposure)) {
                tmp <- tmp / etmp
            }
            if (length(ovar) > 3) {
                # collapse down dimensions; requires counts
                stop("Cannot handle more than 4 dims yet")
            }

            df <- as.data.frame(tmp,
                direction = "long",
                midpoints = tvar)

            print(vars)
            p <- ggplot2::ggplot(df,
                ggplot2::aes_(
                    as.name(vars[1]),
                    ~value,
                    colour = if (length(vars) > 1) as.name(vars[2]) else NULL
                )
            ) +
                ggplot2::geom_path() +
                ggplot2::ylab(svalue(response_var)) +
                ggplot2::theme_minimal()

            if (length(vars) == 3) {
                p <- p +
                    ggplot2::facet_wrap(
                        ggplot2::vars(!!rlang::sym(vars[3]))
                    )
            } else if (length(vars) == 4) {
                p <- p +
                    ggplot2::facet_grid(
                        ggplot2::vars(!!rlang::sym(vars[3])),
                        ggplot2::vars(!!rlang::sym(vars[4]))
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
