#' @name Multivariate
#' @version 1.0.1
#' @author Daniel Barnett
#' @desc A module used for multivariate graphics and analysis.
MultivariateModule <- setRefClass(
  "Multivariate",
  contains = "CustomModule",
  fields = list(
    GUI = "ANY",
    # The User Interface (UI) should modify these "fields",
    # which can be used by other components of the module
    # (for example, plotting)
    activeData = "data.frame",
    vars       = "character",
    binaryVar  = "numeric",
    mid        = "ANY",
    gtab       = "ANY",
    mrObject   = "ANY",
    byMRObject = "ANY",
    plotSet    = "ANY",
    objName    = "character",
    guessName  = "logical",
    slider.transp = "ANY",
    mrOptions = "ANY",
    text.code = "ANY",
    timer = "ANY",
    rhistory = "ANY",
    exportplotBtn = "ANY"
  ),
  methods = list(
    initialize = function(gui, name) {
      callSuper(
        gui,
        name = name,
        embedded = TRUE
      )

      initFields(
        GUI = GUI,
        plotSet = list(),
        objName = "response",
        guessName = TRUE,
        mrObject = NULL,
        mrOptions = list(),
        rhistory = NULL
      )

      activeData <<- GUI$getActiveData()

      ## you must specify any necessary packages for the module:
      install_dependencies(c("vegan", "iNZightMultivariate", "GGally", "corrplot"))

      # dirty hack for now - will add a method to parent in a future update
      if (!requireNamespace("iNZightMultivariate", quietly = TRUE)) {
        gmessage("Unable to load the necessary packages.", type = "error")
        close()
      }
      eval(parse(text = "library(iNZightMultivariate)"), envir = e)

      method.labels <- c(
        "Pairs Plot"                          = "pairs",
        "Correlation Pairs  Plot"             = "pairs_corr",
        "Parallel Coordinates"                = "pcp",
        "Principal Components Analysis"       = "pca",
        "Multidimensional Scaling"            = "mds"# ,
        # "Non-Metric Multidimensional Scaling" = "nmds"
      )

      top <- gvbox(container = mainGrp)
      mid <<- glayout(container = mainGrp, expand = FALSE)

      method.selection <- glayout()
      method.selection[1, 1] <- glabel("Method: ")
      method.combobox <- gcombobox(
        names(method.labels),
        handler = function(h, ...) updateOptions(),
        selected = 2
      )
      method.selection[1, 2, expand = TRUE] <- method.combobox
      add(top, method.selection)

      lab <- glabel("Select variables:")
      font(lab) <- list(weight = "bold", size = 11)
      # add(top, lab, anchor = c(-1, -1))

      numeric.vars <- names(which(sapply(activeData, is.numeric)))
      vars <<- numeric.vars
      gtab <<- gtable(vars, multiple = TRUE)
      size(gtab)  <<- c(-1, 350)

      gtab.container <- glayout()
      gtab.container[1,1, expand = TRUE, anchor = c(-1, 1)] <- lab
      gtab.container[2,1, expand = TRUE] <- gtab
      add(top, gtab.container)

      timer <<- NULL
      addHandlerSelectionChanged(gtab, handler = function(h, ...) {
        if (!is.null(timer))
          if (timer$started) timer$stop_timer()
        timer <<- gtimer(1000, function(...) updateOptions(), one.shot = TRUE)
      })

      colourvar.selection <- glayout()
      colourvar.selection[1, 1] <- glabel("Colour by: ")
      colourvar.combobox <- gcombobox(
        c("None", names(which(!sapply(activeData, is.numeric)))),
        handler = function(h, ...) updateOptions()
      )
      colourvar.selection[1, 2, expand = TRUE] <- colourvar.combobox
      add(top, colourvar.selection)

      shapevar.selection <- glayout()
      shapevar.selection[1, 1] <- glabel("Shape by: ")
      shapevar.combobox <- gcombobox(
        c("None", names(which(!sapply(activeData, is.numeric)))),
        handler = function(h, ...) updateOptions()
      )
      shapevar.selection[1, 2, expand = TRUE] <- shapevar.combobox
      add(top, shapevar.selection)

      slider.transp <<- gslider(0, 100, 10)
      addHandlerChanged(slider.transp, function(h, ...) {
        if (!is.null(timer))
          if (timer$started) timer$stop_timer()
        timer <<- gtimer(1000, function(...) updateOptions(), one.shot = TRUE)
      })

      transp.layout <- glayout()

      transp.layout[1, 1] <- glabel("Transparency: ")
      transp.layout[1, 2, expand = TRUE] <- slider.transp

      add(top, transp.layout)

      n.numeric <- sum(sapply(activeData, is.numeric))

      ndim.selection <- glayout()

      ndim.selection[1, 1] <- glabel("N dimensions to decompose to: ")
      select.ndim <- gslider(2:n.numeric, value = n.numeric,
                             handler = function(h, ...) updateOptions())
      ndim.selection[1, 2, expand = TRUE] <- select.ndim
      add(top, ndim.selection)

      dimensions.selection <- glayout()


      select.dim1 <- gcombobox(1:n.numeric, selected = 1,
                               handler = function(h, ...) updateOptions())
      select.dim2 <- gcombobox(1:n.numeric, selected = 2,
                               handler = function(h, ...) updateOptions())
      dim.label <- glabel("Dimensions to Plot: ")
      # font(dim.label) <- list(weight = "bold", size = 11)
      dimensions.selection[1, 1:4, expand = TRUE] <- dim.label
      dimensions.selection[2, 1] <- glabel("x: ")
      dimensions.selection[2, 2, expand = TRUE] <- select.dim1
      dimensions.selection[2, 3] <- glabel("y: ")
      dimensions.selection[2, 4, expand = TRUE] <- select.dim2

      add(top, dimensions.selection)

      # text.code <<- gtext("## R code will appear here")

      # add(mainGrp, text.code, expand = TRUE, fill = TRUE)

      export.plot <- gbutton("Interactive Plot", handler = function(h, ...) {
        suppressWarnings(
          print(plotly::ggplotly())
        )
      })

      add(mainGrp, export.plot)

      visible(gtab.container)       <- TRUE
      visible(transp.layout)        <- FALSE
      visible(colourvar.selection)   <- FALSE
      visible(dimensions.selection) <- FALSE
      visible(ndim.selection)       <- FALSE
      visible(export.plot)          <- FALSE

      updateOptions <- function() {
        mrOptions$group <<- if (svalue(colourvar.combobox, TRUE) == 1) NULL else svalue(colourvar.combobox)
        mrOptions$shape <<- if (svalue(shapevar.combobox, TRUE) == 1) NULL else svalue(shapevar.combobox)
        mrOptions$vars <<- if (is.null(svalue(gtab)) || length(svalue(gtab)) == 0) names(which(sapply(activeData, is.numeric))) else svalue(gtab)
        mrOptions$alpha <<- 1 - svalue(slider.transp) / 100

        mrOptions$prev.type <<- mrOptions$type
        mrOptions$type <<- method.labels[svalue(method.combobox)]
        mrOptions$dim1 <<- as.numeric(svalue(select.dim1))
        mrOptions$dim2 <<- as.numeric(svalue(select.dim2))

        prev.k <- mrOptions$k
        mrOptions$k <<- as.numeric(svalue(select.ndim))

        visible(gtab.container)       <- mrOptions$type %in% c("pcp", "pairs_corr", "pairs", "pca")
        visible(transp.layout)        <- mrOptions$type %in% c("pcp")
        visible(colourvar.selection)  <- mrOptions$type %in% c("pcp", "pca", "mds", "nmds")
        visible(shapevar.selection)   <- mrOptions$type %in% c("pca", "mds", "nmds")
        visible(dimensions.selection) <- mrOptions$type %in% c("pca", "mds", "nmds")
        visible(ndim.selection)       <- mrOptions$type %in% c("mds", "nmds")
        visible(export.plot)          <- mrOptions$type %in% c("pca", "pcp", "mds", "nmds")
        enabled(exportplotBtn)        <<- mrOptions$type %in% c("pca", "pcp", "mds", "nmds")

        if (mrOptions$type %in% c("pca")) {
          if (length(mrOptions$vars) > 1) {
            blockHandlers(select.dim1)
            select.dim1[] <- 1:length(mrOptions$vars)
            svalue(select.dim1) <- as.character(mrOptions$dim1)
            unblockHandlers(select.dim1)

            blockHandlers(select.dim2)
            select.dim2[] <- 1:length(mrOptions$vars)
            svalue(select.dim2) <- as.character(min(mrOptions$dim2, length(mrOptions$vars)))
            unblockHandlers(select.dim2)
          } else {
            mrOptions$vars <<- names(which(sapply(activeData, is.numeric)))
          }
        }

        if (mrOptions$type %in% c("pcp", "pairs_corr", "pairs")) {
          updatePlot()
        } else {
          updateMVObject()
        }
      }

      updateOptions()

      showhistbtn <- iNZight:::gimagebutton(
        stock.id = "history",
        tooltip = "View code history",
        handler = function(h, ...) {
              codehistory <- gtext("",
                                    expand = TRUE, fill = TRUE,
                                    font.attr = list(family = "monospace"),
                                    container = ggroup(
                                      container = gwindow("Code History",
                                                          width = 800, height = 600,
                                                          parent = GUI$win
                                      )
                                    )
              )

          svalue(codehistory) <- rhistory
          font(codehistory) <- list(family = "monospace")
        }
      )

      img.export <- system.file("images/toolbar-interact.png", package = "iNZight")
      exportplotBtn <<- iNZight:::gimagebutton(
        filename = img.export,
        size = "button",
        tooltip = "Export Interacive Plot"
      )
      addHandlerClicked(exportplotBtn, function(h, ...) {
        if (!enabled(h$obj)) return()
        suppressWarnings(
          print(plotly::ggplotly())
        )
      })
      enabled(exportplotBtn) <<- FALSE

      GUI$plotToolbar$update(NULL,
                             refresh = "updatePlot",
                             extra = list(showhistbtn, exportplotBtn)
      )

      cat("Running new module\n")


    },
    ## add new methods to simplify your code
    updatePlot = function() {
      closeTab("summary")
      closeTab("screeplot")
      showTab("plot")
      if (mrOptions$type %in% c("pcp", "pairs", "pairs_corr")) {
        plot_fun <- list(
          pcp = iNZightMultivariate::inz.parcoord,
          pairs = iNZightMultivariate::inzight.ggpairs,
          pairs_corr = iNZightMultivariate::inzight.corr
        )[[mrOptions$type]]

        plot_arg_names <- list(
          pcp = c("vars", "group", "alpha"),
          pairs = c("vars"),
          pairs_corr = c("vars")
        )[[mrOptions$type]]

        plot_args <- mrOptions[plot_arg_names]
        plot_args <- plot_args[!is.na(names(plot_args))]

        if (mrOptions$type == "pairs") {
          plot(c(0, 1), c(0, 1), ann = FALSE, bty = "n", type = "n", xaxt = "n", yaxt = "n")
          text(0.5, 0.5, "Generating pairs plot - please wait... ")
        }

        plot_exprs <- do.call(plot_fun, c(list(GUI$dataNameWidget$datName), plot_args))

        eval_env <- rlang::env(!!rlang::sym(GUI$dataNameWidget$datName) := activeData)

        eval_results <- lapply(plot_exprs, eval, envir = eval_env)

        plot_object <- eval_results[[length(eval_results)]]

        dev.hold()
        tryCatch(
          print(plot_object),
          finally = dev.flush()
        )

        # svalue(text.code) <<- paste0(unname(unlist(lapply(plot_exprs, rlang::expr_text))), collapse = "\n\n")
        rhistory <<- paste0(c("## Plot variables", unname(unlist(lapply(plot_exprs, rlang::expr_text)))), collapse = "\n\n")
        # font(text.code) <<- list(family = "monospace")
      } else if (mrOptions$type %in% c("pca", "mds", "nmds")) {

        plot_fun <- list(
          pca = iNZightMultivariate::plot_inzight.pca,
          mds = iNZightMultivariate::plot_inzight.mds,
          nmds = iNZightMultivariate::plot_inzight.nmds
        )[[mrOptions$type]]

          mvObject_name <- attr(mrObject, "var_name")

          plot_exprs <- plot_fun(
            mvObject_name,
            data = GUI$dataNameWidget$datName,
            colour = mrOptions$group,
            shape = mrOptions$shape,
            x = mrOptions$dim1,
            y = mrOptions$dim2
          )

          eval_env <- rlang::env(
            !!rlang::sym(GUI$dataNameWidget$datName) := activeData,
            !!rlang::sym(mvObject_name) := mrObject
          )

          eval_results <- lapply(plot_exprs, eval, envir = eval_env)

          plot_object <- eval_results[[length(eval_results)]]

          if (length(mrOptions$vars) > 1) {
            dev.hold()
            tryCatch(
              print(plot_object),
              finally = dev.flush()
            )
          } else {
            dev.hold()
            tryCatch({
              plot(1, 1, type = "n")
              text(0.5, 0.5, labels = "Please select more than one variable")
            }, finally = dev.flush())
          }

        attr(mrObject, "code")[["plot_intro"]] <<- "## Plot results"
        attr(mrObject, "code")[["plot"]] <<- paste0(unname(unlist(lapply(plot_exprs, rlang::expr_text))), collapse = "\n\n")
        # svalue(text.code) <<- paste0(attr(mrObject, "code"), collapse = "\n\n")
        # font(text.code) <<- list(family = "monospace")

        rhistory <<- paste0(attr(mrObject, "code"), collapse = "\n\n")

        if (mrOptions$type == "pca") {
          updateSummary()
        } else {

          showTab("plot")
        }
      }
    },
    updateMVObject = function() {
      analysis_fun <- list(
        pca = iNZightMultivariate::inzight.pca,
        mds = iNZightMultivariate::inzight.mds,
        nmds = iNZightMultivariate::inzight.nmds
      )[[mrOptions$type]]

      plot_arg_names <- list(
        pca = c("vars", "dim1", "dim2"),
        mds = c("vars", "k"),
        nmds = c("vars", "k")
      )[[mrOptions$type]]

      plot_args <- mrOptions[plot_arg_names]
      plot_args <- plot_args[!is.na(names(plot_args))]

      names(plot_args) <- replace(names(plot_args), names(plot_args) == "dim1", "x")
      names(plot_args) <- replace(names(plot_args), names(plot_args) == "dim2", "y")

      plot_exprs <- do.call(analysis_fun, c(list(GUI$dataNameWidget$datName), plot_args))

      eval_env <- rlang::env(!!rlang::sym(GUI$dataNameWidget$datName) := activeData)

      eval_results <- lapply(plot_exprs, eval, envir = eval_env)

      plot_object <- eval_results[[length(eval_results)]]


      mrObject <<- plot_object
      attr(mrObject, "var_name") <<- as.character(as.list(plot_exprs[[1]])[[2]])
      attr(mrObject, "code") <<- list(
        "## Perform analysis",
        analysis = paste0(unname(unlist(lapply(plot_exprs, rlang::expr_text))), collapse = "\n\n")
      )

      updatePlot()
    },
    updateSummary = function() {
      closeTab("summary")

      summary.text <- gtext(paste0(iNZightMultivariate::summary_inzight.pca(mrObject), sep = "\n"))
      font(summary.text) <- list(family = "monospace")
      add(GUI$plotWidget$plotNb, summary.text, label = "Summary", close.button = TRUE)
      showTab("plot")

      closeTab("screeplot")

      scree.graphics <- ggraphics(expand = TRUE)
      add(GUI$plotWidget$plotNb, scree.graphics, label = "Screeplot", close.button = TRUE)
      plot(mrObject, type = "l", main = sprintf("Screeplot of PCA on %s", GUI$dataNameWidget$datName))

      showTab("plot")
    },
    showTab = function(x = c("plot", "summary", "screeplot")) {
      if (GUI$popOut) return(invisible(NULL))
      x <- match.arg(x)
      svalue(GUI$plotWidget$plotNb) <<-
        which(names(GUI$plotWidget$plotNb) ==
                switch(x,
                       "plot" = "plot",
                       "summary" = "Summary",
                       "screeplot" = "Screeplot"
                )
        )
      invisible(NULL)
    },
    closeTab = function(x = c("plot", "summary", "screeplot")) {
      tab_name <- switch(
        x,
        "plot" = "plot",
        "summary" = "Summary",
        "screeplot" = "Screeplot"
      )

      if (tab_name %in% names(GUI$plotWidget$plotNb)) {
        showTab(x)
        GUI$plotWidget$closePlot()
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
