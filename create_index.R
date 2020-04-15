# creates an index of the modules

require(dplyr)
require(iNZightModules)

`%||%` <- function(a, b)
    if (is.null(a)) b else a

create_index <- function() {
    mods <- getModules(".")
    tbl <- lapply(mods,
        function(mod) {
            list(
                Name = mod$display_name,
                Version = mod$meta$version %||% "",
                Description = mod$meta$desc %||% "",
                Author = mod$meta$author %||% ""
            ) %>% as_tibble()
        }
    ) %>% bind_rows()
    tbl
}
