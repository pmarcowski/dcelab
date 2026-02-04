#' Application Utility Functions
#'
#' @description
#' Utility functions for the DCE Shiny application.
#' These functions handle choice set selection and custom attribute processing.

#' Select Choice Set
#'
#' @param rv Reactive values object
#' @param design Design matrix
#' @param bs Begin indices
#' @param es End indices
#' @param atts Attributes data frame
#' @param atts_lvls Attribute levels
#' @param atts_coding Coding types
#' @param atts_num Number of attributes
#' @param config Configuration
#' @return Current choice set matrix
select_choice_set <- function(
    rv,
    design,
    bs,
    es,
    atts,
    atts_lvls,
    atts_coding,
    atts_num,
    config
) {
    set <- design[bs[rv$set_num]:es[rv$set_num], ]
    choice_set <- idefix::Decode(
        des = set,
        n.alts = config$design$n_alts,
        lvl.names = rv$atts_labs,
        coding = atts_coding,
        alt.cte = config$design$alt_cte
    )[[1]]

    choice_set <- t(choice_set[, 1:atts_num])
    colnames(choice_set) <- config$design$alternatives
    rownames(choice_set) <- names(atts)

    if (!is.null(rv$custom_funcs)) {
        choice_set <- apply_custom_attributes(
            choice_set,
            rv$custom_funcs,
            config
        )
    }

    if (identical(config$ui$shuffle_attributes, "participant")) {
        if (is.null(rv$attribute_order)) {
            rv$attribute_order <- sample(rownames(choice_set))
        }
        choice_set <- choice_set[rv$attribute_order, , drop = FALSE]
    } else if (identical(config$ui$shuffle_attributes, "trial")) {
        choice_set <- choice_set[sample(nrow(choice_set)), , drop = FALSE]
    }

    return(choice_set)
}

#' Load Custom Attribute Functions
#'
#' @param config Configuration object
#' @param resources_path Path to resources directory
#' @return Named list of custom functions
load_custom_functions <- function(config, resources_path) {
    if (is.null(config$custom_attributes)) {
        return(list())
    }

    custom_file <- file.path(resources_path, "custom.R")
    if (!file.exists(custom_file)) {
        stop("custom.R file not found in experiment directory")
    }

    env <- new.env()
    source(custom_file, local = env)

    custom_funcs <- list()
    for (attr_name in names(config$custom_attributes)) {
        func_name <- config$custom_attributes[[attr_name]]$function_name
        if (!exists(func_name, envir = env)) {
            stop(sprintf("Function '%s' not found in custom.R", func_name))
        }

        func <- get(func_name, envir = env)
        if (!is.function(func)) {
            stop(sprintf("'%s' must be a function", func_name))
        }

        formals_list <- names(formals(func))
        if (!"context" %in% formals_list) {
            stop(sprintf(
                "Function '%s' must have parameter: context",
                func_name
            ))
        }

        custom_funcs[[attr_name]] <- list(
            func = func,
            label = config$custom_attributes[[attr_name]]$attribute_label,
            function_name = func_name
        )
    }

    custom_funcs
}

#' Apply Custom Attributes
#'
#' @param choice_set Choice set matrix
#' @param custom_funcs List of custom functions
#' @param config Configuration
#' @return Modified choice set
apply_custom_attributes <- function(choice_set, custom_funcs, config) {
    if (length(custom_funcs) == 0) {
        return(choice_set)
    }

    result <- choice_set

    for (attr_name in names(custom_funcs)) {
        func_info <- custom_funcs[[attr_name]]
        if (is.null(func_info$label)) {
            next
        }

        context <- list(
            choice_set = choice_set,
            config = config,
            alternatives = config$design$alternatives
        )

        attr_values <- vapply(
            seq_len(ncol(choice_set)),
            function(col) {
                context$col_index <- col
                tryCatch(
                    {
                        func_info$func(context = context)
                    },
                    error = function(e) {
                        warning(sprintf(
                            "Error in custom function '%s': %s",
                            attr_name,
                            e$message
                        ))
                        ""
                    }
                )
            },
            character(1)
        )

        if (any(attr_values != "")) {
            if (func_info$label %in% rownames(result)) {
                result <- result[
                    rownames(result) != func_info$label,
                    ,
                    drop = FALSE
                ]
            }
            new_row <- matrix(
                attr_values,
                nrow = 1,
                dimnames = list(func_info$label, colnames(choice_set))
            )
            result <- rbind(result, new_row)
        }
    }

    result
}

# Internal helper for null coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x
