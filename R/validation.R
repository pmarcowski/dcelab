#' Validate Experiment Configuration
#'
#' Validates the structure and values of an experiment configuration object.
#'
#' @param config List. Configuration object loaded from config.yaml.
#'
#' @return Invisibly returns `NULL`. Throws an error if validation fails.
#'
#' @details
#' Validates the following configuration sections:
#' * `exp_id` - Must be a character string
#' * `design` - Must include n_sets, n_total, n_alts, alternatives
#' * `ui` - Must include buttons_text, shuffle_attributes, default_option,
#'   no_choice, explicit_choice
#' * `storage` (optional) - If present, validates S3 configuration
#' * `custom_attributes` (optional) - If present, validates structure
#'
#' @seealso [validate_storage_config()], [validate_custom_attributes()]
#'
#' @export
validate_config <- function(config) {
    # Define required fields and their validation functions
    required_fields <- list(
        "exp_id" = is.character,
        "design" = function(x) {
            is.list(x) &&
                all(
                    c("n_sets", "n_total", "n_alts", "alternatives") %in%
                        names(x)
                )
        },
        "ui" = function(x) {
            is.list(x) &&
                all(
                    c(
                        "buttons_text",
                        "shuffle_attributes",
                        "default_option",
                        "no_choice",
                        "explicit_choice"
                    ) %in%
                        names(x)
                )
        }
    )

    # Check each required field exists and is valid
    for (field in names(required_fields)) {
        if (!field %in% names(config)) {
            stop(sprintf("Missing required field: %s", field))
        }
        if (!required_fields[[field]](config[[field]])) {
            stop(sprintf("Invalid configuration for: %s", field))
        }
    }

    # Validate design parameters
    if (!is.numeric(config$design$n_sets) || config$design$n_sets <= 0) {
        stop("design.n_sets must be a positive number")
    }

    if (!is.numeric(config$design$n_total) || config$design$n_total <= 0) {
        stop("design.n_total must be a positive number")
    }

    if (config$design$n_total > config$design$n_sets) {
        stop("design.n_total cannot be greater than design.n_sets")
    }

    # Validate UI configuration
    if (!config$ui$shuffle_attributes %in% c(FALSE, "trial", "participant")) {
        stop("ui.shuffle_attributes must be FALSE, 'trial', or 'participant'")
    }

    if (
        !identical(config$ui$default_option, "random") &&
            !identical(config$ui$default_option, NULL) &&
            !config$ui$default_option %in% config$design$alternatives
    ) {
        stop(sprintf(
            "ui.default_option must be null, 'random', or one of: %s",
            paste(config$design$alternatives, collapse = ", ")
        ))
    }

    if (!is.null(config$ui$no_choice)) {
        if (
            !(isFALSE(config$ui$no_choice) || is.character(config$ui$no_choice))
        ) {
            stop("ui.no_choice must be either false or a character string")
        }
    }

    if (
        !is.null(config$ui$explicit_choice) &&
            !is.logical(config$ui$explicit_choice)
    ) {
        stop("ui.explicit_choice must be 'true' or 'false'")
    }

    # Validate optional configurations
    if (!is.null(config$completion)) {
        if (
            !is.list(config$completion) ||
                is.null(config$completion$url) ||
                !is.character(config$completion$url)
        ) {
            stop(
                "completion.url must be a character string if completion is configured"
            )
        }
    }

    # Storage configuration check and notification
    if (is.null(config$storage)) {
        message(
            "Note: No storage configuration provided. Data will not be saved."
        )
    } else {
        validate_storage_config(config$storage)
    }

    if (!is.null(config$custom_attributes)) {
        validate_custom_attributes(config)
    }

    invisible(NULL)
}

#' Validate Storage Configuration
#'
#' Validates storage provider configuration.
#'
#' @param storage List. Storage configuration section from config.yaml.
#'
#' @return Invisibly returns `NULL`. Throws an error if validation fails.
#'
#' @export
validate_storage_config <- function(storage) {
    storage_providers <- names(storage)
    if (length(storage_providers) == 0) {
        return(invisible(NULL))
    }

    # Define required fields for each provider
    provider_fields <- list(
        local = c("path"),
        s3 = c("bucket", "prefix", "region", "access_key", "secret_key"),
        gsheets = c("sheet_id"),
        dropbox = c("path"),
        gcs = c("bucket", "prefix")
    )

    # Validate configured providers
    for (provider in storage_providers) {
        if (!provider %in% names(provider_fields)) {
            stop(sprintf("Unsupported storage provider: %s", provider))
        }

        missing_fields <- setdiff(
            provider_fields[[provider]],
            names(storage[[provider]])
        )
        if (length(missing_fields) > 0) {
            stop(sprintf(
                "Missing required fields for %s provider: %s",
                provider,
                paste(missing_fields, collapse = ", ")
            ))
        }
    }

    invisible(NULL)
}

#' Validate Custom Attributes Configuration
#'
#' Validates custom attribute function definitions.
#'
#' @param config List. Full configuration object containing custom_attributes.
#'
#' @return Invisibly returns `NULL`. Throws an error if validation fails.
#'
#' @export
validate_custom_attributes <- function(config) {
    # Ensure custom_attributes is a list
    if (!is.list(config$custom_attributes)) {
        stop("custom_attributes must be a list")
    }

    # Validate each custom attribute configuration
    for (attr_name in names(config$custom_attributes)) {
        attr_config <- config$custom_attributes[[attr_name]]

        # Check attribute configuration structure
        if (!is.list(attr_config)) {
            stop(sprintf("Configuration for '%s' must be a list", attr_name))
        }

        # Verify required fields presence
        required_fields <- c("function_name", "attribute_label")
        missing_fields <- setdiff(required_fields, names(attr_config))

        if (length(missing_fields) > 0) {
            stop(sprintf(
                "Missing required fields for custom attribute '%s': %s",
                attr_name,
                paste(missing_fields, collapse = ", ")
            ))
        }

        # Validate field types and lengths
        if (
            !is.character(attr_config$function_name) ||
                length(attr_config$function_name) != 1
        ) {
            stop(sprintf(
                "'function_name' for '%s' must be a single string",
                attr_name
            ))
        }

        if (
            !is.character(attr_config$attribute_label) ||
                length(attr_config$attribute_label) != 1
        ) {
            stop(sprintf(
                "'attribute_label' for '%s' must be a single string",
                attr_name
            ))
        }
    }

    invisible(NULL)
}
