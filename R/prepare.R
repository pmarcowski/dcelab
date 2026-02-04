#' Prepare Experiment for Deployment
#'
#' Validates experiment configuration and generates a D-efficient experimental
#' design. This must be run before [run_experiment()] or [deploy_experiment()].
#'
#' @param path Character. Path to the experiment directory.
#'
#' @return Invisibly returns the generated design object.
#'
#' @details
#' This function performs the following steps:
#' 1. Loads and validates `config.yaml`
#' 2. Reads attribute definitions from `attributes.csv`
#' 3. Generates a D-efficient design using the idefix package
#' 4. Saves the design to `www/design.rds`
#'
#' The design generation uses the Coordinate Exchange Algorithm (CEA) from
#' idefix to create a D-efficient choice design based on the specified
#' number of choice sets, alternatives, and attribute levels.
#'
#' @seealso [create_experiment()], [run_experiment()], [deploy_experiment()]
#'
#' @examples
#' \dontrun{
#' prepare_experiment("my_study")
#' }
#'
#' @export
prepare_experiment <- function(path) {
    # Validate path
    checkmate::assert_directory_exists(path)

    # Check required files exist
    config_file <- file.path(path, "config.yaml")
    atts_file <- file.path(path, "attributes.csv")

    if (!file.exists(config_file)) {
        cli::cli_abort("Configuration file not found: {.file {config_file}}")
    }
    if (!file.exists(atts_file)) {
        cli::cli_abort("Attributes file not found: {.file {atts_file}}")
    }

    cli::cli_alert_info("Preparing experiment at {.path {path}}")

    # Load and validate config
    config <- yaml::read_yaml(config_file)
    validate_config(config)
    cli::cli_alert_success("Configuration validated")

    # Load attributes
    atts <- read.csv(
        atts_file,
        na.strings = c(""),
        check.names = FALSE,
        encoding = "UTF-8"
    )

    # Calculate attribute levels
    atts_lvls <- as.numeric(lengths(lapply(atts, function(x) x[!is.na(x)])))
    atts_coding <- rep("E", length(atts_lvls))

    # Generate profiles and priors
    profiles <- idefix::Profiles(lvls = atts_lvls, coding = atts_coding)
    mu <- rep(0, ncol(profiles))
    sigma <- diag(length(mu))
    par_draws <- MASS::mvrnorm(10, mu, sigma)

    cli::cli_alert_info("Generating D-efficient design...")

    # Generate design
    design <- idefix::CEA(
        lvls = atts_lvls,
        coding = atts_coding,
        n.sets = config$design$n_sets,
        n.alts = config$design$n_alts,
        par.draws = par_draws
    )

    # Handle idefix return structure
    if (!is.null(design$BestDesign)) {
        # If BestDesign is already separated, use it
        design <- design$BestDesign
    } else if (
        inherits(design, "design_list") ||
            (is.list(design) && is.null(design$design))
    ) {
        cli::cli_alert_info(
            "Selecting best design from {length(design)} candidates..."
        )

        # Helper to get error value
        get_err <- function(d) {
            # Try DB.error first (Bayesian), then error (D-error)
            if (!is.null(d$DB.error)) {
                return(d$DB.error)
            }
            if (!is.null(d$error)) {
                return(d$error)
            }
            return(Inf)
        }

        errors <- vapply(design, get_err, numeric(1))
        best_idx <- which.min(errors)
        design <- design[[best_idx]]

        cli::cli_alert_success(
            "Selected design with error: {round(get_err(design), 4)}"
        )
    }

    # Ensure www directory exists
    www_dir <- file.path(path, "www")
    dir.create(www_dir, showWarnings = FALSE, recursive = TRUE)

    # Structuring design for app compatibility
    # Ensure we save a list with 'design' element (matrix)
    app_design <- list(
        design = design$design,
        error = if (!is.null(design$DB.error)) design$DB.error else design$error
    )

    # Save design
    design_file <- file.path(www_dir, "design.rds")
    saveRDS(app_design, file = design_file)

    # Copy config and attributes to www for app access
    file.copy(config_file, file.path(www_dir, "config.yaml"), overwrite = TRUE)
    file.copy(atts_file, file.path(www_dir, "attributes.csv"), overwrite = TRUE)

    # Copy intro/outro if they exist
    intro_file <- file.path(path, "intro.txt")
    outro_file <- file.path(path, "outro.txt")
    if (file.exists(intro_file)) {
        file.copy(intro_file, file.path(www_dir, "intro.txt"), overwrite = TRUE)
    }
    if (file.exists(outro_file)) {
        file.copy(outro_file, file.path(www_dir, "outro.txt"), overwrite = TRUE)
    }

    # Copy custom.R if it exists
    custom_file <- file.path(path, "custom.R")
    if (file.exists(custom_file)) {
        file.copy(custom_file, file.path(www_dir, "custom.R"), overwrite = TRUE)
    }

    cli::cli_alert_success("Design generated")
    cli::cli_alert_success("Experiment prepared successfully")
    cli::cli_bullets(c(
        "i" = "Run {.code run_experiment(\"{path}\")} to test locally",
        "i" = "Run {.code deploy_experiment(\"{path}\")} to deploy to shinyapps.io"
    ))

    invisible(design)
}
