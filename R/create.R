#' Create New DCE Experiment
#'
#' Scaffolds a new experiment directory with all necessary template files.
#' Creates a complete, ready-to-configure experiment structure.
#'
#' @param path Character. Path where the experiment directory will be created.
#' @param open Logical. Open the directory in file explorer after creation.
#'   Defaults to `TRUE` in interactive sessions.
#'
#' @return Invisibly returns the path to the created experiment directory.
#'
#' @details
#' Creates an experiment directory with the following structure:
#' ```
#' path/
#' ├── app.R           # Shiny application
#' ├── utils.R         # App utility functions
#' ├── config.yaml     # Experiment configuration
#' ├── attributes.csv  # Attribute definitions
#' ├── intro.txt       # Introduction text
#' ├── outro.txt       # Closing text
#' └── www/            # Static assets directory
#' ```
#'
#' After creation, edit `config.yaml` and `attributes.csv` to define your
#' experiment, then run [prepare_experiment()] to generate the design.
#'
#' @seealso [prepare_experiment()], [run_experiment()], [deploy_experiment()]
#'
#' @examples
#' \dontrun{
#' # Create a new experiment
#' create_experiment("my_study")
#'
#' # Edit config.yaml and attributes.csv, then:
#' prepare_experiment("my_study")
#' run_experiment("my_study")
#' }
#'
#' @export
create_experiment <- function(path, open = interactive()) {
    # Validate path
    checkmate::assert_character(path, len = 1, min.chars = 1)

    # Check if directory already exists

    if (dir.exists(path)) {
        cli::cli_abort("Directory already exists: {.path {path}}")
    }

    # Get template directory

    template_dir <- system.file("templates", package = "dcelab")
    if (template_dir == "") {
        cli::cli_abort(
            "Template directory not found. Is dcelab installed correctly?"
        )
    }

    # Create experiment directory
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(path, "www"), showWarnings = FALSE)

    # Copy template files
    template_files <- list.files(template_dir, full.names = TRUE)
    file.copy(template_files, path, recursive = TRUE)

    cli::cli_alert_success("Created experiment at {.path {path}}")
    cli::cli_bullets(c(
        "i" = "Next steps:",
        " " = "1. Edit {.file config.yaml} to configure your experiment",
        " " = "2. Edit {.file attributes.csv} to define attributes and levels",
        " " = "3. Run {.code prepare_experiment(\"{path}\")} to generate design",
        " " = "4. Run {.code run_experiment(\"{path}\")} to test locally"
    ))

    # Open directory if requested

    if (open && interactive()) {
        utils::browseURL(path)
    }

    invisible(path)
}
