#' Run Demo Experiment
#'
#' Launches a demonstration DCE experiment to showcase the package workflow.
#' Creates a temporary experiment, prepares it, and runs it in the browser.
#'
#' @param browse Logical. Open the experiment in a browser window.
#'   Defaults to `TRUE`.
#'
#' @return Invisibly returns the path to the demo experiment directory.
#'
#' @details
#' This function demonstrates the complete dcelab workflow:
#' 1. Creates a temporary experiment directory from templates
#' 2. Generates a D-efficient experimental design
#' 3. Launches the Shiny app in your browser
#'
#' Use this to quickly see what a DCE experiment looks like before
#' creating your own.
#'
#' @examples
#' \dontrun{
#' # Launch a demo experiment
#' run_demo()
#' }
#'
#' @export
run_demo <- function(browse = TRUE) {
    if (!requireNamespace("shinyjs", quietly = TRUE)) {
        stop("Package 'shinyjs' is required for the demo. Please install it.")
    }

    # Create temporary experiment directory
    demo_path <- file.path(
        tempdir(),
        paste0("dcelab_demo_", format(Sys.time(), "%H%M%S"))
    )

    cli::cli_alert_info("Creating demo experiment...")

    # Copy templates to demo directory
    template_dir <- system.file("templates", package = "dcelab")
    if (template_dir == "") {
        cli::cli_abort(
            "Template directory not found. Is dcelab installed correctly?"
        )
    }

    dir.create(demo_path, recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(demo_path, "www"), showWarnings = FALSE)

    # Copy all template files
    template_files <- list.files(template_dir, full.names = TRUE)
    file.copy(template_files, demo_path, recursive = TRUE)

    # Prepare the experiment (validate + generate design)
    cli::cli_alert_info("Generating D-efficient design...")
    prepare_experiment(demo_path)

    cli::cli_alert_success("Demo ready!")
    cli::cli_bullets(c(
        "i" = "This demonstrates the default DCE experiment template.",
        "i" = "To create your own: {.code create_experiment(\"my_study\")}"
    ))

    # Run the experiment
    if (browse) {
        run_experiment(demo_path)
    }

    invisible(demo_path)
}
