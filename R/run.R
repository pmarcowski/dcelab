#' Run Experiment Locally
#'
#' Launches the experiment Shiny application for local testing.
#'
#' @param path Character. Path to the experiment directory.
#' @param ... Additional arguments passed to [shiny::runApp()].
#'
#' @return This function does not return; it runs the Shiny app until stopped.
#'
#' @details
#' Before running, ensure you have called [prepare_experiment()] to generate
#' the experimental design. The function checks for the presence of
#' `www/design.rds` and will error if it's not found.
#'
#' @seealso [create_experiment()], [prepare_experiment()], [deploy_experiment()]
#'
#' @examples
#' \dontrun{
#' run_experiment("my_study")
#' }
#'
#' @export
run_experiment <- function(path, ...) {
    # Validate path
    checkmate::assert_directory_exists(path)

    # Check if experiment is prepared
    design_file <- file.path(path, "www", "design.rds")
    if (!file.exists(design_file)) {
        cli::cli_abort(c(
            "Experiment not prepared",
            "i" = "Run {.code prepare_experiment(\"{path}\")} first"
        ))
    }

    # Check app.R exists
    app_file <- file.path(path, "app.R")
    if (!file.exists(app_file)) {
        cli::cli_abort("Application file not found: {.file {app_file}}")
    }

    cli::cli_alert_info("Launching experiment from {.path {path}}")

    # Run the Shiny app
    shiny::runApp(path, ...)
}
