#' Deploy Experiment to shinyapps.io
#'
#' Deploys the experiment Shiny application to shinyapps.io for online
#' data collection.
#'
#' @param path Character. Path to the experiment directory.
#' @param appName Character. Application name on shinyapps.io. Defaults to
#'   the experiment ID from config.yaml.
#' @param account Character. shinyapps.io account name. If NULL, uses the
#'   account from config.yaml or the default configured account.
#' @param ... Additional arguments passed to [rsconnect::deployApp()].
#'
#' @return Invisibly returns the deployment result from rsconnect.
#'
#' @details
#' Before deploying, ensure you have:
#' 1. Called [prepare_experiment()] to generate the design
#' 2. Configured your shinyapps.io account with [rsconnect::setAccountInfo()]
#'    or specified credentials in `config.yaml`
#'
#' The deployment includes all files in the experiment directory.
#'
#' @seealso [create_experiment()], [prepare_experiment()], [run_experiment()]
#'
#' @examples
#' \dontrun{
#' deploy_experiment("my_study", account = "myaccount")
#' }
#'
#' @export
deploy_experiment <- function(path, appName = NULL, account = NULL, ...) {
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

    # Load config for defaults
    config_file <- file.path(path, "config.yaml")
    config <- yaml::read_yaml(config_file)

    # Determine app name
    if (is.null(appName)) {
        appName <- config$deployment$appname %||% config$exp_id
    }

    # Set up account from config if provided
    if (!is.null(config$deployment$account$token)) {
        rsconnect::setAccountInfo(
            name = config$deployment$account$name,
            token = config$deployment$account$token,
            secret = config$deployment$account$secret
        )
        if (is.null(account)) {
            account <- config$deployment$account$name
        }
    }

    cli::cli_alert_info("Deploying {.val {appName}} to shinyapps.io...")

    # Deploy
    result <- rsconnect::deployApp(
        appDir = path,
        appName = appName,
        account = account,
        launch.browser = FALSE,
        ...
    )

    cli::cli_alert_success("Deployment complete")

    if (!is.null(account)) {
        cli::cli_bullets(c(
            "i" = "App URL: {.url https://{account}.shinyapps.io/{appName}}"
        ))
    }

    invisible(result)
}
