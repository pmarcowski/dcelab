#' @keywords internal
"_PACKAGE"

#' dcelab: Create and Deploy Discrete Choice Experiments
#'
#' Framework for creating, configuring, and deploying Discrete Choice
#' Experiments (DCE). Provides a streamlined workflow for scaffolding
#' experiment directories, generating D-efficient designs, and deploying
#' to shinyapps.io.
#'
#' @section Workflow:
#' The typical workflow consists of four steps:
#' 1. `create_experiment()` - Scaffold a new experiment directory
#' 2. Edit configuration files (config.yaml, attributes.csv)
#' 3. `prepare_experiment()` - Validate and generate design
#' 4. `run_experiment()` or `deploy_experiment()` - Test or deploy
#'
#' @section Main functions:
#' * [create_experiment()] - Create new experiment from template
#' * [prepare_experiment()] - Prepare experiment for deployment
#' * [run_experiment()] - Run experiment locally
#' * [deploy_experiment()] - Deploy to shinyapps.io
#'
#' @name dcelab-package
#' @aliases dcelab
NULL

## usethis namespace: start
#' @importFrom stats setNames
#' @importFrom utils read.csv write.table browseURL
#' @importFrom shiny runApp
#' @import shinyjs
## usethis namespace: end
NULL
