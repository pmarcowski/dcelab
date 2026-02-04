#' Discrete Choice Experiment Shiny Application
#'
#' @description
#' Main Shiny application for running DCE experiments.
#' This file is used when deploying the experiment.

library(shiny)
library(shinyjs)
library(dcelab)

# Load app utility functions
source("utils.R")

# Load configuration and resources
resources_path <- "www"
config <- yaml::read_yaml(file.path(resources_path, "config.yaml"))
atts <- read.csv(
    file.path(resources_path, "attributes.csv"),
    na.strings = c(""),
    check.names = FALSE,
    encoding = "UTF-8"
)
design <- readRDS(file.path(resources_path, "design.rds"))

# Load custom functions if configured
custom_funcs <- NULL
if (!is.null(config$custom_attributes)) {
    custom_funcs <- load_custom_functions(config, resources_path)
}

# Process attributes
atts_names <- colnames(atts)
atts_labs <- lapply(atts, function(x) x[!is.na(x)])
atts_lvls <- as.numeric(lengths(atts_labs))
names(atts_lvls) <- atts_names
atts_coding <- rep("E", length(atts))
atts_num <- length(atts_labs)

# Initialize design parameters
bs <- seq(
    1,
    (nrow(design$design) - config$design$n_alts + 1),
    config$design$n_alts
)
es <- c((bs - 1), nrow(design$design))[-1]

# Set app name (default is exp_id)
app_name <- config$deployment$appname %||% config$exp_id

# Define UI
ui <- fluidPage(
    title = app_name,
    useShinyjs(),
    tags$head(
        tags$style(HTML(
            "
      .choice-table td {
        vertical-align: middle !important;
        text-align: center !important;
      }
      .choice-table th:first-child {
        text-align: left !important;
      }
      .choice-table td:first-child {
        text-align: left !important;
      }
    "
        ))
    ),
    tags$script(
        "
    var startTime;
    $(document).ready(function() {
      Shiny.addCustomMessageHandler('startTiming', function(message) {
        startTime = performance.now();
      });
      $(document).on('click', '.radio-options input[type=\"radio\"]', function() {
        var endTime = performance.now();
        var reactionTime = endTime - startTime;
        Shiny.setInputValue('reaction_time', reactionTime);
        Shiny.setInputValue('option_clicked', true, {priority: 'event'});
      });
    });
  "
    ),
    column(
        12,
        align = "center",
        style = "padding-top:200px; padding-bottom:5px",
        textOutput("set_nr")
    ),
    column(12, align = "center", tableOutput("choice_set")),
    column(12, align = "center", uiOutput("buttons")),
    column(
        12,
        align = "center",
        style = "padding-bottom:10px",
        htmlOutput("intro")
    ),
    column(
        12,
        align = "center",
        style = "padding-bottom:10px",
        htmlOutput("end")
    ),
    column(12, align = "center", actionButton("OK", "OK"))
)

# Define server
server <- function(input, output, session) {
    # Initialize reactive values
    rv <- reactiveValues(
        set_num = 0,
        full_design = design$design,
        choice_set = NA,
        choice_sets = matrix(
            NA,
            nrow = (config$design$n_sets * config$design$n_alts),
            ncol = atts_num
        ),
        custom_funcs = custom_funcs,
        survey_data = list(),
        atts_labs = atts_labs,
        option_clicked = FALSE
    )

    # Display intro text
    output$intro <- renderText(
        readLines(file.path(resources_path, "intro.txt"), encoding = "UTF-8")
    )

    # Handle trial progression
    observeEvent(input$OK, {
        rv$option_clicked <- FALSE
        if (isTRUE(config$ui$explicit_choice)) {
            disable("OK")
        } else {
            enable("OK")
        }

        rv$set_num <- rv$set_num + 1

        if (rv$set_num == 1) {
            output$intro <- renderText(NULL)
        }

        if (rv$set_num <= config$design$n_total) {
            output$set_nr <- renderText(
                paste("Choice:", rv$set_num, "/", config$design$n_total)
            )
        } else {
            output$set_nr <- renderText(NULL)
        }

        if (rv$set_num <= config$design$n_total) {
            current_set <- select_choice_set(
                rv = rv,
                design = design$design,
                bs = bs,
                es = es,
                atts = atts,
                atts_lvls = atts_lvls,
                atts_coding = atts_coding,
                atts_num = atts_num,
                config = config
            )

            output$choice_set <- renderTable(
                current_set,
                rownames = TRUE,
                sanitize.text.function = function(x) x,
                class = "choice-table",
                align = paste0(
                    'l',
                    paste0(rep('c', ncol(current_set)), collapse = '')
                )
            )

            if (rv$set_num == 1) {
                rv$choice_sets <- current_set
            } else {
                rv$choice_sets <- rbind(rv$choice_sets, current_set)
            }
        } else {
            output$choice_set <- renderTable(NULL)
            if (rv$set_num == (config$design$n_total + 1)) {
                output$end <- renderText(
                    readLines(
                        file.path(resources_path, "outro.txt"),
                        encoding = "UTF-8"
                    )
                )
                enable("OK")
            }
        }

        if (rv$set_num > 1 && rv$set_num <= (config$design$n_total + 1)) {
            rv$survey_data$responses <- c(
                rv$survey_data$responses,
                input$survey
            )
            rv$survey_data$design <- rv$full_design
            rv$survey_data$survey <- rv$choice_sets

            if (!is.null(config$ui$default_option)) {
                rv$survey_data$defaults <- c(
                    rv$survey_data$defaults,
                    rv$default_option
                )
            }
            rv$default_option <- NULL
        }

        if (rv$set_num == (config$design$n_total + 1)) {
            save_experiment_data(
                data = rv$survey_data,
                config = config
            )
        }

        if (rv$set_num > (config$design$n_total + 1)) {
            if (!is.null(config$completion$url)) {
                shinyjs::runjs(sprintf(
                    "window.location.href='%s'",
                    config$completion$url
                ))
            }
            stopApp()
        }
    })

    # Display choice buttons
    output$buttons <- renderUI({
        if (rv$set_num > 0 && rv$set_num <= config$design$n_total) {
            alternatives <- config$design$alternatives
            if (
                !is.null(config$ui$no_choice) && !isFALSE(config$ui$no_choice)
            ) {
                insert_position <- length(alternatives) %/% 2
                alternatives <- append(
                    alternatives,
                    config$ui$no_choice,
                    after = insert_position
                )
            }

            session$sendCustomMessage("startTiming", list())

            tagList(
                tags$script(
                    "
          $(document).ready(function() {
            $(document).on('click', '.radio-options input[type=\"radio\"]', function() {
              Shiny.setInputValue('option_clicked', true, {priority: 'event'});
            });
          });
        "
                ),
                div(
                    class = "radio-options",
                    radioButtons(
                        "survey",
                        config$ui$buttons_text,
                        alternatives,
                        inline = TRUE,
                        selected = character(0)
                    )
                )
            )
        }
    })

    # Track option clicks
    observeEvent(input$option_clicked, {
        if (input$option_clicked) {
            rv$option_clicked <- TRUE
            if (isTRUE(config$ui$explicit_choice)) {
                enable("OK")
            }
        }
    })

    # Track reaction time
    observeEvent(input$reaction_time, {
        if (!is.null(input$reaction_time)) {
            rv$survey_data$reaction_times <- c(
                rv$survey_data$reaction_times,
                input$reaction_time
            )
        }
    })

    # Handle default option selection
    observe({
        if (
            rv$set_num > 0 &&
                rv$set_num <= config$design$n_total &&
                !rv$option_clicked &&
                !is.null(config$ui$default_option)
        ) {
            alternatives <- config$design$alternatives
            if (
                !is.null(config$ui$no_choice) && !isFALSE(config$ui$no_choice)
            ) {
                insert_position <- length(alternatives) %/% 2
                alternatives <- append(
                    alternatives,
                    config$ui$no_choice,
                    after = insert_position
                )
            }

            selected <- if (identical(config$ui$default_option, "random")) {
                sample(alternatives, 1)
            } else {
                config$ui$default_option
            }

            rv$default_option <- selected

            updateRadioButtons(
                session,
                "survey",
                selected = selected
            )
        }
    })
}

# Null coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# Run application
shinyApp(ui = ui, server = server)
