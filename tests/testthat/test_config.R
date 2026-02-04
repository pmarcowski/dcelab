#' Configuration Validation Tests
#'
#' @description
#' Tests for experiment configuration validation functions

test_that("validate_config accepts valid configuration", {
  valid_config <- list(
    exp_id = "test_experiment",
    design = list(
      n_sets = 30,
      n_total = 30,
      n_alts = 2,
      alternatives = c("Option A", "Option B")
    ),
    ui = list(
      buttons_text = "Which option do you prefer?",
      shuffle_attributes = FALSE,
      default_option = "random",
      no_choice = "Don't know",
      explicit_choice = TRUE
    ),
    storage = list(
      s3 = list(
        bucket = "test-bucket",
        prefix = "data/raw",
        region = "test-region",
        access_key = "test-key",
        secret_key = "test-secret"
      )
    )
  )
  expect_silent(validate_config(valid_config))
})

test_that("validate_config checks required fields", {
  valid_config <- list(
    exp_id = "test",
    design = list(
      n_sets = 30,
      n_total = 30,
      n_alts = 2,
      alternatives = c("A", "B")
    ),
    ui = list(
      buttons_text = "?",
      shuffle_attributes = FALSE,
      default_option = NULL,
      no_choice = FALSE,
      explicit_choice = TRUE
    )
  )

  invalid_config <- valid_config
  invalid_config$exp_id <- NULL
  expect_error(
    validate_config(invalid_config),
    "Missing required field: exp_id"
  )
})

test_that("validate_config validates design parameters", {
  valid_config <- list(
    exp_id = "test",
    design = list(
      n_sets = 30,
      n_total = 30,
      n_alts = 2,
      alternatives = c("A", "B")
    ),
    ui = list(
      buttons_text = "?",
      shuffle_attributes = FALSE,
      default_option = NULL,
      no_choice = FALSE,
      explicit_choice = TRUE
    )
  )

  # Check for negative number of sets
  invalid_config <- valid_config
  invalid_config$design$n_sets <- -1
  expect_error(
    validate_config(invalid_config),
    "design.n_sets must be a positive number"
  )

  # Check total sets don't exceed available sets
  invalid_config <- valid_config
  invalid_config$design$n_total <- 31
  expect_error(
    validate_config(invalid_config),
    "design.n_total cannot be greater than design.n_sets"
  )
})

test_that("validate_config validates UI configuration", {
  valid_config <- list(
    exp_id = "test",
    design = list(
      n_sets = 30,
      n_total = 30,
      n_alts = 2,
      alternatives = c("A", "B")
    ),
    ui = list(
      buttons_text = "?",
      shuffle_attributes = FALSE,
      default_option = NULL,
      no_choice = FALSE,
      explicit_choice = TRUE
    )
  )

  # Invalid shuffle_attributes
  invalid_config <- valid_config
  invalid_config$ui$shuffle_attributes <- "invalid"
  expect_error(
    validate_config(invalid_config),
    "ui.shuffle_attributes must be FALSE, 'trial', or 'participant'"
  )

  # Valid shuffle modes (expect message about missing storage)
  valid_config$ui$shuffle_attributes <- "trial"
  expect_message(validate_config(valid_config), "No storage configuration")

  valid_config$ui$shuffle_attributes <- "participant"
  expect_message(validate_config(valid_config), "No storage configuration")

  # Invalid default_option
  invalid_config <- valid_config
  invalid_config$ui$shuffle_attributes <- FALSE
  invalid_config$ui$default_option <- "Option C"
  expect_error(
    validate_config(invalid_config),
    "ui.default_option must be null, 'random', or one of:"
  )
})
