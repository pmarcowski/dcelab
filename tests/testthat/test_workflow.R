#' Workflow Function Tests
#'
#' @description
#' Tests for create, prepare, run, deploy workflow functions

test_that("create_experiment validates path argument", {
    expect_error(create_experiment(123), "Assertion")
    expect_error(create_experiment(""), "Assertion")
})

test_that("create_experiment fails if directory exists", {
    existing_dir <- tempfile("existing")
    dir.create(existing_dir)
    on.exit(unlink(existing_dir, recursive = TRUE))

    expect_error(create_experiment(existing_dir), "Directory already exists")
})

test_that("create_experiment creates directory with all template files", {
    test_dir <- tempfile("test_exp")
    on.exit(unlink(test_dir, recursive = TRUE))

    create_experiment(test_dir, open = FALSE)

    expect_true(dir.exists(test_dir))
    expect_true(file.exists(file.path(test_dir, "app.R")))
    expect_true(file.exists(file.path(test_dir, "utils.R")))
    expect_true(file.exists(file.path(test_dir, "config.yaml")))
    expect_true(file.exists(file.path(test_dir, "attributes.csv")))
    expect_true(file.exists(file.path(test_dir, "intro.txt")))
    expect_true(file.exists(file.path(test_dir, "outro.txt")))
    expect_true(dir.exists(file.path(test_dir, "www")))
})

test_that("prepare_experiment validates path exists", {
    expect_error(prepare_experiment("/nonexistent/path"), "not exist")
})

test_that("prepare_experiment requires config.yaml", {
    test_dir <- tempfile("test_exp")
    dir.create(test_dir)
    on.exit(unlink(test_dir, recursive = TRUE))

    expect_error(prepare_experiment(test_dir), "Configuration file not found")
})

test_that("run_experiment validates path exists", {
    expect_error(run_experiment("/nonexistent/path"), "not exist")
})

test_that("run_experiment requires prepared experiment", {
    test_dir <- tempfile("test_exp")
    dir.create(test_dir)
    file.create(file.path(test_dir, "app.R"))
    on.exit(unlink(test_dir, recursive = TRUE))

    expect_error(run_experiment(test_dir), "Experiment not prepared")
})

test_that("deploy_experiment validates path exists", {
    expect_error(deploy_experiment("/nonexistent/path"), "not exist")
})

test_that("deploy_experiment requires prepared experiment", {
    test_dir <- tempfile("test_exp")
    dir.create(test_dir)
    on.exit(unlink(test_dir, recursive = TRUE))

    expect_error(deploy_experiment(test_dir), "Experiment not prepared")
})
