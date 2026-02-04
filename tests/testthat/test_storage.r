#' Storage Configuration and Data Saving Tests
#'
#' @description
#' Tests for data storage functionality

test_that("save_experiment_data handles missing storage configuration", {
  test_data <- list(
    survey = matrix(
      c("level1", "level2", "level1", "level2"),
      nrow = 2,
      dimnames = list(c("attr1", "attr2"), c("Option A", "Option B"))
    ),
    responses = c("Option A", "Option B"),
    reaction_times = c(1000, 2000)
  )

  test_config <- list(exp_id = "test")

  # Should run without error when storage config is missing
  expect_silent(save_experiment_data(test_data, test_config))
})

test_that("save_experiment_data formats data correctly", {
  test_data <- list(
    survey = matrix(
      c("level1", "level2", "level1", "level2"),
      nrow = 2,
      dimnames = list(c("attr1", "attr2"), c("Option A", "Option B"))
    ),
    responses = c("Option A", "Option B"),
    reaction_times = c(1000, 2000)
  )

  test_config <- list(exp_id = "test")

  formatted <- save_experiment_data(test_data, test_config)

  expect_equal(nrow(formatted), 4) # 2 attributes * 2 responses
  expect_true("set" %in% colnames(formatted))
  expect_true("attribute" %in% colnames(formatted))
  expect_true("response" %in% colnames(formatted))
  expect_true("reaction_time" %in% colnames(formatted))
})

test_that("save_experiment_data includes defaults when present", {
  test_data <- list(
    survey = matrix(
      c("level1", "level2"),
      nrow = 1,
      dimnames = list(c("attr1"), c("Option A", "Option B"))
    ),
    responses = c("Option A"),
    reaction_times = c(1000),
    defaults = c("Option B")
  )

  test_config <- list(exp_id = "test")

  formatted <- save_experiment_data(test_data, test_config)

  expect_true("default" %in% colnames(formatted))
  expect_equal(formatted$default[1], "Option B")
})

test_that("save_to_s3 constructs correct object key", {
  skip_if_not_installed("mockery")

  test_data <- data.frame(set = 1, response = "A")
  s3_config <- list(
    bucket = "test-bucket",
    prefix = "data/raw",
    region = "us-east-1",
    access_key = "key",
    secret_key = "secret"
  )

  # Mock aws.s3::put_object
  mock_put <- mockery::mock(TRUE)
  mockery::stub(save_to_s3, "aws.s3::put_object", mock_put)

  save_to_s3(test_data, "test.txt", s3_config, "exp1")

  mockery::expect_called(mock_put, 1)
  args <- mockery::mock_args(mock_put)[[1]]
  expect_equal(args$object, "data/raw/exp1/test.txt")
  expect_equal(args$bucket, "test-bucket")
})
