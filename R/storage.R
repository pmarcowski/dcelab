#' Save Experiment Data
#'
#' Formats and saves experiment response data to configured storage providers.
#'
#' @param data List. Experiment data containing survey responses, design,
#'   reaction times, and optional defaults.
#' @param config List. Experiment configuration with storage settings.
#'
#' @return The formatted data frame (invisibly if saved to storage).
#'
#' @details
#' The data list should contain:
#' * `survey` - Matrix of presented choice sets
#' * `responses` - Vector of participant responses
#' * `reaction_times` - Vector of response times in milliseconds
#' * `defaults` (optional) - Vector of default selections if used
#'
#' Data is formatted as a data frame with columns for set number, attribute,
#' alternative values, response, and reaction time. Supports multiple storage
#' providers: local file storage and AWS S3.
#'
#' @seealso [save_to_local()], [save_to_s3()]
#'
#' @export
save_experiment_data <- function(data, config) {
    # Format data for storage
    n_attributes <- length(unique(rownames(data$survey)))

    formatted_data <- data.frame(
        set = rep(seq_along(data$responses), each = n_attributes),
        attribute = rownames(data$survey),
        as.data.frame(
            data$survey,
            stringsAsFactors = FALSE,
            check.names = FALSE
        ),
        response = rep(data$responses, each = n_attributes),
        reaction_time = rep(data$reaction_times, each = n_attributes),
        row.names = NULL
    )

    # Add defaults column if present
    if (!is.null(data$defaults)) {
        formatted_data$default <- rep(data$defaults, each = n_attributes)
    }

    # Add session ID (based on timestamp for uniqueness in merged datasets)
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    formatted_data$session_id <- paste0(config$exp_id, "_", timestamp)

    # Sort data by set number
    formatted_data <- formatted_data[order(formatted_data$set), ]

    # Reorder columns to put session_id first
    formatted_data <- formatted_data[, c(
        "session_id",
        setdiff(names(formatted_data), "session_id")
    )]

    # Generate filename with formatted timestamp and save if storage is configured
    if (!is.null(config$storage)) {
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        filename <- sprintf("%s_%s.txt", config$exp_id, timestamp)

        # Process configured storage providers
        storage_providers <- names(config$storage)
        for (provider in storage_providers) {
            if (provider == "local") {
                tryCatch(
                    {
                        save_to_local(
                            formatted_data,
                            filename,
                            config$storage$local,
                            config$exp_id
                        )
                    },
                    error = function(e) {
                        warning(sprintf(
                            "Failed to save data locally: %s",
                            e$message
                        ))
                    }
                )
            } else if (provider == "s3") {
                tryCatch(
                    {
                        save_to_s3(
                            formatted_data,
                            filename,
                            config$storage$s3,
                            config$exp_id
                        )
                    },
                    error = function(e) {
                        warning(sprintf(
                            "Failed to save data to S3: %s",
                            e$message
                        ))
                    }
                )
            } else if (provider == "gsheets") {
                tryCatch(
                    {
                        save_to_gsheets(
                            formatted_data,
                            config$storage$gsheets,
                            config$exp_id
                        )
                    },
                    error = function(e) {
                        warning(sprintf(
                            "Failed to save data to Google Sheets: %s",
                            e$message
                        ))
                    }
                )
            } else if (provider == "dropbox") {
                tryCatch(
                    {
                        save_to_dropbox(
                            formatted_data,
                            filename,
                            config$storage$dropbox,
                            config$exp_id
                        )
                    },
                    error = function(e) {
                        warning(sprintf(
                            "Failed to save data to Dropbox: %s",
                            e$message
                        ))
                    }
                )
            } else if (provider == "gcs") {
                tryCatch(
                    {
                        save_to_gcs(
                            formatted_data,
                            filename,
                            config$storage$gcs,
                            config$exp_id
                        )
                    },
                    error = function(e) {
                        warning(sprintf(
                            "Failed to save data to Google Cloud Storage: %s",
                            e$message
                        ))
                    }
                )
            }
        }
    }

    # Return the formatted data
    formatted_data
}

#' Save Data to S3
#'
#' Uploads experiment data to an AWS S3 bucket.
#'
#' @param data Data frame. Formatted experiment data.
#' @param filename Character. Filename for the uploaded object.
#' @param s3_config List. S3 configuration with bucket, prefix, region,
#'   access_key, and secret_key.
#' @param exp_id Character. Experiment identifier for organizing data.
#'
#' @return Invisibly returns `NULL`.
#'
#' @details
#' Data is saved as a tab-separated text file to the path:
#' `{bucket}/{prefix}/{exp_id}/{filename}`
#'
#' AWS credentials are set temporarily using [withr::with_envvar()].
#'
#' @export
save_to_s3 <- function(data, filename, s3_config, exp_id) {
    # Create temporary file
    temp_file <- tempfile(fileext = ".txt")
    write.table(
        data,
        temp_file,
        row.names = FALSE,
        quote = FALSE,
        sep = "\t",
        col.names = TRUE,
        fileEncoding = "UTF-8"
    )

    # Construct S3 object key with prefix
    object_key <- file.path(
        s3_config$prefix,
        exp_id,
        filename
    )

    # Set AWS credentials
    withr::with_envvar(
        new = c(
            "AWS_ACCESS_KEY_ID" = s3_config$access_key,
            "AWS_SECRET_ACCESS_KEY" = s3_config$secret_key,
            "AWS_DEFAULT_REGION" = s3_config$region
        ),
        {
            aws.s3::put_object(
                file = temp_file,
                object = object_key,
                bucket = s3_config$bucket
            )
        }
    )

    # Clean up
    unlink(temp_file)

    invisible(NULL)
}

#' Save Data Locally
#'
#' Saves experiment data to a local directory.
#'
#' @param data Data frame. Formatted experiment data.
#' @param filename Character. Filename for the saved file.
#' @param local_config List. Local storage configuration with path.
#' @param exp_id Character. Experiment identifier for organizing data.
#'
#' @return Invisibly returns the full path to the saved file.
#'
#' @details
#' Data is saved as a tab-separated text file to the path:
#' `{config_path}/{exp_id}/{filename}`
#'
#' The directory is created if it doesn't exist.
#'
#' @export
save_to_local <- function(data, filename, local_config, exp_id) {
    # Construct output directory
    output_dir <- file.path(local_config$path, exp_id)

    # Create directory if it doesn't exist
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }

    # Construct full file path
    file_path <- file.path(output_dir, filename)

    # Write data
    write.table(
        data,
        file_path,
        row.names = FALSE,
        quote = FALSE,
        sep = "\t",
        col.names = TRUE,
        fileEncoding = "UTF-8"
    )

    invisible(file_path)
}

#' Save Data to Google Sheets
#'
#' Appends experiment data to a Google Sheet.
#'
#' @param data Data frame. Formatted experiment data.
#' @param gsheets_config List. Google Sheets configuration with sheet_id
#'   and optionally sheet_name.
#' @param exp_id Character. Experiment identifier (used as sheet name if not
#'   specified).
#'
#' @return Invisibly returns `NULL`.
#'
#' @details
#' Requires the `googlesheets4` package. Authentication should be set up
#' beforehand using [googlesheets4::gs4_auth()].
#'
#' Data is appended to the specified sheet, creating a new sheet tab
#' named after the experiment ID if it doesn't exist.
#'
#' @export
save_to_gsheets <- function(data, gsheets_config, exp_id) {
    if (!requireNamespace("googlesheets4", quietly = TRUE)) {
        stop("Package 'googlesheets4' required for Google Sheets storage")
    }

    sheet_name <- gsheets_config$sheet_name %||% exp_id

    # Append data to sheet
    googlesheets4::sheet_append(
        ss = gsheets_config$sheet_id,
        data = data,
        sheet = sheet_name
    )

    invisible(NULL)
}

#' Save Data to Dropbox
#'
#' Uploads experiment data to Dropbox.
#'
#' @param data Data frame. Formatted experiment data.
#' @param filename Character. Filename for the uploaded file.
#' @param dropbox_config List. Dropbox configuration with path and
#'   optionally token_path.
#' @param exp_id Character. Experiment identifier for organizing data.
#'
#' @return Invisibly returns `NULL`.
#'
#' @details
#' Requires the `rdrop2` package. Authentication should be set up
#' beforehand using `rdrop2::drop_auth()`.
#'
#' Data is saved as a tab-separated text file to the path:
#' `{dropbox_path}/{exp_id}/{filename}`
#'
#' @export
save_to_dropbox <- function(data, filename, dropbox_config, exp_id) {
    if (!requireNamespace("rdrop2", quietly = TRUE)) {
        stop("Package 'rdrop2' required for Dropbox storage")
    }

    # Create temporary file
    temp_file <- tempfile(fileext = ".txt")
    write.table(
        data,
        temp_file,
        row.names = FALSE,
        quote = FALSE,
        sep = "\t",
        col.names = TRUE,
        fileEncoding = "UTF-8"
    )

    # Construct Dropbox path
    dropbox_path <- file.path(
        dropbox_config$path,
        exp_id,
        filename
    )

    # Upload file
    rdrop2::drop_upload(
        file = temp_file,
        path = dropbox_path,
        mode = "add"
    )

    # Clean up
    unlink(temp_file)

    invisible(NULL)
}

#' Save Data to Google Cloud Storage
#'
#' Uploads experiment data to a Google Cloud Storage bucket.
#'
#' @param data Data frame. Formatted experiment data.
#' @param filename Character. Filename for the uploaded object.
#' @param gcs_config List. GCS configuration with bucket, prefix, and
#'   optionally credentials_file.
#' @param exp_id Character. Experiment identifier for organizing data.
#'
#' @return Invisibly returns `NULL`.
#'
#' @details
#' Requires the `googleCloudStorageR` package. Authentication should be set up
#' beforehand using [googleCloudStorageR::gcs_auth()].
#'
#' Data is saved as a tab-separated text file to the path:
#' `{bucket}/{prefix}/{exp_id}/{filename}`
#'
#' @export
save_to_gcs <- function(data, filename, gcs_config, exp_id) {
    if (!requireNamespace("googleCloudStorageR", quietly = TRUE)) {
        stop("Package 'googleCloudStorageR' required for GCS storage")
    }

    # Create temporary file
    temp_file <- tempfile(fileext = ".txt")
    write.table(
        data,
        temp_file,
        row.names = FALSE,
        quote = FALSE,
        sep = "\t",
        col.names = TRUE,
        fileEncoding = "UTF-8"
    )

    # Construct GCS object name
    object_name <- file.path(
        gcs_config$prefix,
        exp_id,
        filename
    )

    # Upload file
    googleCloudStorageR::gcs_upload(
        file = temp_file,
        bucket = gcs_config$bucket,
        name = object_name
    )

    # Clean up
    unlink(temp_file)

    invisible(NULL)
}
