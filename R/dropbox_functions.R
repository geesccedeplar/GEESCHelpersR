#' Get a Dropbox Access Token
#'
#' Internal helper function to manage OAuth2 flow.
#' It checks for an existing REFRESH_TOKEN in the .env file.
#' If found, it uses it to get a new access token.
#' If not found, it initiates the manual "code" flow, gets a
#' refresh token, and appends it to the .env file.
#'
#' @param envpath The path to a local .env file
#' @return A short-lived access token.
#' @export
auth <- function(envpath = ".env") {
  # Load environment variables from .env file
  # This makes APP_KEY, APP_SECRET, etc. available via Sys.getenv()
  dotenv::load_dot_env(file = envpath)

  APP_KEY <- Sys.getenv("APP_KEY")
  APP_SECRET <- Sys.getenv("APP_SECRET")
  REFRESH_TOKEN <- Sys.getenv("REFRESH_TOKEN")

  # Check if essential vars are set
  if (APP_KEY == "" || APP_SECRET == "") {
    stop("Error: APP_KEY and APP_SECRET must be set in your .env file.", call. = FALSE)
  }

  # --- Case 1: Refresh Token is PRESENT ---
  # This is the normal flow after the first-time setup.
  if (REFRESH_TOKEN != "") {
    # Use the refresh token to get a new access token
    req <- httr2::request("https://api.dropbox.com/oauth2/token") |>
      httr2::req_auth_basic(user = APP_KEY, password = APP_SECRET) |>
      httr2::req_body_form(
        grant_type = "refresh_token",
        refresh_token = REFRESH_TOKEN
      )

    tryCatch(
      {
        response <- httr2::req_perform(req)
        token_data <- httr2::resp_body_json(response)
        return(token_data$access_token)
      },
      httr2_http_error = function(e) {
        stop("Error refreshing token: ", httr2::resp_body_string(e$resp), call. = FALSE)
      }
    )
  } else {
    # --- Case 2: Refresh Token is MISSING ---
    # This is the first-time setup flow.
    message("No REFRESH_TOKEN found. Starting first-time setup.")

    # 1. Generate the authorization URL
    auth_url <- paste0(
      "https://www.dropbox.com/oauth2/authorize?",
      "client_id=", APP_KEY,
      "&response_type=code",
      "&token_access_type=offline" # This is key to get a refresh token
    )

    message("\nPlease visit this URL in your browser to authorize this script:")
    message(auth_url)
    message("\nAfter authorizing, you will be given a code.")

    # 2. Get the code from the user
    auth_code <- readline(prompt = "Paste the authorization code here: ")

    # 3. Exchange the code for an access token AND a refresh token
    req <- httr2::request("https://api.dropbox.com/oauth2/token") |>
      httr2::req_auth_basic(user = APP_KEY, password = APP_SECRET) |>
      httr2::req_body_form(
        grant_type = "authorization_code",
        code = auth_code
      )

    tryCatch(
      {
        response <- httr2::req_perform(req)
        token_data <- httr2::resp_body_json(response)

        # 4. We got the new refresh token! Save it to the .env file.
        new_refresh_token <- token_data$refresh_token
        if (!is.null(new_refresh_token)) {
          message("Success! Saving new REFRESH_TOKEN to .env file...")
          # Append to the file
          cat(
            paste0("\nREFRESH_TOKEN=", new_refresh_token, "\n"),
            file = envpath,
            append = TRUE
          )
        } else {
          warning("Did not receive a refresh token. Make sure 'token_access_type=offline' is used.", call. = FALSE)
        }

        # 5. Return the new access token for the current session
        return(token_data$access_token)
      },
      httr2_http_error = function(e) {
        stop("Error exchanging code for token: ", httr2::resp_body_string(e$resp), call. = FALSE)
      }
    )
  }
}


#' Download a File from Dropbox
#'
#' Downloads a file from a specified Dropbox path to a local path.
#'
#' @param dbx_token The short-lived access token
#' @param dropbox_path The full path to the file in your Dropbox (e.g., "/Apps/MyApp/data.csv").
#' @param local_path The local file path to save the file to (e.g., "./my_data.csv").
#' @return NULL. The file is saved to disk.
#' @export
download_dropbox_file <- function(dbx_token, dropbox_path, local_path) {
  # The download API expects arguments in a JSON header
  api_arg <- list(path = dropbox_path)

  req <- httr2::request("https://content.dropboxapi.com/2/files/download") |>
    httr2::req_headers(
      "Authorization" = paste("Bearer", dbx_token),
      "Dropbox-API-Arg" = jsonlite::toJSON(api_arg, auto_unbox = TRUE)
    )

  tryCatch(
    {
      # httr2::req_perform with a 'path' argument saves the response body to that file
      httr2::req_perform(req, path = local_path)
      message(paste("File successfully downloaded from", dropbox_path, "to", local_path))
    },
    httr2_http_error = function(e) {
      # The error response body is in e$body (raw vector)
      # Dropbox download errors are often plain text
      error_summary <- "Unable to parse error response."
      try(
        {
          error_summary <- rawToChar(e$body)
        },
        silent = TRUE
      )
      stop("Dropbox API error: ", error_summary, call. = FALSE)
    }
  )
}

#' Download a File from Dropbox to memory
#'
#' Read file into memory without saving to disk
#'
#' @param dbx_token The short-lived access token
#' @param dropbox_path The full path to the file in your Dropbox (e.g., "/Apps/MyApp/data.csv").
#' @return A tempfile read from dropbox
#' @export
download_dropbox_memory <- function(dbx_token, dropbox_path) {
  # The download API expects arguments in a JSON header
  api_arg <- list(path = dropbox_path)

  req <- httr2::request("https://content.dropboxapi.com/2/files/download") |>
    httr2::req_headers(
      "Authorization" = paste("Bearer", dbx_token),
      "Dropbox-API-Arg" = jsonlite::toJSON(api_arg, auto_unbox = TRUE)
    )
  tmp_file <- tempfile(fileext = paste0(".", tools::file_ext(basename(dropbox_path))))
  tryCatch(
    {
      # httr2::req_perform with a 'path' argument saves the response body to that file
      httr2::req_perform(req, path = tmp_file)
      message(paste("File successfully read from", dropbox_path, "to", tmp_file))
      return(tmp_file)
    },
    httr2_http_error = function(e) {
      # The error response body is in e$body (raw vector)
      # Dropbox download errors are often plain text
      error_summary <- "Unable to parse error response."
      try(
        {
          error_summary <- rawToChar(e$body)
        },
        silent = TRUE
      )
      stop("Dropbox API error: ", error_summary, call. = FALSE)
    }
  )
}

#' Get Dropbox File Content Hash
#'
#' Retrieves the metadata for a file in Dropbox, including the 'content_hash'.
#'
#' @param dbx_token The short-lived access token
#' @param dropbox_path The full path to the file in your Dropbox (e.g., "/Apps/MyApp/data.csv").
#' @return A string containing the 'content_hash' of the file.
get_dropbox_hash <- function(dbx_token, dropbox_path) {
  # The metadata API expects a JSON body
  body <- list(
    path = dropbox_path,
    include_media_info = FALSE,
    include_deleted = FALSE,
    include_has_explicit_shared_members = FALSE
  )

  req <- httr2::request("https://api.dropboxapi.com/2/files/get_metadata") |>
    httr2::req_headers(
      "Authorization" = paste("Bearer", dbx_token),
      "Content-Type" = "application/json"
    ) |>
    httr2::req_body_json(body)

  tryCatch(
    {
      response <- httr2::req_perform(req)
      metadata <- httr2::resp_body_json(response)

      if (is.null(metadata$content_hash)) {
        warning("Could not find 'content_hash' in metadata response.", call. = FALSE)
        return(NULL)
      }

      return(metadata$content_hash)
    },
    httr2_http_error = function(e) {
      # This API endpoint (api.dropbox.com) returns JSON errors
      error_data <- httr2::resp_body_json(e$resp)
      error_summary <- error_data$error_summary
      if (is.null(error_summary) && !is.null(error_data$error$".tag")) {
        error_summary <- error_data$error$".tag"
      } else if (is.null(error_summary)) {
        error_summary <- "Unknown Dropbox API error."
      }
      stop("Dropbox API error: ", error_summary, call. = FALSE)
    }
  )
}

#' Compute the Dropbox Content Hash for a Local File
#'
#' Dropbox computes a content hash by:
#' 1. Splitting the file into 4MB chunks.
#' 2. Calculating the SHA256 hash of each chunk.
#' 3. Concatenating all chunk hashes.
#' 4. Calculating the SHA256 hash of the concatenated string.
#' Para arquivos muito grandes, talvez seja muito custoso calcular o hash sempre a depender do computador
#'
#' @param local_path Path to the local file.
#' @return A string (hex digest) representing the Dropbox content hash.
compute_local_hash <- function(local_path) {
  # Dropbox hash chunk size is 4MB
  DROPBOX_HASH_CHUNK_SIZE <- 4L * 1024L * 1024L

  # Open a connection to read the file in raw binary mode
  con <- file(local_path, "rb")

  # This will hold the concatenation of all chunk hashes
  all_block_hashes <- raw(0)

  tryCatch({
    while (TRUE) {
      # Read a chunk from the file
      chunk <- readBin(con, "raw", n = DROPBOX_HASH_CHUNK_SIZE)

      # If the chunk is empty, we're at the end of the file
      if (length(chunk) == 0) {
        break
      }

      # Calculate the SHA256 hash of the chunk (as raw bytes)
      chunk_hash <- digest::digest(chunk, algo = "sha256", serialize = FALSE, raw = TRUE)

      # Concatenate the raw chunk hash
      all_block_hashes <- c(all_block_hashes, chunk_hash)
    }
  }, finally = {
    # Ensure the file connection is closed
    close(con)
  })

  # Calculate the final SHA256 hash of the concatenated block hashes
  # raw = FALSE gives the hex digest string (like .hexdigest())
  final_hash <- digest::digest(all_block_hashes, algo = "sha256", serialize = FALSE, raw = FALSE)
  final_hash
}

#' Compare local file with dropbox's, and then load it into memory
#'
#' Downloads a file from a specified Dropbox path to a local path.
#'
#' @param dbx_token A short-lived access token
#' @param dropbox_path The full path to the file in your Dropbox (e.g., "/Apps/MyApp/data.csv").
#' @param local_path The local file path to save the file to (e.g., "./my_data.csv").
#' @return The file read either locally or from dropbox. The file is also saved to disk.
#' @export
try_download <- function(dbx_token, dropbox_path, local_path) {
  tryCatch(
    {
      if (file.exists(local_path)) {
        hash_local <- compute_local_hash(local_path = local_path)
        dropbox_hash <- get_dropbox_hash(dbx_token = dbx_token, dropbox_path = dropbox_path)
        # if remote hash available compare; otherwise force download
        if (!is.null(dropbox_hash) && identical(hash_local, dropbox_hash)) {
          message(sprintf("Local file up to date, reading local: %s", local_path))
          return(local_path)
        } else {
          message(sprintf("Local file is modified, downloading from dropbox: %s", dropbox_path))
          tmp <- tempfile(fileext = paste0(".", tools::file_ext(basename(dropbox_path))))
          download_dropbox_file(dbx_token = dbx_token, dropbox_path = dropbox_path, local_path = tmp)
          if (file.exists(tmp)) {
            bytes <- readBin(tmp, "raw", n = file.info(tmp)$size)
            # write downloaded temp file to local_path
            writeBin(bytes, local_path)
            return(tmp)
          } else {
            warning("Dropbox download did not produce a temporary file.")
            return(NULL)
          }
        }
      } else {
        message(sprintf("File not Found, downloading from dropbox: %s", dropbox_path))
        tmp <- tempfile(fileext = paste0(".", tools::file_ext(basename(dropbox_path))))
        download_dropbox_file(dbx_token = dbx_token, dropbox_path = dropbox_path, local_path = tmp)
        if (file.exists(tmp)) {
          bytes <- readBin(tmp, "raw", n = file.info(tmp)$size)
          # write downloaded temp file to local_path
          writeBin(bytes, local_path)
          return(tmp)
        } else {
          warning("Dropbox download did not produce a temporary file.")
          return(NULL)
        }
      }
    },
    error = function(e) {
      message("Erro: ", e$message)
      NULL
    }
  )
}

#' Compare local zip (.shp) file with dropbox's, and then load it into memory
#'
#' Loads a zip file into memory using \link[GEESCHelpersR]{try_download}
#'
#' @param dbx_token A short-lived access token
#' @param dropbox_path The full path to the file in your Dropbox (e.g., "/Apps/MyApp/data.csv").
#' @param local_path The local file path to save the file to (e.g., "./my_data.csv").
#' @return the zip tempdir to be read
#' @export
try_sf_download <- function(dbx_token, dropbox_path, local_path) {
  tempdir <- tempfile()
  path <- try_download(dbx_token = dbx_token, dropbox_path = dropbox_path, local_path = local_path)
  zip::unzip(zipfile = path, exdir = tempdir)
  tempdir
}
