#' Print debugging output with cli
#' @noRd

debug_cli <- function(debug,
                      fun = cli::cli_alert,
                      text = "",
                      ...){
  
  if (debug){
    
    ## identify calling function in namespace
    ns <- ls(getNamespace(name = "simutils"))  # insert current name
    which <- -1
    repeat{
      fn <- sys.call(which = which)[1]
      fn <- gsub("\\(.*", "", as.character(fn))
      fn <- gsub(".*:", "", fn)
      if (length(fn) == 0 || fn %in% ns) break
      which <- which - 1
    }
    if (length(fn) == 0)
      fn <- "[UNKNOWN]"
    
    fn <- sprintf("{.field {.strong %s}}:", fn)
    fn <- stringr::str_pad(fn, width = max(debug_width() + 10 + 9,
                                           nchar(fn) + 2), side = "right")
    
    ## text message
    text <- c(fn, text)  # glue
    
    ## prepare and execute function
    if (!is.function(fun)){
      
      ## TODO: replace with cli::cli_text
      fun <- cli::cli_alert
    }
    if (identical(fun, cli::cli_progress_bar)){
      
      text <- c(cli::symbol$arrow_right, " ", text)
    }
    
    args <- list(...)
    if (is.null(args[[".envir"]]))
      args[[".envir"]] <- sys.frame(which = which)
    
    ## add text
    formals_nms <- names(formals(fun))
    if ("text" %in% formals_nms){
      
      args$text <- text
      
    } else if ("msg" %in% formals_nms){
      
      args$msg <- text
      
    } else if ("message" %in% formals_nms){
      
      ## TODO: check glue behavior of cli::cli_abort()
      args$message <- text
      
    } else if ("format" %in% formals_nms){
      
      args$format <- text
      
    }
    ## modify other arguments
    if ("format_done" %in% names(args)){
      
      args$format_done <- c(green_tick, " ", fn, args$format_done)
    }
    if ("format_failed" %in% names(args)){
      
      args$format_failed <- c(red_cross, " ", fn, args$format_failed)
    }
    do.call(what = fun, args = args)
  }
}



#' Determine minimum debug width
#' @noRd

debug_width <- function(){
  
  ns <- ls(getNamespace(name = "simutils"))
  ns <- ns[!grepl("_simutils", ns)]  # exclude _simutils*
  max(nchar(ns)) + 2
}



#' Generate unique ID based on time
#'
#' This function generates a unique ID based on the current time in the format 
#' %Y-%m-%d_%H-%M-%OS3, replacing the decimal point in seconds with a dash '-'.
#'
#' @param time A POSIXct object representing the time to use for generating the ID. 
#' Defaults to the current system time (`Sys.time()`).
#' @return A character string representing the unique ID.
#' @examples
#' unique_id <- time2id()
#' print(unique_id)
#' @noRd

time2id <- function(time = Sys.time()){
  
  ## Format the time to include milliseconds (%OS3)
  formatted_time <- format(time, "%Y-%m-%d_%H-%M-%OS3")
  
  ## Replace the decimal point in seconds with a dash '-'
  unique_id <- gsub("\\.", "-", formatted_time)
  
  ## Return the generated unique ID
  return(unique_id)
}



#' Check and validate seed value(s)
#'
#' This function ensures the input `seed` is a numeric scalar or vector, clamps its values between 
#' 0 and .Machine$integer.max, and converts it to an integer.
#'
#' @param seed A numeric scalar or vector.
#' @return An integer scalar or vector with values clamped to the valid range.
#' @examples
#' valid_seed <- validate_seed(c(-1, 1e10, 123))
#' print(valid_seed)
#' @noRd

validate_seed <- function(seed){
  
  ## Check if the input is numeric
  if (!is.numeric(seed)){
    stop("Input must be numeric.")
  }
  
  ## Clamp values between 0 and .Machine$integer.max
  clamped_seed <- pmin(pmax(seed, 0), .Machine$integer.max)
  
  ## Convert to integer
  integer_seed <- as.integer(clamped_seed)
  
  ## Return the processed seed
  return(integer_seed)
}



#' Validate the number of cores
#'
#' Ensures that the `n_cores` parameter is a positive integer within the range of available cores.
#' If `n_cores` is -1, it sets it to the maximum number of detected cores.
#'
#' @param n_cores An integer specifying the desired number of cores.
#' @return A validated integer number of cores, clamped to the range [1, maximum cores].
#' @examples
#' validated_cores <- validate_n_cores(-1)
#' print(validated_cores)
#' @noRd

validate_n_cores <- function(n_cores){
  
  ## Detect maximum number of cores
  max_cores <- parallel::detectCores()
  
  ## Coerce to 1 if invalid
  if (!is.numeric(n_cores) || length(n_cores) != 1){
    
    debug_cli(TRUE, cli::cli_alert_warning,
              "n_cores = {n_cores} must be a numeric scalar; coercing to 1",
              .envir = environment())
    
    n_cores <- 1
  }
  
  ## Convert to integer
  n_cores <- as.integer(n_cores)
  
  ## If n_cores == -1, set to maximum number of cores
  if (n_cores == -1){
    
    n_cores <- max_cores
    
  }  # Else if necessary, restrict to [1, max_cores]
  else if (n_cores < 1 || n_cores > max_cores){
    
    debug_cli(TRUE, cli::cli_alert_warning,
              "restricting n_cores = {n_cores} to between 1 and {max_cores}",
              .envir = environment())
    
    n_cores <- min(max(1, n_cores), max_cores)
  }
  
  return(n_cores)
}



#' Check simulation directory
#'
#' Ensures the simulation directory contains the required folder and settings file. If the specified
#' folder does not exist, it creates the folder and saves the provided settings. If the folder exists,
#' it validates the settings file.
#'
#' @param sim_id A character string representing the simulation ID.
#' @param sim_dir A character string representing the directory path where simulations are stored.
#' @param sim_settings A list containing the simulation settings to be saved or validated.
#' @param debug An integer for debugging level. Defaults to 0 (no debugging).
#' @return Returns `TRUE` invisibly if all checks pass.
#' @noRd

check_sim_id <- function(sim_id,
                         sim_dir,
                         sim_settings,
                         debug = 0){
  
  sim_dir_id <- file.path(sim_dir, sim_id)
  
  if (!dir.exists(sim_dir_id) || length(list.files(sim_dir_id)) == 0){
    
    ## If necessary, create a folder named sim_id in sim_dir
    if (!dir.exists(sim_dir_id)){
      
      dir.create(sim_dir_id, recursive = TRUE)
    }
    
    ## Save sim_settings in the sim_id folder as "sim_settings.rds"
    saveRDS(sim_settings, file = file.path(sim_dir_id, "sim_settings.rds"))
    
  } else{
    
    debug_cli(!file.exists(file.path(sim_dir_id, "sim_settings.rds")), cli::cli_abort,
              "folder {sim_id} exists and is non-empty, but {.file sim_settings.rds} is missing: please check and delete folder if necessary",
              .envir = environment())
    
    ## Read file "sim_settings.rds" as object named sim_settings0
    sim_settings0 <- readRDS(file.path(sim_dir_id, "sim_settings.rds"))
    
    ## Compare the settings
    ae <- all.equal(sim_settings, sim_settings0)
    debug_cli(!is.logical(ae) || !ae, cli::cli_abort,
              "existing {.file sim_settings.rds} do not match supplied `sim_settings`: {ae}",
              .envir = environment())
  }
  
  return(invisible(TRUE))
}



#' Check simulation grid
#' @noRd

check_sim_grid <- function(sim_grid){
  
  
  ## If not a data.frame, throw error
  debug_cli(!is.data.frame(sim_grid), cli::cli_abort,
            "sim_grid must be a data.frame; class(sim_grid) = {class(sim_grid)}",
            .envir = environment())
  
  
  ## TODO: add additional checks
  
  
  return(invisible(TRUE))
}
