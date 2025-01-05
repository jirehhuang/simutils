#' Simulate across settings
#'
#' Simulates a simulation function across combinations of settings.
#'
#' @param sim_fn A simulation function.
#' @param sim_dir A character string representing the directory path of the simulation. \code{dirname(sim_dir)} must exist, but \code{basename(sim_dir)} will be created if necessary.
#' @param n_reps A positive integer representing the number of repetitions of \code{sim_fn} for each combination of settings.
#' @param seed A list containing the simulation settings to be saved or validated.
#' @param n_cores A positive integer representing the number of cores to use for execution. Set to -1 to automatically detect the number of cores.
#' @param mc_method A character value representing the multicore execution method. "basic" allocates one core to each unique combination of settings, whereas "redundant" will give every core an opportunity to work on each setting combination.
#' @param debug An integer for debugging level. Defaults to 1.
#' @param ... Additional settings passed to \code{sim_fn}. \code{sim_fn} will be executed over each unique combination of settings. Optionally, a \code{sim_grid} data.frame object can be provided with the desired setting combinations.
#' @return Returns list of simulation results.
#' @examples
#' ## Example simulation function
#' sim_fn <- function(n, mean, sd){
#'   Sys.sleep(0.5)
#'   x <- rnorm(n = n, mean = mean, sd = sd)
#'   data.frame(mean = mean(x),
#'              sd = sd(x))
#' }
#' 
#' ## Example execution using ... and single core
#' res1 <- sim_across(sim_fn = sim_fn, sim_dir = "inst/example/example1",
#'                    n_reps = 16, seed = 1, n_cores = 1,
#'                    n = seq_len(2) * 10, mean = seq_len(2), sd = seq_len(2),
#'                    debug = 1)
#' 
#' ## Example execution using sim_grid and basic multicore
#' sim_grid <- expand.grid(n = seq_len(2) * 10,
#'                         mean = seq_len(2),
#'                         sd = seq_len(2))
#' 
#' res2 <- sim_across(sim_fn = sim_fn, sim_dir = "inst/example/example2",
#'                    n_reps = 16, seed = 1, n_cores = -1, mc_method = "basic",
#'                    sim_grid = sim_grid,
#'                    debug = 1)
#' 
#' all.equal(res1, res2, check.attributes = FALSE)
#' 
#' ## Example execution using ... and redundant multicore
#' res3 <- sim_across(sim_fn = sim_fn, sim_dir = "inst/example/example3",
#'                    n_reps = 16, seed = 1, n_cores = -1, mc_method = "redundant",
#'                    n = seq_len(2) * 10, mean = seq_len(2), sd = seq_len(2),
#'                    debug = 1)
#' @export

sim_across <- function(sim_fn,
                       sim_dir = file.path(getwd(), time2id()),
                       n_reps = 1,
                       seed = 1,
                       n_cores = 1,
                       mc_method = c("basic", "redundant"),
                       debug = 1,
                       ...){
  
  
  ## Start timer
  start_time <- Sys.time()
  job_id <- time2id(time = start_time)
  
  
  ## ================================================================================##
  ## Check arguments
  ## ================================================================================##
  
  ## If sim_fn is missing, throw error
  debug_cli(missing(sim_fn), cli::cli_abort,
            "sim_fn must be provided")
  
  ## If n_reps is not a positive integer, throw error
  debug_cli(n_reps != round(n_reps) || n_reps < 1, cli::cli_abort,
            "n_reps = {n_reps} is invalid: must be a positive integer", 
            .envir = environment())
  
  ## Validate seed; coerce to a valid value if necessary
  seed <- head(validate_seed(seed), 1)
  
  ## Validate n_cores; coerce to a valid value if necessary
  n_cores <- validate_n_cores(n_cores = n_cores)
  
  ## Check multicore method, overriding to "basic" if necessary
  mc_method <- match.arg(mc_method)

  if (mc_method != "basic" && n_reps == 1){
    
    debug_cli(TRUE, cli::cli_alert_warning,
              "mc_method = {mc_method} is not applicable when n_reps == 1",
              .envir = environment())
    
    mc_method <- "basic"
  }
  
  ## If debug is not a vector of length 1, throw error
  debug_cli(!is.vector(debug) || length(debug) != 1, cli::cli_abort,
            "debug = {debug} is invalid: must be a vector of length 1", 
            .envir = environment())
  
  
  ## ================================================================================##
  ## Initialize
  ## ================================================================================##
  
  ## Create a list named `sim_settings` containing all arguments as named elements
  ## except for debug, including those provided through "..." (ellipsis)
  inputs <- list(sim_fn = sim_fn,
                 sim_dir = sim_dir,
                 n_cores = n_cores,
                 n_reps = n_reps,
                 seed0 = seed,
                 debug = debug)
  
  ellipsis <- list(...)
  
  sim_settings <- c(inputs, ellipsis)
  
  ## Create simulation grid containing unique combinations of settings
  sim_grid <- do.call(expand.grid, sim_settings[sapply(sim_settings, is.vector)])
  
  ## If sim_grid provided
  if ("sim_grid" %in% names(sim_settings)){
    
    check_sim_grid(sim_grid = ellipsis$sim_grid)
    
    sim_grid <- merge(sim_grid, sim_settings$sim_grid, by = NULL)
  }
  
  ## Create parameter grid
  sim_par_grid <- sim_grid[, names(sim_grid) %in% names(formals(sim_fn))]
  
  ## Subset to unique parameter values
  unique_rows <- !duplicated(sim_par_grid)
  sim_grid <- sim_grid[unique_rows,,drop = FALSE]
  sim_par_grid <- sim_par_grid[unique_rows,,drop = FALSE]
  
  ## If basic execution, restrict n_cores to at most the number of unique settings
  if (mc_method %in% c("basic")){
    
    n_cores <- min(n_cores, nrow(sim_grid))
  }
  
  ## Use overall seed to generate seeds for each combination of settings
  set.seed(seed)
  sim_grid$seed1 <- sample(.Machine$integer.max, nrow(sim_grid))
  
  ## Include in sim_settings for convenience and archival purposes
  sim_settings$sim_grid <- sim_grid
  
  
  ## Check contents of sim_dir against sim_settings, creating sim_dir if necessary
  check_sim_dir(sim_dir,
                sim_settings = sim_settings,
                debug = debug)
  
  
  ## Other simulation objects
  pad_width_i <- nchar(nrow(sim_grid))
  
  
  ## Create simulation wrapper function
  sim_fn_ <- function(i, sim_grid, ...){
    
    tryCatch({
      
      ## Prepare arguments: formals of sim_fn_() and sim_fn()
      seed_i <- sim_grid$seed1[i]
      sim_args <- as.list(sim_grid[i,])[names(sim_grid) %in% names(formals(sim_fn))]
      
      ## Simulation number and file/folder names
      pad_i <- stringr::str_pad(i, width = pad_width_i, side = "left", pad = "0")  # left pad with 0's
      file_i <- file.path(sim_dir, sprintf("%s.rds", pad_i))
      temp_i <- gsub("\\.rds", ".tmp", file_i)
      dir_i <- gsub("\\.rds", "", file_i)
      
      debug_cli(debug, cli::cli_alert,
                "executing {pad_i}", .envir = environment())
      
      ## If completed, skip next
      if (file.exists(file_i)) return(invisible(FALSE))
      
      ## Set seed
      set.seed(seed_i)
      
      ## If n_reps == 1
      if (n_reps == 1){
        
        ## If in progress, skip next
        if (file.exists(temp_i)) next
        
        ## Write temp file to indicate in progress
        write.table(0, file = temp_i, row.names = FALSE, col.names = FALSE)
        
        ## Execute simulation function
        res_i <- do.call(sim_fn, sim_args)
        
        ## write sub-task result and delete temp file
        saveRDS(object = res_i, file = file_i)
        file.remove(temp_i)
        
      }  # Else (n_reps > 1)
      else{
        
        ## If doesn't already exist, create folder for sub-tasks
        if (!dir.exists(paths = dir_i)){
          
          dir.create(path = dir_i)
        }
        
        ## Generate seeds for each repetition
        seeds2 <- sample(.Machine$integer.max, n_reps)
        
        ## Pad width for repetitions
        pad_width_j <- nchar(n_reps)
        
        ## For each repetition
        for (j in seq_len(n_reps)){
          
          tryCatch({
            
            ## Sub-task number and file names
            pad_ij <- sprintf("%s-%s", pad_i, stringr::str_pad(j, width = pad_width_j, side = "left", pad = "0"))
            file_ij <- file.path(dir_i, sprintf("%s.rds", pad_ij))
            temp_ij <- gsub("\\.rds", ".tmp", file_ij)
            
            debug_cli(debug, cli::cli_alert,
                      "executing {pad_ij}", .envir = environment())
            
            ## If file_i or file_ij exists (complete) or temp_ij exists (in progress), skip next
            if (file.exists(file_i) || file.exists(file_ij) || file.exists(temp_ij)) next
            
            ## Write temp file to indicate sub-task in progress
            write.table(0, file = temp_ij, row.names = FALSE, col.names = FALSE)
            
            ## Execute simulation function
            res_ij <- do.call(sim_fn, sim_args)
            
            ## write sub-task result and delete temp file
            saveRDS(object = res_ij, file = file_ij)
            file.remove(temp_ij)
            
          }, error = function(err){
            
            debug_cli(TRUE, cli::cli_alert_danger,
                      "error executing {pad_ij}: {as.character(err)}",
                      .envir = environment())
            
            cat(sprintf("error executing %s: %s",
                        pad_ij, as.character(err)), 
                file = gsub("\\.tmp", ".err", temp_ij))
          })
        }  # End for j
        
        ## If all n_reps present, compile result if haven't already
        
      }  # End else (if n_reps > 1)
      
      ## If n_reps == 1, write task result delete temp file
      if (n_reps == 1){
        
        saveRDS(object = res_i, file = file_i)
        
        file.remove(temp_i)
        
      }  # Else, if all n_reps present, compile result if necessary
      else if (length(files_i <- list.files(dir_i, pattern = "\\.rds", full.names = TRUE)) == n_reps){
        
        ## Compile and write task result
        res_i <- lapply(files_i, readRDS)
        saveRDS(object = res_i, file = file_i)
        
        ## Delete sub-task folder
        unlink(dir_i, recursive = TRUE)
      }
      
      return(invisible(TRUE))
      
    }, error = function(err){
      
      debug_cli(TRUE, cli::cli_alert_danger,
                "error executing {pad_i}: {as.character(err)}",
                .envir = environment())
      
      cat(sprintf("error executing %s: %s",
                  pad_i, as.character(err)), 
          file = gsub("\\.tmp", ".err", temp_i))
      
      return(invisible(FALSE))
    })
  }  # End sim_fn_()
  
  
  ## ================================================================================##
  ## Execute simulation
  ## ================================================================================##
  
  ## Write timer
  cat(glue::glue("Job ID:      {job_id}
                  Start time:  {format(start_time, '%Y-%m-%d %H:%M:%S %Z')}
                  End time:    
                  Run time:    "),
      file = (job_file <- file.path(sim_dir, sprintf("job_%s.txt", job_id))))
  
  
  ## Set indices to execute over based on mc_method
  X <- seq_len(nrow(sim_grid))
  X <- switch(mc_method,
              redundant = rep(seq_len(nrow(sim_grid)), each = n_cores),
              X)  # default basic
  
  
  ## Execute, in parallel if possible
  if (Sys.info()[["sysname"]] == "Windows" && n_cores > 1){
    
    cl <- parallel::makeCluster(spec = getOption("cl.cores", n_cores))
    
    executed <- parallel::parLapply(cl = cl,
                                    X = X,
                                    fun = sim_fn_,
                                    sim_grid = sim_grid)
    
    parallel::stopCluster(cl)
    
  } else{
    
    executed <- parallel::mclapply(X = X,
                                   FUN = sim_fn_,
                                   sim_grid = sim_grid, 
                                   mc.preschedule = FALSE, 
                                   mc.cores = n_cores)
  }
  
  ## End timer
  if (any(unlist(executed))){
    
    end_time <- Sys.time()
    run_time <- prettyunits::pretty_sec(as.numeric(end_time - start_time, unit = "secs"))
    cat(glue::glue("Job ID:      {job_id}
                    Start time:  {format(start_time, '%Y-%m-%d %H:%M:%S %Z')}
                    End time:    {format(end_time, '%Y-%m-%d %H:%M:%S %Z')}
                    Run time:    {run_time}"),
        file = job_file)
    
  } else{
    
    file.remove(job_file)
  }
  
  
  ## Return existing simulation results
  return(invisible(read_sim_dir(sim_dir = sim_dir)))
}



#' Read simulation directory
#'
#' Reads and returns results existing in a simulation directory.
#'
#' @param sim_dir A character string representing the directory path of the simulation.
#' @return Returns list of simulation results.
#' @export

read_sim_dir <- function(sim_dir){
  
  ## Return existing simulation results
  res <- lapply(list.files(sim_dir, 
                           pattern = "^\\d+\\.rds", full.names = TRUE), readRDS)
  attr(res, 
       "sim_settings") <- readRDS(file.path(sim_dir, "sim_settings.rds"))
  
  return(invisible(res))
}



#' Clear simulation directory
#'
#' Clears all temporary files (indicating in progress) from a simulation directory.
#'
#' @param sim_dir A character string representing the directory path of the simulation.
#' @param debug An integer for debugging level. Defaults to 1.
#' @return Returns (invisibly) the output of \code{file.remove()}.
#' @export

clear_sim_dir <- function(sim_dir,
                          debug = 1){
  
  tmp_files <- list.files(sim_dir, pattern = "\\.tmp", 
                          all.files = TRUE, recursive = TRUE, full.names = TRUE)
  
  debug_cli(length(tmp_files) && debug, cli::cli_alert_info,
            "deleting {length(tmp_files)} .tmp file{ifelse(length(tmp_files) == 1, '', 's')}: {tmp_files}",
            .envir = environment())
  
  return(invisible(file.remove(tmp_files)))
}
