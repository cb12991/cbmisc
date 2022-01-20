start_time <- function(beg_msg = '', in_func = TRUE) {
  stm <- get0('.stm', envir = .GlobalEnv, ifnotfound = list(start_time = NA))
  msg <- get0(
    '.msg', envir = .GlobalEnv, ifnotfound = list(st_msg = NA, rt_msg = NA)
  )
  
  i <- ifelse(is.na(stm[[1]][1]), 1, length(stm[[1]]) + 1)
  
  stm[['start_time']][i] <- Sys.time()
  msg[['st_msg']][i] <- ifelse(
    beg_msg != '',
    beg_msg,
    paste0('Run Timer ', i)
  )
  
  assign('.msg', msg, envir = .GlobalEnv)
  assign('.stm', stm, envir = .GlobalEnv)
  
  return(
    if (in_func) 
      message('\n', Sys.time(), ' >> ', msg[['st_msg']][i])
    else 
      cat('\n', Sys.time(), ' >> ', msg[['st_msg']][i])
  )
}
st <- function(beg_msg = '', in_func = TRUE) {
  return(start_time(beg_msg, in_func))
}

run_time <- function(time_only = FALSE, in_func = TRUE, end_msg = 'DONE: ') {
  rtm <- get0('.rtm', envir = .GlobalEnv, ifnotfound = list(run_time = NA))
  stm <- get0('.stm', envir = .GlobalEnv, ifnotfound = list(start_time = NA))
  msg <- get0(
    '.msg', envir = .GlobalEnv, ifnotfound = list(st_msg = NA, rt_msg = NA)
  )
  
  if (is.na(stm[[1]][1])) 
    stop('No start time has been initialized.')
  else {
    length(rtm[[1]]) <- length(stm[[1]])
    i <- suppressWarnings(max(which(is.na(rtm[['run_time']]))))
    if (is.finite(i)) {
      rtm[['run_time']][i] <- round(
        x = as.numeric(Sys.time()) - stm[['start_time']][i],
        digits = 3
      )
      msg[['rt_msg']][i] <- paste0(
        end_msg, rtm[['run_time']][i], ' seconds elapsed'
      )
      assign('.msg', msg, envir = .GlobalEnv)
      assign('.rtm', rtm, envir = .GlobalEnv)
      result <- ifelse(
        test = time_only, 
        yes = rtm[['run_time']][i], 
        no = msg[['rt_msg']][i]
      )
      if (in_func) 
        message(result)
      else
        cat(result)
    }
  }
}
rt <- function(time_only = FALSE, in_func = TRUE, end_msg = 'DONE: ') {
  return(run_time(time_only, in_func, end_msg))
}

clear_tms <- function() {
  suppressWarnings(rm(.stm, .msg, .rtm, envir = .GlobalEnv))
}

view_tms <- function() {
  rtm <- get0('.rtm', envir = .GlobalEnv, ifnotfound = list(run_time = NA))
  stm <- get0('.stm', envir = .GlobalEnv, ifnotfound = list(start_time = NA))
  msg <- get0(
    '.msg', envir = .GlobalEnv, ifnotfound = list(st_msg = NA, rt_msg = NA)
  )
  return(tibble::as_tibble(msg))
}

save_tms <- function(dir = paste0(getwd(), '/Data/'), name = '.tms') {
  rtm <- get0('.rtm', envir = .GlobalEnv, ifnotfound = list(run_time = NA))
  stm <- get0('.stm', envir = .GlobalEnv, ifnotfound = list(start_time = NA))
  msg <- get0(
    '.msg', envir = .GlobalEnv, ifnotfound = list(st_msg = NA, rt_msg = NA)
  )
  if (substring(dir, nchar(dir)) != '//') dir <- paste0(dir, '/')
  
  if (any(is.null(c(rtm[[1]], stm[[1]])))) 
    warning(
      'Missing start or end times, so no file saved. View log for more info ',
      'by entering `view_tms()` in the console'
    )
  else {
    tms <- get0(
      x = name, 
      envir = .GlobalEnv,
      ifnotfound = NULL
    )
    tms <- c(tms, list(stm, rtm, msg))
    assign(name, tms, envir = .GlobalEnv)
    save(tms, file = paste0(dir, name, '.Rdata'))
  }
}

load_tms <- function(dir = paste0(getwd(), '/Data/'), name = '.tms.Rdata') {
  if (substring(dir, nchar(dir)) != '//') dir <- paste0(dir, '/')
  path <- paste0(dir, name)
  if (file.exists(path))
    load(path, envir = .GlobalEnv)
  else
    warning(paste0('No runtime file found in ', dir))
}