#' Launch and operate a RevBayes session
#' 
#' @param rb_path Path to RevBayes executable. Defaults to `Sys.getenv("rb.exe")`.
#' Set this default value with `file.edit(".Renviron")` and adding a line
#' `rb.exe=path/to/rb.exe`.
#' @param args Arguments to pass to `rb.exe`.
#' @param timeout Time in seconds to wait for new lines to appear before
#' returning control to the R session.
#'
#' @return `rb_session()` returns a list with methods:
#' - `do(cmd, wait = 0.1, echo = TRUE, timeout = 2)`: execute `cmd`, returning
#'   control to the R terminal after no new output has been received for `wait`
#'   seconds, or no output at all has been received for `timeout` seconds.
#'   if `echo = TRUE`, output will be returned to the R console.
#' - `interact()`: control the session interactively.
#'   A blank line will return control to the R console, without terminating the
#'   RevBayes session.
#' - `quit()`: terminate the session
#'
#' @examples
#' \dontrun{
#'   # New interactive session
#'   rb <- rb_session()
#'   # Run a command
#'   rb$do('print("Hello, world")')
#'   # Continue the session interactively
#'   rb$interact()
#'   # Terminate the session
#'   rb$quit()
#'
#'   # Run a script with argument '123'
#'   rb <- rb_session("path/to/script.Rev 123")
#' }
#' 
#' @importFrom cli cat_line col_blue col_green col_red col_silver col_yellow
#' @importFrom processx process
#' @export
# install.packages("processx")
rb_session <- function(args = character(), rb_path = Sys.getenv("rb.exe"),
                       timeout = if (length(args) > 0) 2 else 0.5) {

  p <- processx::process$new(
    command = rb_path,
    args = args,
    stdin = "|",
    stdout = "|",
    stderr = "2>&1"
  )
  
  .output_buffer <- character()
  
  .echo_line <- function(line) {
    errPrefix <- "Error"
    warnPrefix <- c("Missing Variable")
    
    .starts_with <- function(prefix) {
      start <- paste0("   ", prefix, ":\t")
      for (pref in start) {
        if (startsWith(line, pref)) return(TRUE)
      }
      FALSE
    }
    
    if (.starts_with(errPrefix)) {
      cat_line(col_red(line))
    } else if (.starts_with(warnPrefix)) {
      cat_line(col_yellow(line))
    } else {
      cat_line(col_blue(line))
    }
  }
  
  .echo_until_prompt <- function(timeout = 2, stable_wait = 0.1, echo = TRUE) {
    prompt = "^>\\s*$"
    buffer <- character()
    start <- Sys.time()
    last_new <- start
    repeat {
      # try to read any new lines
      new <- p$read_output_lines()
      if (length(new)) {
        buffer <- c(buffer, new)
        if (any(nzchar(new))) {
          last_new <- Sys.time()
          if (isTRUE(echo)) {
            sapply(new, .echo_line)
          }
        }
      } else {
        if (as.numeric(difftime(Sys.time(), last_new, units = "secs")) > stable_wait) break
      }
      if (as.numeric(difftime(Sys.time(), start, units = "secs")) > timeout) break
      Sys.sleep(0.05)
    }
    paste(buffer, collapse = "\n")
  }
  
  interact <- function(wait = 0.1, echo = TRUE, timeout = 2) {
    while(nzchar(cmd <- readline("rb> "))) {
      send(cmd, wait = wait, echo = echo, timeout = timeout)
    }
    if (p$is_alive()) {
      cat_line(col_green(
        "Left interactive mode; RevBayes session continues in background")
        )
    }
  }
  
  send <- function(cmd, wait = 0.1, echo = TRUE, timeout = 2) {
    if (!p$is_alive()) stop("RevBayes not running")
    p$write_input(paste0(cmd, "\n"))
    out <- .echo_until_prompt(stable_wait = wait, timeout = timeout, echo = echo)
    invisible(out)
  }
  
  stop_session <- function() {
    if (p$is_alive()) p$kill()
    invisible(TRUE)
  }
  
  banner <- .echo_until_prompt(stable_wait = timeout)
  
  # return a proper list of *functions*
  list(
    process = p,
    do = send,
    interactive = interact,
    quit = stop_session
  )
}
