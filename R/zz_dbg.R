#' Print a timestamped debugging message with the current process memory
#'
#' Writes the time, the resident memory of the R process (VmRSS, Linux only)
#' and the given text to stderr through message(), which is not buffered, so
#' the line reaches a redirected log file even if the process is killed by
#' the out-of-memory killer right afterwards. Placed before memory-heavy
#' steps of discovery, the last line printed names the step that was running.
#' @param ... pieces of the message, pasted together as in message()
#' @noRd
dbg <- function(...) {
    rssMb <- NA_real_
    if (file.exists("/proc/self/status")) {
        rssLine <- grep("^VmRSS", readLines("/proc/self/status"), value = TRUE)
        rssMb <- round(as.numeric(gsub("\\D", "", rssLine)) / 1024)
    }
    message(format(Sys.time(), "%H:%M:%S"), " | RSS ", rssMb, " Mb | ", ...)
}

#' Name an rcfile for dbg(): its path, or a placeholder if already loaded
#' @param x a path to a read class .rds file, or a loaded readClassSe
#' @noRd
dbgName <- function(x) {
    if (is.character(x)) return(x)
    return("(read classes in memory)")
}
