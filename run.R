# FIDS reader reproduction entry point.
# Reproduces only the figures and tables released in outputs/ from supplied
# aggregate inputs. No individual-level records, model refitting, or external
# downloads are required.

get_repository_root <- function() {
  # source("run.R") / source("/path/to/run.R")
  src <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(src) && nzchar(src)) {
    return(dirname(normalizePath(src, winslash = "/", mustWork = TRUE)))
  }

  # Rscript run.R / Rscript /path/to/run.R
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) {
    path <- sub("^--file=", "", file_arg[1])
    return(dirname(normalizePath(path, winslash = "/", mustWork = TRUE)))
  }

  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

root <- get_repository_root()
main_script <- file.path(root, "R", "reproduce_results.R")
if (!file.exists(main_script)) {
  stop("Cannot locate R/reproduce_results.R relative to run.R.")
}

Sys.setenv(
  FIDS_REPOSITORY_ROOT = root,
  FIDS_OUTPUT_ROOT = file.path(root, "outputs")
)

source(main_script, encoding = "UTF-8")
