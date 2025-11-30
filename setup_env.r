# setup_env.R
# -------------------------------------------------------------------
# How to use:
#   1) Put this file next to requirements.txt
#   2) In RStudio: open this file and click "Source"
#      - or: source("setup_env.R")
#   3) When prompted, choose:
#        [1] Strict (use exact versions from requirements.txt)
#        [2] Relaxed (ignore versions; install latest CRAN)
# -------------------------------------------------------------------

options(repos = c(CRAN = "https://cloud.r-project.org"))

ensure_pkg <- function(p) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p, quiet = TRUE)
}

# Prompt for mode if interactive; default to strict otherwise
pick_mode <- function() {
  if (interactive()) {
    ans <- utils::menu(c("Strict", "Relaxed"),
                       title = "Install mode (enter 1 or 2):")
    if (ans == 2) return("relaxed")
    return("strict")
  } else {
    message("Non-interactive session detected: Defaulting to STRICT mode.")
    return("strict")
    }
}

read_requirements <- function(path = "requirements.txt") {
  if (!file.exists(path)) stop(sprintf("requirements file not found: %s", path), call. = FALSE)
  lines <- readLines(path, warn = FALSE)
  lines <- trimws(lines)
  lines <- lines[lines != "" & !grepl("^\\s*#", lines)]
  parse_one <- function(x) {
    if (grepl("==", x, fixed = TRUE)) {
      parts <- strsplit(x, "==", fixed = TRUE)[[1]]
      list(name = trimws(parts[1]), version = trimws(parts[2]))
    } else {
      list(name = trimws(x), version = NA_character_)
    }
  }
  lapply(lines, parse_one)
}

install_one <- function(entry, mode) {
  pkg <- entry$name
  ver <- entry$version
  if (identical(mode, "relaxed") || is.na(ver) || !nzchar(ver)) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message(sprintf("[relaxed] Installing latest %s ...", pkg))
      install.packages(pkg, quiet = TRUE)
    } else {
      message(sprintf("[relaxed] %s already installed.", pkg))
    }
  } else {
    ensure_pkg("remotes")
    message(sprintf("[strict] Installing %s==%s ...", pkg, ver))
    remotes::install_version(pkg, version = ver, upgrade = "never", quiet = TRUE)
  }
}

install_requirements <- function(req_file = "requirements.txt", mode = NULL) {
  if (is.null(mode)) mode <- pick_mode()
  reqs <- read_requirements(req_file)
  
  message(sprintf("Mode: %s", toupper(mode)))
  message(sprintf("Using %s", normalizePath(req_file, winslash = "/")))
  message("This may take a few minutes...")
  
  failures <- list()
  for (r in reqs) {
    tryCatch(
      install_one(r, mode),
      error = function(e) {
        msg <- sprintf("Failed: %s%s (%s)",
                       r$name,
                       if (!is.na(r$version) && nzchar(r$version)) paste0("==", r$version) else "",
                       e$message)
        message(msg)
        failures[[length(failures) + 1]] <<- msg
      }
    )
  }
  
  cat("\n----- Environment Summary -----\n")
  cat("R version: ", paste(R.version$major, R.version$minor, sep = "."), "\n", sep = "")
  for (r in reqs) {
    pkg <- r$name
    if (requireNamespace(pkg, quietly = TRUE)) {
      cat(sprintf("  - %s %s\n", pkg, as.character(utils::packageVersion(pkg))))
    } else {
      cat(sprintf("  - %s (NOT INSTALLED)\n", pkg))
    }
  }
  if (length(failures)) {
    cat("\nSome installs failed:\n")
    cat(paste0("  * ", unlist(failures), collapse = "\n"), "\n")
    if (!identical(mode, "relaxed")) {
      cat("\nTip: re-run in RELAXED mode to allow latest versions.\n")
    }
  }
  cat("--------------------------------\n")
  invisible(NULL)
}

# Auto-run when sourced:
install_requirements()
