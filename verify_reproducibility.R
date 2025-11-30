# verify_reproducibility.R
# -----------------------------------------------------------------------------
# Automates Docker build/run and SAVES A LOG FILE
# -----------------------------------------------------------------------------

verify_in_container <- function(image_name = "replication-pkg-v1", 
                                log_file = "replication_output.log") {
  
  # 1. Check if Docker is running
  docker_cmd <- Sys.which("docker")
  if (docker_cmd == "") stop("Docker not found. Is it installed?")
  
  check <- suppressWarnings(system2(docker_cmd, args = "info", stdout = FALSE, stderr = FALSE))
  if (check != 0) stop("Docker is not running. Open Docker Desktop and wait for it to start.")
  
  message(paste0("\u2714 Docker is ready."))
  
  # 2. Build the Image
  message("\n--- Step 1: Building Docker Image ---")
  message("(This output is displayed here because it's the setup phase)")
  
  build_code <- system2(docker_cmd, args = c("build", "-t", image_name, "."))
  
  if (build_code != 0) stop("x Build Failed. Check the console errors.")
  message("\u2714 Build Successful.")
  
  # 3. Run the Container and Log Output
  message("\n--- Step 2: Running Analysis ---")
  message(paste0("Running container... Output is being saved to '", log_file, "'"))
  message("The console will appear 'frozen' while this runs. Please wait...")
  
  current_dir <- getwd()
  
  # Run arguments
  run_args <- c(
    "run", "--rm", 
    "-v", paste0(shQuote(current_dir), ":/project"), 
    image_name
  )
  
  # Run and Redirect Output to File
  run_code <- system2(
    docker_cmd, 
    args = run_args, 
    stdout = log_file,  # Standard output goes to file
    stderr = log_file   # Errors also go to file
  )
  
  # 4. Verify Success
  if (run_code == 0) {
    message("\n\u2714 SUCCESS: Analysis complete.")
    message(paste0("  See '", log_file, "' for the detailed execution log."))
    
    # Optional: Append a success timestamp to the log
    cat(paste("\n\nVerified Successfully at:", Sys.time()), file = log_file, append = TRUE)
    
  } else {
    message("\n x FAILURE: The script encountered an error.")
    message(paste0("  Open '", log_file, "' to see what went wrong."))
  }
}

# Run it
verify_in_container()
