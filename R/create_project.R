#' create_project
#'
#' @param project_name Generate a new project and subfolders
#'
#' @export

create_project <- function(project_name, location = getwd()) {

  # Check if required packages are installed and load the packages
  required_packages <- c("here", "usethis")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

  if (length(missing_packages) > 0) {
    install_commands <- paste("install.packages('", missing_packages, "')", sep = "")
    stop(paste("The following packages are required:", paste(missing_packages, collapse = ", "),
               "please install using", paste(install_commands, collapse = " or ")))
  }

  # Create RStudio project
  proj_path <- here::here(location, project_name)
  tryCatch(
    usethis::create_project(path = proj_path, open = FALSE),
    error = function(e) {
      warning("An error occurred while creating the RStudio project.")
    }
  )


  # # Create main project directory at the specified or current location
  # proj_dir <- here::here(location, project_name)
  # tryCatch(
  #   dir.create(proj_dir),
  #   error = function(e) {
  #     stop("An error occurred while creating the project directory.")
  #   }
  # )

  # Create subdirectories within the project directory
  subdirs <- c("data", "code", "output", "figs", "doc", "src")

  for (subdir in subdirs) {
    subdir_path <- here::here(location, project_name, subdir)
    tryCatch(
      dir.create(subdir_path, recursive = TRUE),
      error = function(e) {
        warning(paste("An error occurred while creating the '", subdir, "' subdirectory."))
      }
    )
  }


  # Create readme.txt file
  readme_content <- paste0("This is the project folder for '", project_name, "'.\n\n")
  readme_content <- paste0(readme_content, 'Set the Project as the Root Directory: To make the project directory the root directory for your R code, use set_here() from the "here" package. Place this command at the top of your R script.\n\n')
  readme_content <- paste0(readme_content, "Please place the appropriate files into the following folders:\n\n")

  for (subdir in subdirs) {
    explanation <- switch(subdir,
                          data = "contains the raw data files used in the project. These files should not be altered and are ideally read-only.",
                          code = "R scripts or other programming code files.",
                          output = "contains non-figure objects created by the scripts. For example, processed data.",
                          figs = "contains any plots, images, tables, or figures created and saved by your code. It should be possible to delete and regenerate this folder with the scripts in the project folder.",
                          doc = "contains any manuscripts or interim summaries produced with the project.",
                          src = "an optional folder for any files you may want to source() in your scripts. This is not code that is run. For example, simple .R files containing functions."
    )
    readme_content <- paste0(readme_content, "- '", subdir, "': \n\t", explanation, "\n\n")
  }


  readme_path <- here::here(location, project_name, "README.txt")
  cat(readme_content, file = readme_path)

  cat("Project folders created successfully, a new RStudio project has been initiated, and the README.txt file has been generated.\n")
}
