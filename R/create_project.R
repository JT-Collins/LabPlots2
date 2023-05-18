#' create_project
#'
#' @param project_name Generate a new project and subfolders
#'
#' @export

create_project <- function(project_name) {
  # Create main project directory
  dir.create(here::here(project_name))

  # Create subdirectories within the project directory
  subdirs <- c("data", "code", "output", "figs", "doc", "src")

  for (subdir in subdirs) {
    dir.create(here::here(project_name, subdir), recursive = TRUE)
  }

  # Create RStudio project
  setwd(here::here(project_name))
  usethis::create_project(path = here::here(project_name))

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


  readme_path <- here::here(project_name, "README.txt")
  cat(readme_content, file = readme_path)

  cat("Project folders created successfully, a new RStudio project has been initiated, and the README.txt file has been generated.\n")
}
