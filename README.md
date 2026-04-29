# LabPlots2

reworking of LabPlots

The goal of `LabPlots2` is to standardize the look of ggplot2 plots made in the lab. Sizes based on figures for a "paper", "Grant, and "Presentation" are included. A number of helper functions are also included.

Also includes a function for setting up a project file structure and for basic reading of Cerillo plate reader data.

## Installation

You can install the development version of `LabPlots2` like so:

``` r
# install.packages("pak")
pak::pak("JT-Collins/LabPlots2")

library('LabPlots2')
```

## Functions

`create_project()` Takes two inputs - the name of the new project and the location the project should be located e.g. `create_project("MyProject", ~/Box/Projects)`. If a second variable is not provided then the location will default to the current working directory. Run this function where you want to the project to exist and it will generate a folder structure for you and generate a new RStudio project.

`set_lab_theme(plot_type = "paper")` Sets the standard LabPlot style and should be called once.

`set_lab_theme(plot_type = "grant")` Makes a few adjustments to make plots more suitable for grants.

`set_lab_theme(plot_type = "presentation")` Increases font sizes to make plots more suitable for presentations.

`add_pvalue()` is ported from `ggprism` See [here](https://csdaw.github.io/ggprism/articles/pvalues.html) and is itself refactored from `stat_pvalue_manual` from [kassambara/ggpubr](https://github.com/kassambara/ggpubr), altered to have less dependencies, and more flexibility with input format and aesthetics. Examples using `stat_pvalue_manual` found on [Datanovia](https://www.datanovia.com/en/?s=p-value&search-type=default) will also work with `add_pvalue`.

`cerillo_excel()` This function simply outputs an excel sheet to be filled with the raw OD data and a map detailing what is in each well. At a minimum you should add the 'Strain' and 'Media' to the `map` sheet but it can take as many variables as required. The only input required is the file name which defaults to the current date if nothing is supplied.

`load_cerillo()` is a simple way to import growth curve data from the Cerillo plate readers. This function takes the file name and the time between readings (defaults to 10 mins). The excel file should contain the OD600 readings in a 'dat' sheet and the variable data in a 'map' sheet. These will be joined by Well name.
