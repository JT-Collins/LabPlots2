# LabPlots2
reworking of LabPlots

The goal of `LabPlots2` is to standardize the look of ggplot2 plots made in the lab. It makes use of `hrbrthemes` with some tweaks and a custom colour palate. A number of helper functions are also included.

## Installation

You can install the development version of `LabPlots2` like so:

``` r
# install.packages("devtools")
devtools::install_github("JT-Collins/LabPlots2")

library('LabPlots2')
```
## Functions

`lab_style()` Sets the standard LabPlot style and should be called once
`grant_style()` Makes a few adjustments to make plots more suitable for grants/papers
`add_pvalue()` is ported from `ggprism` See [here](https://csdaw.github.io/ggprism/articles/pvalues.html) and is itself refactored from  `stat_pvalue_manual` from [kassambara/ggpubr](https://github.com/kassambara/ggpubr), altered to have less dependencies, and more flexibility with input format and aesthetics. Examples using `stat_pvalue_manual` found on [Datanovia](https://www.datanovia.com/en/?s=p-value&search-type=default) will also work with `add_pvalue`.
`load_cerillo()` is a simple way to import growth curve data from the cerillo plate readers. This function takes the file name and the time between readings (defaults to 10 mins). The excel file should contain the OD600 readings in a 'dat' sheet and the variable data in a 'map' sheet. These will be joined by Well name
