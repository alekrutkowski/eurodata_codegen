# eurodata_codegen
## [R](https://www.r-project.org) code generator for a dataset import from [Eurostat](https://ec.europa.eu/eurostat/databrowser/explore/all/all_themes)
### [Shiny](https://shiny.rstudio.com) app for rapid generation of an autocommented code based on the [eurodata](https://CRAN.R-project.org/package=eurodata) package

▶&#xFE0E; The app can be run in 3 alternative ways:
- in your web browser &ndash; just go to https://shiny-r.dev/eurodata_codegen or https://alekr.shinyapps.io/eurodata_codegen
- run locally in R with `source('https://raw.githubusercontent.com/alekrutkowski/eurodata_codegen/main/app.R')` (or with `source('app.R')` if the [app.R](https://raw.githubusercontent.com/alekrutkowski/eurodata_codegen/main/app.R) file is downloaded to your current working directory)
- put the [app.R](https://raw.githubusercontent.com/alekrutkowski/eurodata_codegen/main/app.R) file in the appropriate sub-directory (sub-folder) of your [Shiny server](https://www.rstudio.com/products/shiny/shiny-server)'s root directory, e.g. in `/srv/shiny-server/eurodata_codegen`

⚠&#xFE0E; If running on your Shiny server or locally in your R, there are some **prerequisites/dependencies**. Install them, if they are not yet installed:
```r
needed_pkgs <-
  c('magrittr','eurodata','data.table','shiny','shinybusy',
    'rclipboard','xml2','prismjs','shinyalert','memoise')
installed_pkgs <-
  row.names(installed.packages())
for (n in needed_pkgs)
  if (!(n %in% installed_pkgs))
      install.packages(n)
```
<br>

💡&#xFE0E; *BTW, you can also **search** Eurostat datasets at https://shiny-r.dev/find_estat or https://alekr.shinyapps.io/find_estat*

<br> A screenshot of the `eurodata_codegen` app:

![Screenshot](screenshot.png)
