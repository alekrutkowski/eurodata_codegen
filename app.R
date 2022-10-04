library(magrittr)
library(eurodata)
library(data.table)
library(shiny)
library(rclipboard)

# Helpers

codeWithLabel <- function(code, label)
  paste0('[',code,']',
         label %>%
           {ifelse(is.na(.),"",paste0(" ",.))})

codeWithLabelInNames <- function(code, label)
  code %>% 
  setNames(codeWithLabel(code,label))

withNotNull <- function(.df, ...)
  if (!is.null(.df)) with(.df, ...)

memoImportLabels <-
  memoise::memoise(importLabels)

describe_dt_to_Rcode <- function(describe_dt)
  describe_dt %>% {
    if (nrow(.)==0) "" else 
      .[, is_single_val := length(Dim_val)==1, by=`Dim_name`] %>%
      .[, sep := ifelse(is_single_val, " & ", ', '), by=`Dim_name`] %>%
      .[, sep := ifelse(!is_single_val & Dim_val==last(Dim_val), ") & ", sep), by=`Dim_name`] %>%
      .[, sep := ifelse(Dim_val==last(Dim_val),
                        ifelse(sep==") & ",') '," "),
                        sep)] %>%
      .[, comment := Dim_val_label %>% {ifelse(is.na(.),"",paste0('# ',.))} %>%
          paste0(ifelse(only_one,'# THE ONLY OPTION AVAILABLE ',""),.,'\n')] %>% 
      .[, Dim_val := paste0('"',Dim_val,'"')] %>% 
      .[, Dim_val := Dim_val %>% ifelse(is_single_val,.,paste0('  ',.))] %>%
      .[, Dim_name := paste0('# ',Dim_name_label,':\n',Dim_name)] %>% 
      .[,.(Rcode = ifelse(is_single_val,
                          paste0(Dim_name,'==',Dim_val,sep,comment),
                          paste0(Dim_val,sep,comment) %>% 
                            paste(collapse="") %>% 
                            paste0(Dim_name,' %in% c(\n',.)) %>% 
             unique()),
        by=`Dim_name`] %>%
      .$Rcode %>% 
      paste(collapse="")
  }

colsToFormula <- function(col_names) {
  standard_panel_id_varnames <-
    c('geo','TIME_PERIOD','nace_r2','nace_r1')
  lhs <-
    col_names %>% 
    .[. %in% standard_panel_id_varnames]
  rhs <-
    col_names %>% 
    setdiff(standard_panel_id_varnames)
  list(lhs,rhs) %>% 
    sapply(. %>%
             `if`(length(.)==0,"...",.) %>% 
             paste(collapse=" + ")) %>% 
    paste(collapse=" ~ ")
}

dcastCode <- function(metadata_dt) {
  useful_cols_names <-
    metadata_dt[!(only_one),Dim_name] %>%
    unique()
  formula_code <-
    useful_cols_names %>%
    colsToFormula()
  if (grepl('...', formula_code, fixed=TRUE))
    paste0('.[, .(',paste(c('value_',useful_cols_names),collapse=", "),')]') else
      paste0('dcast(',
             formula_code,
             ',\n      fun.aggregate=identity, value.var="value_"',
             ',\n      fill=NA_real_, sep=" ")')
}

datasets <-
  if (exists('datasets')) datasets else
    importDataList() %>%
  as.data.table() %>%
  .[,.(Code,`Dataset name`)] %>%
  unique() %>%
  with(codeWithLabelInNames(Code,`Dataset name`))

# Application
shinyApp(
  ui = fluidPage(
    tags$head(tags$style(HTML('* {font-weight: bold; font-family: monospace};'))),
    rclipboardSetup(),
    titlePanel("R code generator for a dataset import from Eurostat"),
    p("Shiny app for rapid generation of an autocommented code based on the `eurodata` package"),
    fluidRow(
      column(6,
             selectInput("selected_ds",
                         label=h3("Select dataset"), 
                         choices=c('<none>', datasets),
                         width='100%'),
             uiOutput('dim_selection_ui')
      ),
      column(6,
             verbatimTextOutput("value"),
             uiOutput("clip")
      ))),
  server = function(input, output) {
    metadata <- reactive(
      input$selected_ds %>% 
        {`if`(.!='<none>',
              describe(.) %>% 
                .[, only_one := length(Dim_val)==1, by=`Dim_name`] %>% 
                # Corrections below due to changes between old and new Eurostat metadata:
                .[, Dim_name := Dim_name %>% ifelse(.=='time','TIME_PERIOD',.)] %>% 
                .[, Dim_val := Dim_val  %>%
                    ifelse(Dim_name=='TIME_PERIOD' & grepl('^....M..$',Dim_val),
                           sub('M',"-",Dim_val,fixed=TRUE),
                           .)] %>%
                .[, Dim_val := Dim_val  %>%
                    ifelse(Dim_name=='TIME_PERIOD' & grepl('^....Q..$',Dim_val),
                           sub('Q',"-Q",Dim_val,fixed=TRUE),
                           .)]
              )}
    )
    dims <- reactive({
      md <- metadata()
      if (is.data.table(md))
        md %>%
        .[,.(Dim_name,Dim_name_label,only_one)] %>% 
        unique() %>% 
        split(seq_len(nrow(.)))
    })
    output$dim_selection_ui <-
      renderUI({
        di <- dims()
        md <- metadata()
        if (!is.null(di))
          di %>%
          lapply(function(dt) {
            choices <-
              md[Dim_name==dt$Dim_name] %>% 
              {`if`(dt$Dim_name=='TIME_PERIOD',rev(.$Dim_val),
                    codeWithLabelInNames(.$Dim_val,.$Dim_val_label))}
            selectInput(paste0('selected_',dt$Dim_name),
                        label=h4(HTML(paste(ifelse(dt$only_one,"&#9888; Only 1 option available in<br>","Select")),
                                      codeWithLabel(dt$Dim_name,dt$Dim_name_label))), 
                        choices=choices,
                        selected = if (dt$only_one) choices[1],
                        multiple=TRUE,
                        width='100%')
          })
      })
    Rcode <-
      reactive({
        di <- dims()
        md <- metadata()
        if (!is.null(di) && !is.null(md))
          di %>%
          rbindlist() %>% 
          .$Dim_name %>%
          lapply(. %>%
                   data.table(Dim_name=.,
                              Dim_val=input[[paste0('selected_',.)]] %>%
                                `if`(is.null(.),"",.))) %>%
          rbindlist() %>% 
          merge(md %>% print, by=c('Dim_name','Dim_val')) %>% 
          describe_dt_to_Rcode() %>% 
          paste0('# Code generated on ',Sys.time(),', ',Sys.timezone(),'\n\n',
                 'library(magrittr)\nlibrary(data.table)\nlibrary(eurodata)\n\n',
                 'dt__',input$selected_ds,' <-\n',
                 'importData("',input$selected_ds,'") %>% # ',
                 names(datasets)[datasets==input$selected_ds] %>%
                   sub('\\[.+\\] (.+)','\\1',.),'\n',
                 'as.data.table() %>%\n',
                 '.[!is.na(value_)] %>%\n',
                 '.[, flags_ := NULL] %>% # flags not needed\n',
                 '.[\n',.,'] %>%\n',
                 md[(only_one),Dim_name] %>% unique() %>% 
                   {`if`(length(.)>0,
                         paste0('.[, c(',paste0('"',.,'"',collapse=","),
                                ') := NULL] %>% # not needed since only single options in each\n'),
                         "")},
                 dcastCode(md))
      })
    output$value <-
      reactive(Rcode())
    output$clip <- renderUI({
      rc <- Rcode()
      if (!is.null(rc))
        rclipButton(
          inputId = "clipbtn",
          label = "Copy to clipboard",
          clipText = rc, 
          icon = icon("clipboard")
        )
    })
  }
) %>% 
  if (interactive()) runApp(.) else .
