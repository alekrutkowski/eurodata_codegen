library(magrittr)
library(eurodata)
library(data.table)
library(shiny)
library(shinybusy)
library(rclipboard)
library(xml2)
library(prismjs)

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

unneededColsCode <- function(metadata_dt) {
  metadata_dt %>%
    .[(only_one),Dim_name] %>%
    unique() %>% 
    {`if`(length(.)>0,
          paste0('.[, c(',paste0('"',.,'"',collapse=","),
                 ') := NULL] %>% # not needed since only single options in each\n'),
          "")}
}

datasets <-
  importDataList() %>%
  as.data.table() %>%
  .[,.(Code,`Dataset name`)] %>%
  unique() %>%
  with(codeWithLabelInNames(Code,`Dataset name`))

link <- function(txt, url)
  paste0('<a href="',url,'" target="_blank">',txt,'</a>')

xmlMetadataAddress <- function(ds_code)
  ds_code %>% 
  toupper(.) %>% 
  paste0('https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/datastructure/estat/',.)

urlStructure <- memoise::memoise(function(ds_code)
  ds_code %>% 
    xmlMetadataAddress() %>% 
    xml2::read_xml() %>% 
    xml2::as_list() %>% 
    {.$Structure$
        Structures$
        DataStructures$
        DataStructure$
        DataStructureComponents$
        DimensionList} %>% 
    sapply(function(x) attr(x$ConceptIdentity$Ref,'id')))

prism_style <-
  "code[class*=language-],pre[class*=language-]{color:#000;background:0 0;text-shadow:0 1px #fff;font-family:Consolas,Monaco,'Andale Mono','Ubuntu Mono',monospace;font-size:1em;text-align:left;white-space:pre;word-spacing:normal;word-break:normal;word-wrap:normal;line-height:1.5;-moz-tab-size:4;-o-tab-size:4;tab-size:4;-webkit-hyphens:none;-moz-hyphens:none;-ms-hyphens:none;hyphens:none}code[class*=language-] ::-moz-selection,code[class*=language-]::-moz-selection,pre[class*=language-] ::-moz-selection,pre[class*=language-]::-moz-selection{text-shadow:none;background:#b3d4fc}code[class*=language-] ::selection,code[class*=language-]::selection,pre[class*=language-] ::selection,pre[class*=language-]::selection{text-shadow:none;background:#b3d4fc}@media print{code[class*=language-],pre[class*=language-]{text-shadow:none}}pre[class*=language-]{padding:1em;margin:.5em 0;overflow:auto}:not(pre)>code[class*=language-],pre[class*=language-]{background:#f5f2f0}:not(pre)>code[class*=language-]{padding:.1em;border-radius:.3em;white-space:normal}.token.cdata,.token.comment,.token.doctype,.token.prolog{color:#708090}.token.punctuation{color:#999}.token.namespace{opacity:.7}.token.boolean,.token.constant,.token.deleted,.token.number,.token.property,.token.symbol,.token.tag{color:#905}.token.attr-name,.token.builtin,.token.char,.token.inserted,.token.selector,.token.string{color:#690}.language-css .token.string,.style .token.string,.token.entity,.token.operator,.token.url{color:#9a6e3a;background:hsla(0,0%,100%,.5)}.token.atrule,.token.attr-value,.token.keyword{color:#07a}.token.class-name,.token.function{color:#dd4a68}.token.important,.token.regex,.token.variable{color:#e90}.token.bold,.token.important{font-weight:700}.token.italic{font-style:italic}.token.entity{cursor:help}"

# Application
shinyApp(
  ui = fluidPage(
    tags$head(tags$style(HTML('* {font-weight: bold; font-family: monospace};',
                              prism_style))),
    rclipboardSetup(),
    add_busy_spinner(spin="fading-circle", position='full-page',
                     height='100px', width='100px'),
    HTML('<input type="text" id="client_time" name="client_time" style="display: none;"> '),
    tags$script('$(function() {
    var time_now = new Date()
    $("input#client_time").val(Intl.DateTimeFormat("en-GB", { dateStyle: "full", timeStyle: "long" }).format(time_now))});'),
    titlePanel(HTML(paste0(link('R','https://www.r-project.org'),
                           ' code generator for a dataset import from ',
                           link('Eurostat',
                                'https://ec.europa.eu/eurostat/databrowser/explore/all/all_themes'))),
               windowTitle='eurodata_codegen'),
    p(HTML(paste0(link('Shiny','https://shiny.rstudio.com'),
                  ' app for rapid generation of an autocommented code based on the ',
                  link('eurodata','https://CRAN.R-project.org/package=eurodata'),
                  ' package',
                  ' &#9632; ',
                  link('Source code of the app','https://github.com/alekrutkowski/eurodata_codegen')))),
    fluidRow(
      column(6,
             selectInput("selected_ds",
                         label=h3("Select dataset"), 
                         choices=c('<none>', datasets),
                         width='100%'),
             uiOutput('dim_selection_ui')
      ),
      column(6,
             uiOutput("clip"),
             br(),
             htmlOutput("value")
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
    AltRcode <- reactive({
      di <- dims()
      md <- metadata()
      input$selected_ds %>% 
        {`if`(.!='<none>' & !is.null(md) & !is.null(di),
              paste0('\n\n',
                     '# ___________________________________________________________________________\n',
                     '# Alternative approach with smaller download if few dimension values selected\n',
                     '# or you don\'t need rules-based filtering of datatset observations:\n',
                     'library(magrittr)\nlibrary(data.table)\nlibrary(eurodata)\n',
                     'dt__',input$selected_ds,' <-\n',
                     {`if`(is.data.table(md),
                           md %>% 
                             .[,.(Dim_name,Dim_name_label,Dim_val,Dim_val_label)] %>%
                             unique() %>%
                             {dt <- (.)
                             lapply(unique(dt$Dim_name),
                                    function(x) {
                                      selected_vals <- input[[paste0('selected_',x)]]
                                      if (!is.null(selected_vals) && x!='TIME_PERIOD')
                                        dt[Dim_name==x & Dim_val %in% selected_vals]
                                    })} %>%
                             rbindlist() %>% 
                             {if (nrow(.)==0) "" else 
                               merge(.,unique(md[,.(Dim_name,only_one)])) %>% 
                                 {paste0("# ",.$Dim_name,'=',.$Dim_val,
                                         ' -- ',
                                         .$Dim_name_label,' = ',.$Dim_val_label,
                                         ifelse(.$only_one,
                                                ' # THE ONLY OPTION AVAILABLE', ""),
                                         '\n') %>%
                                     paste(collapse="") %>% 
                                     paste0('## Meaning of the codes in `filters` below:\n',.)}},
                           "")},
                     di %>%
                       rbindlist() %>% 
                       .$Dim_name %>%
                       sapply(function(x)
                         input[[paste0('selected_',x)]] %>% 
                           paste0('"',.,'"',collapse=",") %>% 
                           paste0(x,' = c(',.,')')) %>% 
                       paste(collapse=',\n                          ') %>% 
                       paste0('           filters = list(',.,')') %>% 
                       paste0('importData("',input$selected_ds,'", # ',
                              names(datasets)[datasets==input$selected_ds] %>%
                                sub('\\[.+\\] (.+)','\\1',.),'\n',
                              .,') %>%\n'),
                     'as.data.table() %>%\n',
                     '.[!is.na(value_)] %>%\n',
                     '.[, freq := NULL] %>% # probably not needed\n',
                     '.[, value_ := as.numeric(value_)] %>%\n',
                     '.[, flags_ := NULL] %>% # flags not needed\n',
                     unneededColsCode(md),
                     dcastCode(md))
        )}
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
          merge(md, by=c('Dim_name','Dim_val')) %>% 
          describe_dt_to_Rcode() %>% 
          paste0('# Code generated on ',input$client_time,'\n\n',
                 'library(magrittr)\nlibrary(data.table)\nlibrary(eurodata)\n',
                 'dt__',input$selected_ds,' <-\n',
                 'importData("',input$selected_ds,'") %>% # ',
                 names(datasets)[datasets==input$selected_ds] %>%
                   sub('\\[.+\\] (.+)','\\1',.),'\n',
                 'as.data.table() %>%\n',
                 '.[!is.na(value_)] %>%\n',
                 '.[, flags_ := NULL] %>% # flags not needed\n',
                 '.[\n',.,'] %>%\n',
                 unneededColsCode(md),
                 dcastCode(md),
                 AltRcode()) %>% 
          list(txt=.,
               html=prismjs::prism_highlight_text(.,language='r') %>% 
                 paste0('<pre class="shiny-text-output" style="background-color:white;">',.,'</pre>'))
      })
    output$value <-
      reactive(Rcode()$html)
    output$clip <- renderUI({
      rc <- Rcode()$txt
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
