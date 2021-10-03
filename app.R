library(shiny)
library(DT)
library(tidyverse)
library(googlesheets4)
library(gt)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)



# allow read public sheets
gs4_deauth()


# function to create hyperlink in a GT table
make_hyperlink = function(myurl) {
  paste('<a href="',myurl,'">',"link",'</a>')
}


# shiny app

shinyApp(
  
  
  
  ui = navbarPage("jouRnal club",
                  
                  theme = shinytheme("flatly"),
                  
                  tabPanel("App",
                           
                           
                           
                           fluidRow(
                             style = 'margin: 5px',
                             column(12, 
                                    tags$p(HTML(paste0("This app presents only the most", tags$b(" CRITICAL"), 
                                                       " information from reviewed journal articles.", 
                                                       tags$br(), 
                                                       "Select ", 
                                                       tags$b('keywords ', 
                                                              #style = 'color:#FD3D00;'
                                                              ), 
                                                       "and ", 
                                                       tags$b('year of publication ' 
                                                              #, style = 'color:#FD3D00;'
                                                              ), 
                                                       "to search for relevant articles."
                                    )), style = "font-size:20px;"), 
                                    
                                    # h4("This app presents only the most CRITICAL information from reviewed journal articles."), 
                                    # h4("Select 'keywords' and 'year of publication' to search for relevant articles."), 
                                    tags$br()
                             )
                           ),
                           
                           sidebarLayout(
                             sidebarPanel(
                               
                               fluidRow(
                                 style = 'margin: 5px;',
                                 column(12, 
                                        textInput("gs_url", 
                                                  label = "Enter URL for your google sheet here (must be publicly available). An example table is loaded by default.", 
                                                  value = "https://docs.google.com/spreadsheets/d/127sU3zM8gA5TnLq7MJUXS-2bGzkG1i5lBeA2ok56bNM/edit?usp=sharing", 
                                                  width = '100%'
                                        )
                                 )
                               ),
                               tags$br(), 
                               
                               fluidRow(
                                 style='margin: 5px;',
                                 column(12,  
                                        
                                        
                                        uiOutput("key_choices")
                                 )
                               ),
                               
                               fluidRow(
                                 style='margin: 5px;', 
                                 column(12, 
                                        
                                        uiOutput("year_choices")
                                 )
                               ), width = 3
                             ),
                             
                             mainPanel(
                               
                               
                               uiOutput("tables")
                             )
                           )
                  ),
                  tabPanel("About", 
                           tags$p(HTML(paste0(br(),
                                              "<b>Eric Bergh, M.D.</b> is an OB/GYN, Maternal-Fetal Medicine Specialist",
                                              " in Houston, TX with interests in both fetal diseases and computer/data science.",
                                              tags$br(),
                                              "This web app was designed using the R programming language, {gt}, {googlesheets4} and the {shiny} web framework.", 
                                              tags$br(), 
                                              "Please do not hesitate to contact ",
                                              tags$a(href = "https://twitter.com/ericberghMD", "@ericberghMD "),
                                              "with any questions/suggestions.", 
                                              tags$br(), 
                                              "The code for this app can be found ",
                                              tags$a(href = "https://github.com/berghe01/allo_wheel", "here. "), 
                                              tags$br(), 
                                              tags$br(), 
                                              "This app was created for students, researchers and medical professionals and is NOT intended to be used as medical advice.",
                                              tags$br(), 
                                              "The purpose is to help identify and recall useful stats and measures from important articles of interest without having to read through the entire article/abstract again.",
                                              tags$br(),
                                              "I personally find this useful for patient counseling, and writing detailed and evidence-based reports. ",
                                              tags$br(),
                                              "If you are interested in curating your own list of articles, simply follow the directions below to create a table in google sheets.",
                                              tags$br(),
                                              "Then use the URL from your table to pull your own curated data into the app.", 
                                              tags$br()
                           ))), 
                           tags$br(), 
                           
                           tabsetPanel(
                             
                             tabPanel("Example Google Sheet",
                                      tags$br(), 
                                      tags$p(HTML(paste0(
                                        "Below is an ", tags$a(href="https://docs.google.com/spreadsheets/d/127sU3zM8gA5TnLq7MJUXS-2bGzkG1i5lBeA2ok56bNM/edit?usp=sharing", "example google sheet ", target = "_blank"),  "which can be used as a reference to create your own jouRnal club. "))),
                                      tags$li(HTML(paste0("Data is grouped by ", tags$b("pmid"), ". Columns in ", tags$b("purple", style = 'color:#DC96CB;'),  " repeat for every row within a group. "))),
                                      tags$li(HTML(paste0("Each row represents a study ", tags$em("arm.")))), 
                                      tags$li(HTML(paste0("Within each ", tags$em("pmid"), " group, a ", tags$b("measure"), " repeats for each study", tags$em("arm"), "."))),
                                      tags$li(HTML(paste0("Multiple ", tags$em("measures"), " may be added to populate the table."))), 
                                      tags$li(HTML(paste0("Furthermore, ", tags$em("footnotes"), " may be added to each ", tags$em("measure"), ". Keep in mind, ", tags$em("footnotes"), " should be repeated on rows within each ", tags$em("measure.")))),
                                      tags$br(),
                                      imageOutput("example_sheet"), 
                                      tags$br()
                             ),
                             
                             tabPanel("Permissions",
                                      tags$br(), 
                                      tags$p(HTML(paste0(
                                        "In order for the app to read your google sheet, you must set permissions/access to public (i.e. 'Anyone with the link')", 
                                        tags$br(), 
                                        "Enter this link into the previous {App} page to sync with your google sheet."))),
                                      tags$br(), 
                                      imageOutput("example_share"), 
                                      tags$br(), 
                                      tags$br()
                             )
                             
                             
                           )
                  )
  ), 
  
  server = function(input, output, session){
    
    output$example_sheet <- renderImage({
      
      list(src = 'www/example.png', align = 'center')
      
    }, deleteFile = FALSE)
    
    
    output$example_share <- renderImage({
      
      list(src = 'www/share.png', align = 'center')
      
    }, deleteFile = FALSE)
    
    
    raw <- eventReactive(input$gs_url, {
      
      req(input$gs_url)
      
      read_sheet(input$gs_url, col_types = "c")
      
    })
    
    
    keys <- reactive({
      
      raw() %>%
        select(keywords) %>%
        tidyr::separate_rows(keywords, sep = ", ") %>%
        distinct() %>%
        arrange(keywords)
      
    })
    
    year_min <- reactive({
      min(as.numeric(raw()$year))
    })
    
    year_max <- reactive({
      max(as.numeric(raw()$year))
    })
    
    
    
    
    output$year_choices <- renderUI({
      
      numericRangeInput(
        "date_range",
        "Select publication range",
        value = c(year_min(), year_max()),
        width = '450px',
        separator = " to "
      )
      
    })
    
    output$key_choices <-  renderUI({
      
      
      
      selectizeInput("pick_keys", 
                     "Select keywords",
                     width = '450px',
                     choices = keys(), 
                     selected = NULL, 
                     multiple = TRUE,
                     options = NULL)
    })
    
    
    search_keys <- reactive({
      
      paste(input$pick_keys, collapse = "|")
      
    })
    
    mylist <- eventReactive(
      c(input$pick_keys, input$date_range, input$gs_url), {
        
        
        #require(input$date_range & input$pick_keys)
        
        validate(
          need(length(input$pick_keys) > 0, "Select at least 1 keyword")
        )
        
        df <- raw() %>%
          select(arm, measure, value, statistic, footnote, keywords, comment, title, year, citation, pmid) %>%
          filter(str_detect(keywords, as.character(search_keys()))) %>%
          filter(year >= input$date_range[1] & year <= input$date_range[2]) %>%
          mutate(year = as.numeric(year)) %>%
          pivot_wider(
            names_from = arm, 
            names_prefix = "Arms___",
            values_from = value) %>%
          filter(str_detect(keywords, as.character(search_keys()))) %>%
          select(measure, starts_with("arm"), statistic, everything()) %>%
          filter(year >= input$date_range[1] & year <= input$date_range[2]) %>%
          mutate(year = as.numeric(year)) %>%
          mutate(link = paste0("https://pubmed.ncbi.nlm.nih.gov/27083761/", pmid)
                 #link = glue::glue("[web]({link})")#,
                 # link = sapply(link, toString),
                 # link = map(link, gt::md)
          ) %>%
          group_by(pmid) %>%
          mutate(link = replace(link, row_number() > 1, NA)) %>%
          group_split(title) 
        
        # some of these are empty, need to pull out then reattach as they would get removed in the next step
        
        pull_footnotes <- df %>%
          map(~select_if(., str_detect(names(.), "footnote|comment")))
        
        # remove columns which are completly empty
        
        clean_df <- df %>% 
          map(~select_if(., ~!all(., is.na(.))))
        
        df <- mapply(cbind, clean_df, pull_footnotes, SIMPLIFY = FALSE)
        
        df <- df %>%
          map(~subset(., select = which(!duplicated(names(.)))))
        
        
        return(df)
        
        
      }, ignoreNULL = TRUE)
    
    
    observeEvent(mylist(), {
      
      
      
      # Insert the right number of table output objects into the web page
      
      
      output$tables <-  renderUI({
        
        # list of output objects
        
        table_output_list <- list()
        
        for(i in 1:length(mylist())){
          
          tablename <- paste("table", i, sep="")
          
          
          table_output_list[i] <- tagList(
            tabPanel(
              title = HTML(paste(
                #unique(mylist()[[i]]$pmid), 
                stringr::word(unique(mylist()[[i]]$citation),1, sep = fixed(',')),
                unique(mylist()[[i]]$year),
                sep = "</br>")
              ), 
              gt_output(tablename), 
              tags$br(),
              tags$br(),
              class = "span7"
            )
          )
          
          
        }
        
        do.call(tabsetPanel, table_output_list)
      })
      
      
      
      
      
      
      for (i in 1:length(mylist())){
        
        
        local({
          my_i <- i
          
          #tablename <- paste0("table", my_i)
          
          output[[paste0("table", my_i)]] <- render_gt({
            
            
            
            ft_notes <- list()
            
            for (k in mylist()[[my_i]]$footnote){
              
              if(!is.na(k)){
                z = which(mylist()[[my_i]]$footnote == k)
                j <- paste("tab_footnote(", 
                           "footnote = '", 
                           mylist()[[my_i]]$footnote[z], "', ", 
                           "locations = ",
                           "cells_body(columns = measure, ", 
                           "rows = measure == '", mylist()[[my_i]]$measure[z], "'))", 
                           sep = "")
                
                ft_notes[[z]] <- j
              } 
              
              
            }
            
            
            ft_notes <- paste(compact(ft_notes), collapse = " %>% ")
            
            if(is.na(unique(mylist()[[my_i]]$comment))){
              
              add_header <- " tab_header(
                                                                                title = unique(mylist()[[my_i]]$title)
                                                                      ) %>%
                                                                      cols_hide(
                                                                                columns = c(keywords, title, year, citation, pmid, footnote, link, comment)
                                                                      )"
            } else {
              
              add_header <- " tab_header(
                                                                                title = unique(mylist()[[my_i]]$title),
                                                                                subtitle = paste('Comment: ', unique(mylist()[[my_i]]$comment), sep = '')
                                                                      ) %>%
                                                                      cols_hide(
                                                                                columns = c(keywords, title, year, citation, pmid, footnote, link, comment)
                                                                      ) "
              
            }
            
            gt_table <- "mylist()[[my_i]] %>%
                                                                      
                                                                      gt() %>%
                                                                     
                                                                      tab_source_note(
                                                                                 source_note = 
                                                                                          html(
                                                                                           htmltools::tags$a(
                                                                                                
                                                                                                    href = unique(mylist()[[my_i]]$link)[1], 
                                                                                                    target = '_blank',
                                                                                                    'PubMed'
                                                                                                     
                                                                                           ) %>% 
                                                                                           as.character()
                                                                                          )
                                                                                
                                                                       ) %>% 
                                                                      
                                                                      tab_source_note(
                                                                                 source_note = unique(mylist()[[my_i]]$citation)[1]
                                                                                          
                                                                       ) %>% 

                                                                      tab_spanner_delim(
                                                                                delim = '___'
                                                                                
                                                                      ) %>%
                                                                      fmt (
                                                                                columns = 'link',
                                                                                fns = make_hyperlink
                                                                      ) %>%
                                                                      
                                                                      fmt_missing(
                                                                           columns = everything(), 
                                                                           rows = everything(), 
                                                                           missing_text = ''
                                                                      ) %>%
                                                                      cols_label(
                                                                                measure = 'Measure',
                                                                                statistic = 'Statistic'
                                                                      ) %>%
                                                                      opt_row_striping() %>%
                                                                      tab_options(
                                                                                table.width = px(1000), 
                                                                                container.width = px(1000),
                                                                                table.border.top.color = '#a9b6c3', 
                                                                                table.border.top.width = 3, 
                                                                                table.border.bottom.color = '#a9b6c3', 
                                                                                table.border.bottom.width = 3, 
                                                                                table.border.left.color = '#a9b6c3', 
                                                                                table.border.left.width = 3, 
                                                                                table.border.right.color = '#a9b6c3', 
                                                                                table.border.right.width = 3, 
                                                                                table_body.border.top.width = 3,
                                                                                table_body.border.bottom.width = 3, 
                                                                                row.striping.background_color = '#f9f9fb'
                                                                                
                                                                      ) %>%
                                                                      tab_style(
                                                                                style = list(
                                                                                          cell_fill(color = '#B7EEE3'),
                                                                                          cell_text(weight = 'bold') 
                                                                                          
                                                                                ),
                                                                                locations = cells_column_spanners(
                                                                                          spanners = everything()
                                                                                )
                                                                      ) %>%
                                                                      tab_style(
                                                                      
                                                                                style = list(
                                                                                          cell_text(weight = 'bold')
                                                                                ), 
                                                                                locations = cells_column_labels(
                                                                                          columns = everything()
                                                                                )
                                                                      
                                                                      ) %>%
                                                                      
                                                                      cols_align(
                                                                                align = c('center'), 
                                                                                columns = colnames(mylist()[[my_i]])[2]:colnames(mylist()[[my_i]])[dim(mylist()[[my_i]])[2]-9]
                                                                      ) %>%
                                                            
                                                                      cols_width(
                                                                                link ~ px(10),
                                                                                everything() ~ px(20)
                                                                      )"
            
            
            gt_table <- paste(gt_table, add_header, sep = " %>% ")
            
            if(nchar(ft_notes) > 1) {
              
              final_table <- paste(gt_table,  
                                   ft_notes, sep = " %>% ")
            }else{
              final_table <- gt_table
            }
            
            eval(parse(text = final_table))
            
            
          })
          
          
          
        })
      }
      
      
      
    })
    
    
  }
)

