library(shiny)
library(tidyverse)
library(googlesheets4)
library(gt)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)



# allow read public sheets
gs4_deauth()


# shiny app
shinyApp(
  
  
  
  ui = navbarPage("jouRnal club",
                  
                  theme = shinytheme("flatly"),
                  
                  tabPanel("App",
                           
                           
                           
                           fluidRow(
                             style = 'margin: 5px',
                             column(12, 
                                    tags$p(
                                      HTML(
                                        paste0(
                                          "This app presents only the most", tags$b(" CRITICAL"), 
                                          " information from reviewed journal articles in easy-to-read ", tags$a(href='https://gt.rstudio.com/', "{gt}", target = "_blank"), " tables.", 
                                          tags$br(), 
                                          "Select ", 
                                          tags$b('keywords ', 
                                          ), 
                                          "and ", 
                                          tags$b('year of publication ' 
                                          ), 
                                          "to search for relevant articles."
                                        )
                                      ), style = "font-size:20px;"), 
                                    
                                    
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
                  tabPanel("How to", 
                           tags$p(
                             HTML(
                               paste0(
                                 "This app was created for students, researchers and medical professionals and is NOT intended to be used as medical advice.",
                                 tags$br(), 
                                 "The purpose is to help identify and recall useful stats and measures from important articles of interest without having to read through the entire article/abstract again.",
                                 tags$br(),
                                 "I personally find this useful for patient counseling, and writing detailed and evidence-based reports. ",
                                 tags$br(),
                                 "If you are interested in curating your own list of articles, simply follow the directions below to create a table in google sheets.",
                                 tags$br(),
                                 "Then, give public sharing access to your table, and copy the public 'share link' (see permissions section below) from your table to pull your own curated data into the app.", 
                                 tags$br()
                               )
                             )
                           ), 
                           tags$br(), 
                           
                           tabsetPanel(
                             
                             tabPanel("Example Google Sheet",
                                      tags$br(), 
                                      tags$p(
                                        HTML(
                                          paste0(
                                            "Below is an ", tags$a(href="https://docs.google.com/spreadsheets/d/127sU3zM8gA5TnLq7MJUXS-2bGzkG1i5lBeA2ok56bNM/edit?usp=sharing", "example google sheet ", target = "_blank"),  "which can be used as a reference to create your own ", tags$b("jouRnal club"), "."))),
                                      tags$li(HTML(paste0("Data is grouped by ", tags$b("pmid"), ". Columns in ", tags$b("purple", style = 'color:#DC96CB;'),  " repeat for every row within a group. "))),
                                      tags$li(HTML(paste0("Each row represents a study ", tags$b("arm.")))), 
                                      tags$li(HTML(paste0("Within each ", tags$b("pmid"), " group, a ", tags$b("measure"), " repeats for each study", tags$b("arm"), "."))),
                                      tags$li(HTML(paste0("Multiple ", tags$b("measures"), " may be added to populate the table."))), 
                                      tags$li(HTML(paste0("Furthermore, ", tags$b("footnotes"), " may be added to each ", tags$b("measure"), ". Keep in mind, ", tags$b("footnotes"), " should be repeated on rows within each ", tags$b("measure.")))),
                                      tags$li(HTML(paste0("Lastly, ", tags$b("keywords"), " should be separated by a comma ' , ' and should repeat on each row for the group"))),
                                      tags$br(),
                                      imageOutput("example_sheet"), 
                                      tags$br()
                             ),
                             
                             tabPanel("Permissions",
                                      tags$br(), 
                                      tags$p(
                                        HTML(
                                          paste0(
                                            "In order for the app to read your google sheet, you must set permissions/access to public (i.e. 'Anyone with the link')", 
                                            tags$br(), 
                                            "Enter this link into the previous {App} page to sync with your google sheet."))),
                                      tags$br(), 
                                      imageOutput("example_share"), 
                                      tags$br(), 
                                      tags$br()
                             )
                             
                             
                           )
                  ), 
                  tabPanel("About", 
                           tags$p(
                             HTML(
                               paste0(
                                 "<b>Eric Bergh, M.D.</b> is an OB/GYN, Maternal-Fetal Medicine and Fetal Intervention Specialist",
                                 " in Houston, TX with interests in both fetal diseases and computer/data science.",
                                 tags$br(),
                                 "This web app was designed using the ", 
                                 tags$a(href = "https://www.r-project.org/", "R", target = "_blank"),  " programming language, ",
                                 tags$a(href = "https://gt.rstudio.com/", "{gt}", target = "_blank"),", ", 
                                 tags$a(href = "https://googlesheets4.tidyverse.org/", "{googlesheets4}", target = "_blank"), " packages and the ", 
                                 tags$a(href = "https://shiny.rstudio.com/", "{shiny}", target = "_blank"), " web framework.", 
                                 tags$br(), 
                                 "Please do not hesitate to contact ",
                                 tags$a(href = "https://twitter.com/ericberghMD", "@ericberghMD ", target = "_blank"),
                                 "with any questions/suggestions.", 
                                 tags$br(), 
                                 "The code for this app can be found ",
                                 tags$a(href = "https://github.com/berghe01/jouRnal_club/blob/main/app.R", "here. ", target = "_blank"), 
                                 tags$br() 
                               )
                             )
                           )
                  )
  ), 
  
  server = function(input, output, session){
    
    
    #render image of example google sheet
    output$example_sheet <- renderImage({
      
      list(src = 'www/example.png', align = 'center')
      
    }, deleteFile = FALSE)
    
    # render image of permissions
    output$example_share <- renderImage({
      
      list(src = 'www/share.png', align = 'center')
      
    }, deleteFile = FALSE)
    
    
    
    # import data from google sheet
    raw <- eventReactive(input$gs_url, {
      
      req(input$gs_url)
      
      read_sheet(input$gs_url, # url is personalized to google sheet of the individual accessing site
                 col_types = "c")
      
    })
    
    # make vector of unique keywords in the database, will populate the selectize input
    
    keys <- reactive({
      
      keys <- raw() %>%
        select(keywords) %>%
        tidyr::separate_rows(keywords, sep = ", ") %>%
        distinct() %>%
        arrange(keywords)
      
    })
    
    # extract min and max year in the database to filter with
    
    year_min <- reactive({
      min(as.numeric(raw()$year))
    })
    
    year_max <- reactive({
      max(as.numeric(raw()$year))
    })
    
    
    # create UI for year selection with dynamic range of years based on data that is passed to raw()
    
    output$year_choices <- renderUI({
      
      numericRangeInput(
        "date_range",
        "Select publication range",
        value = c(year_min(), year_max()),
        width = '450px',
        separator = " to "
      )
      
    })
    
    # create UI that has dynamic range of keyword choices based on data passed to raw()
    
    output$key_choices <-  renderUI({
      
      selectizeInput("pick_keys", 
                     "Select keywords",
                     width = '450px',
                     choices = keys()$keywords, 
                     selected = NULL, 
                     multiple = TRUE,
                     options = NULL)
    })
    
    # create character string with | logic to pass to str_detect (used to filter dataframe)
    
    search_keys <- reactive({
      
      paste(input$pick_keys, collapse = "|")
      
    })
    
    # generate list of data frames from the original df
    # will first filter for possible studies that meet keywords and years
    # then will group data
    # then will make each pmid group into its own dataframe which will populate mylist()
    
    mylist <- eventReactive(
      c(input$pick_keys, input$date_range, input$gs_url), {
        
        
        #require(input$date_range & input$pick_keys)
        
        validate(
          need(length(input$pick_keys) > 0, "Select at least 1 keyword")
        )
        
        df <- raw() %>%
          select(measure, arm, value, statistic, footnote, keywords, comment, title, year, citation, pmid) %>%
          filter(str_detect(keywords, as.character(search_keys()))) %>%
          filter(year >= input$date_range[1] & year <= input$date_range[2]) %>%
          mutate(year = as.numeric(year)) %>%
          pivot_wider(
            names_from = arm, 
            names_prefix = "Arms___",
            values_from = value) %>%
          select(measure, starts_with("arm"), statistic, everything()) %>%
          mutate(link = paste0("https://pubmed.ncbi.nlm.nih.gov/", pmid)) %>%
          mutate(footnote = case_when(
            is.na(footnote) ~ NA_character_, 
            TRUE ~ paste(footnote, row_number(), sep = "______"))
          )%>% # append unique number to footnotes, in case identical footnotes are put for different measures
          group_by(pmid) %>%
          mutate(link = replace(link, row_number() > 1, NA)) %>%
          group_split(title) 
        
        # some of these columns are empty, need to pull out then reattach as they would get removed in the next step
        # can't get rid of entirely, otherwise code for GT table will not work, even if emptly footnote or comment column, ok to pass to GT
        
        pull_footnotes <- df %>%
          map(~select_if(., str_detect(names(.), "footnote|comment|statistic")))
        
        # remove all other columns which are completly empty (specifically, the additional study arm columns)
        
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
      
      
      
      
      # create GT table for each unique PMID element/group that is within mylist()
      
      for (i in 1:length(mylist())){
        
        
        local({
          my_i <- i
          
          
          output[[paste0("table", my_i)]] <- render_gt({
            
            
            
            ft_notes <- list()
            
            for (k in mylist()[[my_i]]$footnote){
              
              
              if(!is.na(k)){
                
                z = which(mylist()[[my_i]]$footnote == k)
                
                j <- paste("tab_footnote(", 
                           "footnote = '", 
                           gsub("______.*", "", mylist()[[my_i]]$footnote[z]), "', ", # pull the appended delimiter and row name off the footnote, prior to render
                           "locations = ",
                           "cells_body(columns = measure, ", 
                           "rows = measure == '", mylist()[[my_i]]$measure[z], "'))", 
                           sep = "")
                
                ft_notes[[z]] <- j
              } 
              
              
            }
            
            
            ft_notes <- paste(compact(ft_notes), collapse = " %>% ")
            
            if(is.na(unique(mylist()[[my_i]]$comment))){
              
              add_header <-  "tab_header(
                title = unique(mylist()[[my_i]]$title)
              ) %>%
                cols_hide(
                  columns = c(keywords, title, year, citation, pmid, footnote, link, comment)
                )"
            } else {
              
              add_header <- "tab_header(
                title = unique(mylist()[[my_i]]$title),
                subtitle = paste('Comment: ', unique(mylist()[[my_i]]$comment), sep = '')
              ) %>%
                cols_hide(
                  columns = c(keywords, title, year, citation, pmid, footnote, link, comment)
                )"
              
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
            
            # combind gt table and header
            
            gt_table <- paste(gt_table, add_header, sep = " %>% ")
            
            # if footnotes exist, append to table, otherwise ignore footnotes
            
            if(nchar(ft_notes) > 1) {
              
              final_table <- paste(gt_table,  
                                   ft_notes, sep = " %>% ")
            }else{
              final_table <- gt_table
            }
            
            # render final gt table
            
            eval(parse(text = final_table))
            
            
          })
          
          
          
        })
      }
      
      
      
    })
    
    
  }
)

