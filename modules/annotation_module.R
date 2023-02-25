annotationPlot <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width=12,div(
        span(textOutput(ns("current_code_label"),inline = TRUE),style = "font-size:2.5rem;color: black;center;center;"),
        style =
          "padding-top: 10px;
             padding-bottom: 5px;
             margin-top: 3px;
             display: flex;
             align-items: center;
             justify-content: space-evenly;"
      )),
      
      column(12,
             box(width=12,title=strong("Interactive Manhattan/Scatter Plot and Network",style="font-size: 2.0rem;"),solidHeader = F,status="warning",
                 fluidRow(
                   column(12,div(p("Click a point to annotate it. Drag region to highlight multiple points. Click or drag again to remove annotation.",style = "font-size: 2.0rem;color: black;"))),
                   # column(2,div(p("Hovered over:",style = "font-size:2.0rem;color: black;"))),
                   # column(6,span(textOutput(ns("plot_hoverinfo")),style = "font-size:2.0rem;color: black;")),
                   column(4,actionButton(ns("dump_annotations"), "Remove all annotations")),
                   column(12,div(style = "height:30px"))))),
      
      ## three tabs
      column(12,tabBox(width=12,
                       
                       ## 1st tab
                       tabPanel("VUMC vs MGB",
                                fluidRow(
                                  column(6,
                                         withSpinner(div(plotlyOutput(ns("plot1_vandy_mgh"),height="100%",
                                                                      click = ns("point_click"),
                                                                      hover = hoverOpts(id=ns("point_hover")),
                                                                      brush = brushOpts(id=ns("point_brush"))))),
                                         
                                         downloadButton(ns("manhattan1"))
                                  ),
                                  
                                  column(6,
                                         withSpinner(plotOutput(ns("plot2_vandy_mgh"),width='500px',height = "400px",
                                                                click = ns("point_click"),
                                                                hover = hoverOpts(id=ns("point_hover")),
                                                                brush = brushOpts(id=ns("point_brush")))),
                                         
                                         downloadButton(ns("scatter1"))
                                  )
                                ),
                                
                                column(12,div(style = "height:30px")),
                                
                                box(width=12,title=strong("Associationsubgraph: annotated points are colored"),solidHeader = F,status="warning",
                                    column(12,fluidRow(
                                      # div(p(strong("Associationsubgraph: annotated points are colored"),
                                      #               style = "font-size:2.0rem;")),
                                      column(12,div(actionButton(ns("update_subgraph_vandy_mgh"), "Update associationsubgraphs"),style="float:right"))
                                      ,
                                      column(12,div(style = "height:30px")),
                                      column(12,uiOutput(ns("spinner1")))
                                    ))
                                ),
                                
                                column(12,div(style = "height:30px")),
                                
                                box(width=12,title=strong("Annotated Points: points annotated in the plot will be displayed below"),solidHeader = F,status="warning",
                                    column(12,fluidRow(
                                      div(
                                        # p(strong("Annotated Points: points annotated in the plot will be displayed below"), style = "font-size: 2.0rem;"),
                                        dataTableOutput(ns("annotated_points_table1"))))
                                      # downloadButton(ns("download_table1")))
                                    ))
                       )
                       ,
                       tabPanel("VUMC vs UKB",
                                fluidRow(
                                  column(6,
                                         withSpinner(plotOutput(ns("plot1_vandy_ukbb"),width='600px',height = "400px",
                                                                click = ns("point_click"),
                                                                hover = hoverOpts(id=ns("point_hover")),
                                                                brush = brushOpts(id=ns("point_brush")))),
                                         
                                         downloadButton(ns("manhattan2"))
                                  ),
                                  column(6,
                                         withSpinner(plotOutput(ns("plot2_vandy_ukbb"),width='500px',height = "400px",
                                                                click = ns("point_click"),
                                                                hover = hoverOpts(id=ns("point_hover")),
                                                                brush = brushOpts(id=ns("point_brush")))),
                                         
                                         downloadButton(ns("scatter2"))
                                  )
                                ),
                                
                                column(12,div(style = "height:30px")),
                                
                                box(width=12,title=strong("Associationsubgraph: annotated points are colored"),solidHeader = F,status="warning",
                                    column(12,fluidRow(
                                      # div(p(strong("Associationsubgraph: annotated points are colored"),
                                      #               style = "font-size:2.0rem;")),
                                      column(12,div(actionButton(ns("update_subgraph_vandy_mgh"), "Update associationsubgraphs"),style="float:right"))
                                      ,
                                      column(12,div(style = "height:30px")),
                                      column(12,uiOutput(ns("spinner2")))
                                    ))
                                ),
                                
                                column(12,div(style = "height:30px")),
                                
                                box(width=12,title=strong("Annotated Points: points annotated in the plot will be displayed below"),solidHeader = F,status="warning",
                                    column(12,fluidRow(
                                      div(
                                        # p(strong("Annotated Points: points annotated in the plot will be displayed below"), style = "font-size: 2.0rem;"),
                                        dataTableOutput(ns("annotated_points_table2"))))
                                      # downloadButton(ns("download_table1")))
                                    ))
                       ),
                       
                       tabPanel("MGB vs UKBB",
                                fluidRow(
                                  column(6,
                                         withSpinner(plotOutput(ns("plot1_mgh_ukbb"),width='600px',height = "400px",
                                                                click = ns("point_click"),
                                                                hover = hoverOpts(id=ns("point_hover")),
                                                                brush = brushOpts(id=ns("point_brush")))),
                                         
                                         downloadButton(ns("manhattan3"))
                                  ),
                                  
                                  column(6,
                                         withSpinner(plotOutput(ns("plot2_mgh_ukbb"),width='500px',height = "400px",
                                                                click = ns("point_click"),
                                                                hover = hoverOpts(id=ns("point_hover")),
                                                                brush = brushOpts(id=ns("point_brush")))),
                                         
                                         downloadButton(ns("scatter3"))
                                  )
                                ),
                                
                                column(12,div(style = "height:30px")),
                                
                                box(width=12,title=strong("Associationsubgraph: annotated points are colored"),solidHeader = F,status="warning",
                                    column(12,fluidRow(
                                      # div(p(strong("Associationsubgraph: annotated points are colored"),
                                      #               style = "font-size:2.0rem;")),
                                      column(12,div(actionButton(ns("update_subgraph_vandy_mgh"), "Update associationsubgraphs"),style="float:right"))
                                      ,
                                      column(12,div(style = "height:30px")),
                                      column(12,uiOutput(ns("spinner3")))
                                    ))
                                ),
                                
                                box(width=12,title=strong("Annotated Points: points annotated in the plot will be displayed below"),solidHeader = F,status="warning",
                                    column(12,fluidRow(
                                      div(
                                        # p(strong("Annotated Points: points annotated in the plot will be displayed below"), style = "font-size: 2.0rem;"),
                                        dataTableOutput(ns("annotated_points_table3"))))
                                      # downloadButton(ns("download_table1")))
                                    ))
                       )
      )) ##tabbox
    ) ##fluidRow
  ) ##tagList
}

annotationPlotServer <- function(id, code_id, code_data, type, type_label,plot_fn1, plot_fn2, code_description) {
  moduleServer(
    id,
    function(input, output, session) {
      
      annotated_points <- reactiveVal(c(0)) #change into reactive Values
      
      #===============================================
      observeEvent(input$point_brush, {
        
        selected_point <- brushedPoints(code_data(), input$point_brush)$phecode
        old_selection <- annotated_points()
        already_selected <- selected_point %in% old_selection
        if(sum(already_selected)>=1){
          # Remove the point from the selection if it's been clicked again
          annotated_points(c(old_selection[!(old_selection %in% selected_point)],
                             selected_point))
          # c(old_selection[!(old_selection %in% selected_point)],
          #                   selected_point)
        } else {
          # otherwise add it to the selection
          annotated_points(c(old_selection, selected_point))
          # c(old_selection, selected_point)
        }
      })
      #===============================================
      
      observeEvent(input$dump_annotations, {
        annotated_points(c(0))
      })
      
      # toListen <- reactive({
      #   list(input$point_brush,input$update_subgraph)
      # })
      observeEvent(input$update_subgraph_vandy_mgh,{
        output$spinner1 = renderUI({
          withSpinner(r2d3::d3Output(session$ns("plot3_vandy_mgh"),width = "100%", height = "690px"))
        })
        output$plot3_vandy_mgh <- r2d3::renderD3({
          Sys.sleep(3)
          comorbidity_subnetwork(com_sim,isolate(annotated_points()),paste0(glue("{(type_label)}_vandy_mgh")),code_description)
        })
      })
      
      observeEvent(input$update_subgraph_vandy_ukbb,{
        output$spinner2 = renderUI({
          withSpinner(r2d3::d3Output(session$ns("plot3_vandy_ukbb"),width = "100%", height = "690px"))
        })
        output$plot3_vandy_ukbb <- r2d3::renderD3({
          Sys.sleep(3)
          comorbidity_subnetwork(com_sim,isolate(annotated_points()),paste0(glue("{(type_label)}_vandy_ukbb")),code_description)
        })
      })
      
      observeEvent(input$update_subgraph_mgh_ukbb,{
        output$spinner3 = renderUI({
          withSpinner(r2d3::d3Output(session$ns("plot3_mgh_ukbb"),width = "100%", height = "690px"))
        })
        output$plot3_mgh_ukbb <- r2d3::renderD3({
          Sys.sleep(3)
          comorbidity_subnetwork(com_sim,isolate(annotated_points()),paste0(glue("{(type_label)}_mgh_ukbb")),code_description)
        })
      })
      
      
      ##output of hover points information
      output$plot_hoverinfo <- renderText({
        hover_phecode = nearPoints(code_data(), input$point_hover)$phecode[1]
        hover_description = nearPoints(code_data(), input$point_hover)$description[1]
        if(!is.na(hover_phecode)) {
          paste0(hover_phecode,"-",hover_description)
        } else {""}
        
      })
      
      #vandy & mgh
      code_data_vandy_mgh = reactive({code_data() %>% drop_na(z_vandy_mgh)})
      output$plot1_vandy_mgh <- renderPlot({
        
        if(nrow(code_data_vandy_mgh())!=0){
          m_plot = function(){plot_fn1(
            code_id,
            mutate(code_data_vandy_mgh(), is_labeled = phecode %in% annotated_points()),
            type,
            type_label,
            "vandy_mgh",
            "yes"
          )}
        } else{
          m_plot = function(){plot_fn1(
            code_id,
            mutate(code_data_vandy_mgh(), is_labeled = phecode %in% annotated_points()),
            type,
            type_label,
            "vandy_mgh",
            "no"
          )}
        }
        ggsave("manhattan.png",m_plot())
        m_plot()
        
      })
      
      output$plot2_vandy_mgh <- renderPlot({
        
        if(nrow(code_data_vandy_mgh())!=0){
          s_plot = function(){plot_fn2(
            code_id,
            mutate(code_data_vandy_mgh(), is_labeled = phecode %in% annotated_points()),
            type,
            type_label,
            "vandy","mgh",
            "yes"
          )}
        } else{
          s_plot = function(){plot_fn2(
            code_id,
            mutate(code_data_vandy_mgh(), is_labeled = phecode %in% annotated_points()),
            type,
            type_label,
            "vandy","mgh",
            "no"
          )}
        }
        ggsave("scatter.png",s_plot())
        s_plot()
      })
      
      #vandy & ukbb
      code_data_vandy_ukbb = reactive({code_data() %>% drop_na(z_vandy_ukbb)})
      output$plot1_vandy_ukbb <- renderPlot({
        
        if(nrow(code_data_vandy_ukbb())!=0){
          m_plot = function(){plot_fn1(
            code_id,
            mutate(code_data_vandy_ukbb(), is_labeled = phecode %in% annotated_points()),
            type,
            type_label,
            "vandy_ukbb",
            "yes"
          )}
        } else{
          m_plot=function(){plot_fn1(
            code_id,
            mutate(code_data_vandy_ukbb(), is_labeled = phecode %in% annotated_points()),
            type,
            type_label,
            "vandy_ukbb",
            "no"
          )}
        }
        ggsave("manhattan.png",m_plot())
        m_plot()
      })
      
      output$plot2_vandy_ukbb <- renderPlot({
        
        if(nrow(code_data_vandy_ukbb())!=0){
          s_plot = function(){plot_fn2(
            code_id,
            mutate(code_data_vandy_ukbb(), is_labeled = phecode %in% annotated_points()),
            type,
            type_label,
            "vandy","ukbb",
            "yes"
          )}
        } else{
          s_plot = function(){plot_fn2(
            code_id,
            mutate(code_data_vandy_ukbb(), is_labeled = phecode %in% annotated_points()),
            type,
            type_label,
            "vandy","ukbb",
            "no"
          )}
        }
        ggsave("scatter.png",s_plot())
        s_plot()
      })
      
      #mgh & ukbb
      code_data_mgh_ukbb = reactive({code_data() %>% drop_na(z_mgh_ukbb)})
      output$plot1_mgh_ukbb <- renderPlot({
        
        if(nrow(code_data_mgh_ukbb())!=0){
          m_plot=function(){plot_fn1(
            code_id,
            mutate(code_data_mgh_ukbb(), is_labeled = phecode %in% annotated_points()),
            type,
            type_label,
            "mgh_ukbb",
            "yes"
          )}
        } else{
          m_plot=function(){plot_fn1(
            code_id,
            mutate(code_data_mgh_ukbb(), is_labeled = phecode %in% annotated_points()),
            type,
            type_label,
            "mgh_ukbb",
            "no"
          )}
        }
        ggsave("manhattan.png",m_plot())
        m_plot()
      })
      
      output$plot2_mgh_ukbb <- renderPlot({
        
        if(nrow(code_data_mgh_ukbb())!=0){
          s_plot = function(){plot_fn2(
            code_id,
            mutate(code_data_mgh_ukbb(), is_labeled = phecode %in% annotated_points()),
            type,
            type_label,
            "mgh","ukbb",
            "yes"
          )}
        } else{
          s_plot=function(){plot_fn2(
            code_id,
            mutate(code_data_mgh_ukbb(), is_labeled = phecode %in% annotated_points()),
            type,
            type_label,
            "mgh","ukbb",
            "no"
          )}
        }
        ggsave("scatter.png",s_plot())
        s_plot()
      })
      
      annotated_points_table1 <- renderDataTable(
        datatable(code_data_vandy_mgh() %>%
                    #filter(phecode == phecodes$phecode[start_index]) %>%
                    filter(phecode %in% annotated_points()) %>%
                    dplyr::select(phecode,description,category,glue("{(type_label)}_vandy"),glue("{(type_label)}_mgh"),
                                  glue("{(type_label)}_ukbb")),
                  options(list(pageLength=5)))
      )
      output$annotated_points_table1 = annotated_points_table1
      
      annotated_points_table2 <- renderDataTable(
        datatable(code_data_vandy_ukbb() %>%
                    #filter(phecode == phecodes$phecode[start_index]) %>%
                    filter(phecode %in% annotated_points()) %>%
                    dplyr::select(phecode,description,category,glue("{(type_label)}_vandy"),glue("{(type_label)}_mgh"),
                                  glue("{(type_label)}_ukbb")),
                  options(list(pageLength=5)))
      )
      output$annotated_points_table2 = annotated_points_table2
      
      annotated_points_table3 <- renderDataTable(
        datatable(code_data_mgh_ukbb() %>%
                    #filter(phecode == phecodes$phecode[start_index]) %>%
                    filter(phecode %in% annotated_points()) %>%
                    dplyr::select(phecode,description,category,glue("{(type_label)}_vandy"),glue("{(type_label)}_mgh"),
                                  glue("{(type_label)}_ukbb")),
                  options(list(pageLength=5))),
        extensions = 'Buttons',buttons = c('csv', 'excel')
      )
      output$annotated_points_table3 = annotated_points_table3
      
      output$current_code_label <- renderText(glue("Current selection: {code_id()}"))
      
      output$manhattan1 <- downloadHandler(
        filename = function() {
          "manhattan.png"
        },
        content = function(file) {
          file.copy("manhattan.png", file, overwrite=TRUE)
        }
      )
      
      output$scatter1 <- downloadHandler(
        filename = function() {
          "scatter.png"
        },
        content = function(file) {
          file.copy("scatter.png", file, overwrite=TRUE)
        }
      )
      
      output$manhattan2 <- downloadHandler(
        filename = function() {
          "manhattan.png"
        },
        content = function(file) {
          file.copy("manhattan.png", file, overwrite=TRUE)
        }
      )
      
      output$scatter2 <- downloadHandler(
        filename = function() {
          "scatter.png"
        },
        content = function(file) {
          file.copy("scatter.png", file, overwrite=TRUE)
        }
      )
      
      output$manhattan3 <- downloadHandler(
        filename = function() {
          "manhattan.png"
        },
        content = function(file) {
          file.copy("manhattan.png", file, overwrite=TRUE)
        }
      )
      
      output$scatter3 <- downloadHandler(
        filename = function() {
          "scatter.png"
        },
        content = function(file) {
          file.copy("scatter.png", file, overwrite=TRUE)
        }
      )
    })
}












