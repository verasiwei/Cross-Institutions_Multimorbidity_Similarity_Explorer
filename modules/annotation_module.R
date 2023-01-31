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
             box(width=12,title=strong("Information",style="font-size: 2.0rem;"),solidHeader = F,status="warning",
    fluidRow(
      column(12,div(p("Drag points region to annotate multiple points; Drag again to remove annotation",style = "font-size: 2.0rem;color: black;"))),
      column(2,div(p("Hovered over:",style = "font-size:2.0rem;color: black;"))),
      column(6,span(textOutput(ns("plot_hoverinfo")),style = "font-size:2.0rem;color: black;"))
      ,
      column(4,actionButton(ns("dump_annotations"), "Remove all annotations"))
    )))),

    fluidRow(
      column(12,
             box(width=12,title=strong("Interact with plot and network",style="font-size: 2.0rem;"),solidHeader = F,status="warning",
                fluidRow(
      column(12,div(p("First click the tab to select institutions to compare; Then click 'Update associationsubgraphs' to update the network",style = "font-size: 2.0rem;color: black;"))),
      column(12,div(p("If there are no points in the plot, then there is no selected phecode in one of the institutions that compared",style = "font-size: 2.0rem;color: black;"))),
      column(8,""),
      # column(4,actionButton(ns("update_subgraph"), "Update associationsubgraphs")),
      hr(),
      column(12,tabBox(width=12,
      # id = ns,
      # type = "tabs",
      tabPanel("Vandy vs MGH",fluidRow(
        column(12,div(actionButton(ns("update_subgraph_vandy_mgh"), "Update associationsubgraphs"),style="float:right")),
        hr(),
        column(6,
               box(width=12,style = "overflow-x:scroll;overflow-y:scroll;",
                   withSpinner(plotOutput(ns("plot1_vandy_mgh"),width='700px',height = "300px",
                   click = ns("point_click"),
                   hover = hoverOpts(id=ns("point_hover")),
                   brush = brushOpts(id=ns("point_brush")))),
                   withSpinner(plotOutput(ns("plot2_vandy_mgh"),width='700px',height = "400px",
                   click = ns("point_click"),
                   hover = hoverOpts(id=ns("point_hover")),
                   brush = brushOpts(id=ns("point_brush")))))
               ),
        column(6,  fluidRow(column(12,box(width=12,
                        div(p("Associationsubgraph: annotated points are colored",
                                      style = "font-size: 1.7rem; margin: 0px 10px 0px;")),
                        # conditionalPanel(
                        #   condition = "input.update_network > 0",
                        #   style = "display: none;",
                        #   ns=ns,
                        uiOutput(ns("spinner1"))
                        # )
                        ))
        )),

        column(12,box(width=12,style = "overflow-x:scroll;overflow-y:scroll;",
                    div(
                      p("Annotated Points: points annotated in the plot will be displayed below", style = "font-size: 1.7rem; margin: 0px 10px 0px;"),
                      dataTableOutput(ns("annotated_points_table1")))))
      ))
      ,
      tabPanel("Vandy vs UKBB",fluidRow(
        column(12,div(actionButton(ns("update_subgraph_vandy_ukbb"), "Update associationsubgraphs"),style="float:right")),
        hr(),
        column(6,
               # wellPanel(style ="overflow-y:scroll;overflow-x:scroll; background-color: white; border-color: white;",
               box(width=12,style = "overflow-x:scroll;overflow-y:scroll;",
                   withSpinner(plotOutput(ns("plot1_vandy_ukbb"),width='700px',height = "300px",
                                          # width = "700px", height = "490px",
                                          click = ns("point_click"),
                                          hover = hoverOpts(id=ns("point_hover")),
                                          brush = brushOpts(id=ns("point_brush")))),
                   withSpinner(plotOutput(ns("plot2_vandy_ukbb"),width='700px',height = "400px",
                                          click = ns("point_click"),
                                          # width = "700px", height = "520px",
                                          hover = hoverOpts(id=ns("point_hover")),
                                          brush = brushOpts(id=ns("point_brush")))))
        ),
        column(6,  fluidRow(column(12,box(width=12,
                                          div(p("Associationsubgraph: annotated points are colored",
                                                style = "font-size: 1.7rem; margin: 0px 10px 0px;")),

                                          # wellPanel(style ="background-color: white; border-color: white;",
                                          uiOutput(ns("spinner2"))))
        )),

        column(12,box(width=12,style = "overflow-x:scroll;overflow-y:scroll;",
                      div(
                        p("Annotated Points: points annotated in the plot will be displayed below", style = "font-size: 1.7rem; margin: 0px 10px 0px;"),
                        dataTableOutput(ns("annotated_points_table2")))))
      ))
      ,
      tabPanel("MGH vs UKBB",fluidRow(
        column(12,div(actionButton(ns("update_subgraph_mgh_ukbb"), "Update associationsubgraphs"),style="float:right")),
        hr(),
        column(6,
               # wellPanel(style ="overflow-y:scroll;overflow-x:scroll; background-color: white; border-color: white;",
               box(width=12,style = "overflow-x:scroll;overflow-y:scroll;",
                   withSpinner(plotOutput(ns("plot1_mgh_ukbb"),width='700px',height = "300px",
                                          # width = "700px", height = "490px",
                                          click = ns("point_click"),
                                          hover = hoverOpts(id=ns("point_hover")),
                                          brush = brushOpts(id=ns("point_brush")))),
                   withSpinner(plotOutput(ns("plot2_mgh_ukbb"),width='700px',height = "400px",
                                          click = ns("point_click"),
                                          # width = "700px", height = "520px",
                                          hover = hoverOpts(id=ns("point_hover")),
                                          brush = brushOpts(id=ns("point_brush")))))
        ),
        column(6,  fluidRow(column(12,box(width=12,
                                          div(p("Associationsubgraph: annotated points are colored",
                                                style = "font-size: 1.7rem; margin: 0px 10px 0px;")),

                                          # wellPanel(style ="background-color: white; border-color: white;",
                                          uiOutput(ns("spinner3"))))
        )),

        column(12,box(width=12,style = "overflow-x:scroll;overflow-y:scroll;",
                      div(
                        p("Annotated Points: points annotated in the plot will be displayed below", style = "font-size: 1.7rem; margin: 0px 10px 0px;"),
                        dataTableOutput(ns("annotated_points_table3")))))
      ))
  )))
  ))
    ))
}

annotationPlotServer <- function(id, code_id, code_data, type, type_label,plot_fn1, plot_fn2, code_description) {
  moduleServer(
    id,
    function(input, output, session) {

      annotated_points <- reactiveVal(c(0)) #change into reactive Values

      #where the issue comes from=====================
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
          plot_fn1(
            code_id,
            mutate(code_data_vandy_mgh(), is_labeled = phecode %in% annotated_points()),
            type,
            type_label,
            "vandy_mgh",
            "yes"
          )
        } else{
          plot_fn1(
            code_id,
            mutate(code_data_vandy_mgh(), is_labeled = phecode %in% annotated_points()),
            type,
            type_label,
            "vandy_mgh",
            "no"
          )
        }

      })

      output$plot2_vandy_mgh <- renderPlot({

        if(nrow(code_data_vandy_mgh())!=0){
        plot_fn2(
          code_id,
          mutate(code_data_vandy_mgh(), is_labeled = phecode %in% annotated_points()),
          type,
          type_label,
          "vandy","mgh",
          "yes"
        )
        } else{
          plot_fn2(
            code_id,
            mutate(code_data_vandy_mgh(), is_labeled = phecode %in% annotated_points()),
            type,
            type_label,
            "vandy","mgh",
            "no"
          )

        }
      })

      #vandy & ukbb
      code_data_vandy_ukbb = reactive({code_data() %>% drop_na(z_vandy_ukbb)})
      output$plot1_vandy_ukbb <- renderPlot({

        if(nrow(code_data_vandy_ukbb())!=0){
        plot_fn1(
          code_id,
          mutate(code_data_vandy_ukbb(), is_labeled = phecode %in% annotated_points()),
          type,
          type_label,
          "vandy_ukbb",
          "yes"
        )
          } else{
            plot_fn1(
              code_id,
              mutate(code_data_vandy_ukbb(), is_labeled = phecode %in% annotated_points()),
              type,
              type_label,
              "vandy_ukbb",
              "no"
            )
        }
      })

      output$plot2_vandy_ukbb <- renderPlot({

        if(nrow(code_data_vandy_ukbb())!=0){
        plot_fn2(
          code_id,
          mutate(code_data_vandy_ukbb(), is_labeled = phecode %in% annotated_points()),
          type,
          type_label,
          "vandy","ukbb",
          "yes"
        )
        } else{
          plot_fn2(
            code_id,
            mutate(code_data_vandy_ukbb(), is_labeled = phecode %in% annotated_points()),
            type,
            type_label,
            "vandy","ukbb",
            "no"
          )
        }
      })

      #mgh & ukbb
      code_data_mgh_ukbb = reactive({code_data() %>% drop_na(z_mgh_ukbb)})
      output$plot1_mgh_ukbb <- renderPlot({

        if(nrow(code_data_mgh_ukbb())!=0){
        plot_fn1(
          code_id,
          mutate(code_data_mgh_ukbb(), is_labeled = phecode %in% annotated_points()),
          type,
          type_label,
          "mgh_ukbb",
          "yes"
        )
        } else{
          plot_fn1(
            code_id,
            mutate(code_data_mgh_ukbb(), is_labeled = phecode %in% annotated_points()),
            type,
            type_label,
            "mgh_ukbb",
            "no"
          )
        }
      })

      output$plot2_mgh_ukbb <- renderPlot({

        if(nrow(code_data_mgh_ukbb())!=0){
        plot_fn2(
          code_id,
          mutate(code_data_mgh_ukbb(), is_labeled = phecode %in% annotated_points()),
          type,
          type_label,
          "mgh","ukbb",
          "yes"
        )
        } else{
          plot_fn2(
            code_id,
            mutate(code_data_mgh_ukbb(), is_labeled = phecode %in% annotated_points()),
            type,
            type_label,
            "mgh","ukbb",
            "no"
          )
        }
      })

      output$annotated_points_table1 <- renderDataTable(
        datatable(code_data_vandy_mgh() %>%
          #filter(phecode == phecodes$phecode[start_index]) %>%
          filter(phecode %in% annotated_points()) %>%
          dplyr::select(phecode,description,category,glue("{(type_label)}_vandy"),glue("{(type_label)}_mgh"),
                        glue("{(type_label)}_ukbb")),
        options(list(pageLength=5)))
      )

      output$annotated_points_table2 <- renderDataTable(
        datatable(code_data_vandy_ukbb() %>%
                    #filter(phecode == phecodes$phecode[start_index]) %>%
                    filter(phecode %in% annotated_points()) %>%
                    dplyr::select(phecode,description,category,glue("{(type_label)}_vandy"),glue("{(type_label)}_mgh"),
                                  glue("{(type_label)}_ukbb")),
                  options(list(pageLength=5)))
      )

      output$annotated_points_table3 <- renderDataTable(
        datatable(code_data_mgh_ukbb() %>%
                    #filter(phecode == phecodes$phecode[start_index]) %>%
                    filter(phecode %in% annotated_points()) %>%
                    dplyr::select(phecode,description,category,glue("{(type_label)}_vandy"),glue("{(type_label)}_mgh"),
                                  glue("{(type_label)}_ukbb")),
                  options(list(pageLength=5)))
      )

      output$current_code_label <- renderText(glue("Current selection: {code_id()}"))

    })
}












