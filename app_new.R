source("load_libraries.R")
# source("helper_functions.R")
source("data_loading.R")
source("data_table.R")
source("overview_consistency.R")
source("comorbidity_network.R")
source("annotation_module.R")
source("manhattan_plot.R")
source("scatter_plot.R")
source("comorbidity_subnetwork.R")
# source("data_table.R")

starting_code <- "594.10"
# Used in data table to both select correct row and navigate table to that row
start_index <- which(phecodes$phecode == starting_code)

ui = tagList(dashboardPage(
  dashboardHeader(

    ## to move the sidebar from left to top
    # tags$li(
    #   class = "dropdown",
    #   tags$style(HTML("
    #       .navbar-custom-menu{float:left !important;}
    #       .sidebar-menu{display:flex;align-items:baseline;}"))
    # ),
    # #
    # tags$li(
    #   class = "dropdown",
    #   sidebarMenu(
    #     id = "tablist",
    #     menuItem("Code Selection", tabName = "code_selection")
    #   )
    # )

    ## header
    title = strong("Cross-Institution Multimorbidity Explorer"),
    titleWidth = 450
  ),

  dashboardSidebar(
    tags$style(
      "#sidebarItemExpanded {
            overflow: auto;
            max-height: 100vh;
        }"
    ),
    sidebarMenu(
      menuItem(strong("Code selection"),tabName = "code_selection"),
      menuItem(strong("Comorbidity consistency"),tabName = "comorbidity_consistency"),
      menuItem(strong("Comorbidity network"),tabName = "comorbidity_network"),
      menuItem(strong("Explore comorbidity"),tabName = "explore_comorbidity"),
      menuItem(strong("Explore similarity"),tabName = "explore_similarity"),
      menuItem(strong("Download comobidity"),tabName = "download_comorbidities"),
      menuItem(strong("Download similarity"),tabName = "download_similarities")
    )
  ),

  dashboardBody(
    # use_theme(mytheme),

    tabItems(
      #1st tab content
      tabItem(
        tabName = "code_selection",
        fluidRow(column(width=12,div(
          span(textOutput("current_code_label",inline = TRUE),style = "font-size:2.5rem;color: black;center;center;"),
          style =
             "padding-top: 10px;
             padding-bottom: 5px;
             margin-top: 3px;
             display: flex;
             align-items: center;
             justify-content: space-evenly;"
        )),
                 column(width=12,box(width=12,
                   title = strong("Select code of interest",style="font-size: 2.0rem;"),
                   status="warning",solidHeader = FALSE,
                   div(
                     p("Please click on the rows to select code of interest, click twice to unselect;",
                       style = "text-align: left;font-size: 2.0rem; color:black;"),
                     p("You could search the phecode by typing in the 'Search' box;",
                       style = "text-align: left;font-size: 2.0rem; color:black;")
                   ),
                   # hr(),
                   # div(
                   #     span(
                   #       textOutput("current_code_label"),
                   #       style = "font-size:2.5rem;color: black;center;center;"
                   #     )),
                   hr(),
                   DTOutput('code_selection')
                 )),
        )
      ),

      #2nd tab content
      tabItem(
        tabName = "comorbidity_consistency",
        consistency("comorbidity_consistency")
      ),

      #3rd tab content
      tabItem(
        tabName = "comorbidity_network",
        comorbidity_networkPlot("comorbidity_network")
      ),

      #4th tab content
      tabItem(
        tabName = "explore_comorbidity",
        annotationPlot("comorbidity")
      ),

      #5th tab content
      tabItem(
        tabName = "explore_similarity",
        annotationPlot("similarity")
      ),

      #6th tab content
      tabItem(
        tabName = "download_comorbidities",
        fluidRow(
          column(12,div(span(textOutput(strong("all_comorbidity_label"), inline = TRUE),
                                             style = "font-size:2rem;color:black;"
                                    ))),
          column(12,downloadButton('download1',"Download the data")),
          hr(),
          column(12,DTOutput("comorbidities_table")))
      ),

      #7th tab content
      tabItem(
        tabName = "download_similarities",
        fluidRow(
          column(12,div(span(textOutput(strong("all_similarity_label"), inline = TRUE),
                             style = "font-size:2rem;color:black;"
          ))),
          column(12,downloadButton('download2',"Download the data")),
          hr(),
          column(12,DTOutput("similarities_table")))
      )

    ),
    tags$head(tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #2c3e50;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #859900;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #2c3e50;
                              }

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #2c3e50;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color:#859900;
                              /*#dfdfdf;*/
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #2c3e50;
                              /*white;*/
                              color: white;
                              /*#000000;*/
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #859900;
                              /*#dfdfdf;*/
                              }
        /* toggle button when hovered  */
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #859900;
                              /*black;*/
         }
        /* warning status color  */
         .box.box-solid.box-warning>.box-header {
                              color:black;
                              background:#2c3e50;
                              /*#dfdfdf;*/
                              }
         .box.box-solid.box-warning{
                              border-bottom-color:white;
                              border-left-color:white;
                              border-right-color:white;
                              border-top-color:#2c3e50;
                              }
          .box.box-warning>.box-header {
                              color:#000000;
                              background:#fff
                    }
          .box.box-warning{
                              border-bottom-color:#2c3e50;
                              border-left-color:#2c3e50;
                              border-right-color:#2c3e50;
                              border-top-color:#2c3e50;
                              }')))
)
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  current_code <- reactive(phecodes$phecode[input$code_selection_rows_selected]) #phecode
  code_description = reactive(phecodes$description[input$code_selection_rows_selected]) #phecode description
  code_id <- reactive(glue("{current_code()} ({get_phecode_info(current_code())})"))
  current_comorbidities <- reactive(extract_and_diff(com_sim, code_description())) #comorbidity table of selected phecode with other phecodes
  current_similarities <- reactive(extract_and_diff(com_sim, code_description())) #similarity table of selected phecode with other phecodes

  ## overview consistency across three institutions
  consistencyPlotServer(id="comorbidity_consistency",code_description=code_description(),
                        code_id=code_id())

  ## comorbidity network
  # output$comorbidity_network = r2d3::renderD3({
  #    comorbidity_network(all_dat,code_description)
  # })
  comorbidity_networkServer(
    id = "comorbidity_network",
    comorbidities = com_sim,
    code_description = code_description(),
    code_id = code_id
  )

  #explore comorbidity
  ##comorbidity_manhattan and scatter plot across institutions
  annotationPlotServer(
    id = "comorbidity",
    code_id = code_id,
    code_data = current_comorbidities,
    type = "comorbidities",
    type_label = "z",
    plot_fn1 = gen_manhattan,
    plot_fn2 = gen_across_scatter,
    code_description = code_description()
  )
  #
  # #explore similarity
  # ##similarity_manhattan and scatter plot across institutions
  annotationPlotServer(
    id = "similarity",
    code_id = code_id,
    code_data = current_similarities,
    type = "similarities",
    type_label = "sim",
    plot_fn1 = gen_manhattan,
    plot_fn2 = gen_across_scatter,
    code_description = code_description()
  )
  #
  # #data table
  output$comorbidities_table <- renderDT(gen_table(code_id, current_comorbidities, "z"))
  output$download1 <- downloadHandler(
    filename = function(){glue("{code_id()}_comorbidity.csv")},
    content = function(fname){
      write.csv(gen_table(code_id, current_comorbidities, "z"), fname)
    }
  )

  output$similarities_table <- renderDT(gen_table(code_id, current_similarities, "sim"))
  output$download2 <- downloadHandler(
    filename = function(){glue("{code_id()}_similarity.csv")},
    content = function(fname){
      write.csv(gen_table(code_id, current_similarities, "sim"), fname)
    }
  )

  output$code_selection <- renderDT({
    datatable(phecodes,
              rownames = FALSE,
              options = list(displayStart = start_index - 2),
              selection = list(mode = 'single', selected = start_index)
    )},server = FALSE)
  output$current_code_label <- renderText(glue("Current selection: {code_id()}"))
  output$all_comorbidity_label = renderText(glue("All comorbidities for: {code_id()}"))
  output$all_similarity_label = renderText(glue("All similarities for: {code_id()}"))

}

# Run the application
shinyApp(ui = ui, server = server)





