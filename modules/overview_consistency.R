#1st page: overview of the comorbidity consistency across institutions and selection of p-values
consistency = function(id){
  ns <- NS(id)
  vars = list("Vanderbilt"="z_vandy","MGH"="z_mgh","UKB"="z_ukbb")
  labels = c("Vanderbilt","MGH","UKB")
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
    column(width=12,
    box(width=12,title = strong("Select institutions and input p-value",style="font-size: 2.0rem;"),
        solidHeader=F,status="warning",
           # wellPanel(style ="margin-top: 10px;background-color: white; border-color: white;",
      fluidRow(
        column(12,
          div(
            p("Please click 'update consistency plot' to visualize the consistency",
              style = "text-align: left;font-size: 2.0rem; color:black;")),
          hr()),
        column(3,p(HTML("<b>Institution 1</b>"),span(shiny::icon("info-circle"),id = "info_ins1"),
                           selectInput(ns('xcol'), NULL,choices=vars),
                           tippy::tippy_this(elementId = "info_ins1",tooltip = "X-axis plotted the value of institution 1",placement = "right")
      )),
      column(3,p(HTML("<b>Institution 2</b>"),span(shiny::icon("info-circle"),id = "info_ins2"),
                  selectInput(ns('ycol'), NULL,choices = vars,selected = vars[[2]]),
                  tippy::tippy_this(elementId = "info_ins2",tooltip = "Y-axis plotted the value of institution 1",placement = "right")
      )
      ),
      column(3,p(HTML("<b>Input p-value</b>"),span(shiny::icon("info-circle"),id = "info_pvalue"),
                  numericInput(ns('pvalue'), NULL,min=0,max=1,value=1),
                  tippy::tippy_this(elementId = "info_pvalue",tooltip = "The range is from 0 to 1",placement = "right"))
      ),
      column(3,actionButton(ns("update_network"), "Update consistency plot"))
    ))
    ),
    column(width=12,
    box(width=12,
        title=strong("Comorbidity consistency with selected phecode across system",style="font-size: 2.0rem;"),
        solidHeader=F,status="warning",
      # wellPanel(style ="margin-top: 10px;background-color: white; border-color: white;",
      conditionalPanel(
        condition = "input.update_network > 0",
        style = "display: none;",
        ns=ns,
        withSpinner(plotOutput(ns('consistency_plot'),height="600px"))
    )
                 )
      # )
    )
  )
}

consistencyPlotServer = function(id,code_description,code_id){
  moduleServer(
    id,
    function(input,output,session){

      selectedData <- eventReactive(input$update_network,{
        # select pvalue
        p1 = colnames(com_sim)[which(colnames(com_sim)==input$xcol)+1]
        p2 = colnames(com_sim)[which(colnames(com_sim)==input$ycol)+1]
        if(input$xcol!=input$ycol) {
          com_sim[(com_sim[[p1]]<=input$pvalue) & (com_sim[[p2]]<=input$pvalue),] %>%
            # filter((a==code_description) | (b==code_description)) %>%
            mutate(sig_codes=factor(ifelse((a==code_description) | (b==code_description),"yes","no"),levels = c("no","yes"))) %>%
            dplyr::select(x=input$xcol,y=input$ycol,sig_codes)

        } else {
          com_sim[(com_sim[[p1]]<=input$pvalue) & (com_sim[[p2]]<=input$pvalue),] %>%
            # filter((a==code_description) | (b==code_description)) %>%
            mutate(sig_codes=factor(ifelse((a==code_description) | (b==code_description),"yes","no"),levels = c("no","yes"))) %>%
            dplyr::select(x=input$xcol,y=input$xcol,sig_codes)

        }

      })

      output$consistency_plot <- renderPlot({
        Sys.sleep(3)
        ggplot(selectedData(),aes(x=x,y=y)) +
          # geom_bin2d(bins = 100) +
          stat_binhex(bins = 200) +
          scale_fill_gradient(name = "count", trans = "log",
                              breaks = c(160000,8000,400,20,1)) +
          theme_bw() +
          coord_cartesian(xlim = c(-50, 150)) +
          labs(
            x = "Institution 1",
            y = "Institution 2"
          ) +
          theme(
            # panel.border = element_blank(),
            axis.line = element_line(colour = "black"),
            legend.position = "right",
            axis.ticks.x = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank(),
            # panel.grid.major.x = ggplot2::element_blank(),
            # panel.grid.minor.x = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_blank(),
            axis.title.x = element_text(size=20, face="bold", colour = "black"),
            axis.title.y = element_text(size=20, face="bold", colour = "black"),
            plot.title = element_text(size=15, face= "bold", colour= "black" ),
            legend.text=element_text(size=15),
            legend.title=element_text(size=15)
          ) +
          geom_point(selectedData() %>% filter(sig_codes=="yes"),
                     mapping = aes(x = x, y = y),color="#859900")+
          labs(caption = "Green: Significantly associated with selected phecode (< Input p-value)")+
          theme(plot.caption= element_text(size=12, colour= "black" ))
      })

      output$current_code_label <- renderText(glue("Current selection: {code_id}"))
})
}













