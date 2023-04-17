#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinycssloaders)
source("global.R")
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel(h1("African Future Scenarios Review")),

    # Sidebar with a slider input for number of bins 
    column(width=12,
        fluidRow(
            checkboxGroupInput("edge_type", h2("Select study origin"), 
                               choices=c("African first author", "Overseas first author"),
                               selected=c("African first author", "Overseas first author"))
        ),

        # Show a plot of the generated distribution
        fluidRow(
          withSpinner(plotOutput("distPlot", width = "98vw", height="70vh"))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white',
    #          xlab = 'Waiting time to next eruption (in mins)',
    #          main = 'Histogram of waiting times')
    # })
  
  # common plot theme
  maptheme <- theme(panel.grid = element_blank()) +
    theme(axis.text = element_blank()) +
    theme(axis.ticks = element_blank()) +
    theme(axis.title = element_blank()) +
    theme(legend.position = "bottom") +
    theme(panel.grid = element_blank()) +
    theme(panel.background = element_rect(fill = "#596673")) + #596673
    theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))
  
  # common polygon geom for plotting the country shapes
  country_shapes <- geom_polygon(data = map_data('world'), aes(x = long, y = lat, group = group),
                                 fill = "#f2f6fa", color = "#515151", size = 0.15)
  # common coordinate system for all the following plots
  mapcoords <- coord_fixed(xlim = c(-150, 180), ylim = c(-55, 80))
  
  
  ##################
  theme_transp_overlay <- theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA)
  )
  
  
  p_base <- ggplot() + country_shapes + mapcoords + maptheme
  
  
  #Edges with 2 different colors
  
  p_edges_ori_afr <- ggplot(country_pairs[country_pairs$origin =="From Africa",]) +
    geom_curve(aes(x = x, y = y, xend = xend, yend = yend,     # draw edges as arcs
                   size = weight),arrow=arrow(length = unit(0.03, "npc"), type = "closed"),
               curvature = 0.33, alpha = 0.25,color = "#ff860d") +
    scale_size_continuous(guide = "none", range = c(0.7, 2.5)) +  # scale for edge widths
    mapcoords + maptheme + theme_transp_overlay +
    theme(legend.position = c(0.5, 0.05), legend.direction = "horizontal")
  
  p_edges_ori_afr
  
  p_edges_ori_non_afr <- ggplot(country_pairs[country_pairs$origin =="From Oversees",]) +
    geom_curve(aes(x = x, y = y, xend = xend, yend = yend,     # draw edges as arcs
                   size = weight),arrow=arrow(length = unit(0.03, "npc"), type = "closed"),
               curvature = 0.33, alpha = 0.25,color = "#0dffff") +
    scale_size_continuous(guide = "none", range = c(0.7, 2.5)) +  # scale for edge widths
    mapcoords + maptheme + theme_transp_overlay +
    theme(legend.position = c(0.5, 0.05), legend.direction = "horizontal")
  
  p_edges_ori_non_afr
  
  p_nodes_noLegend2 <- ggplot(unique_countries) +
    geom_point(data=unique_countries_co, aes(x = lon, y = lat, size = weight),
               shape = 21, fill = "#172ce6", color = "#172ce6",    # draw nodes
               stroke = 0.5)+
    scale_size_continuous(guide = "none", range = c(1, 5)) +    # scale for node size
    geom_point(data=unique_countries_first,aes(x = lon, y = lat, fill = count),
               shape = 23, alpha=1, color = "grey",size=1.1,
               stroke = 0.3) +
    scale_fill_gradient(low = "white", high = "red")+
    mapcoords + maptheme + theme_transp_overlay+
    theme(legend.position = "none")
  
  
  p_nodes_noLegend2
  
  
  ######################################################
  #####################FINAL FOR PAPER#################
  # p <- p_base +
  #   annotation_custom(ggplotGrob(p_edges_ori_non_afr), ymin = -74) +
  #   annotation_custom(ggplotGrob(p_edges_ori_afr), ymin = -74) +
  #   annotation_custom(ggplotGrob(p_nodes_noLegend2), ymin= -74)
  # 
  output$distPlot <- renderPlot({
    if("Overseas first author" %in% input$edge_type){
      p <- p_base + annotation_custom(ggplotGrob(p_edges_ori_non_afr), ymin = -74) +
        annotation_custom(ggplotGrob(p_nodes_noLegend2), ymin= -74)
    }
    if ("African first author" %in% input$edge_type){
      p <- p_base + annotation_custom(ggplotGrob(p_edges_ori_afr), ymin = -74) +
        annotation_custom(ggplotGrob(p_nodes_noLegend2), ymin= -74)
    }
    
    if (length(setdiff(c("African first author", "Overseas first author"), input$edge_type)) == 0){
      p <- p_base +
        annotation_custom(ggplotGrob(p_edges_ori_non_afr), ymin = -74) +
        annotation_custom(ggplotGrob(p_edges_ori_afr), ymin = -74) +
        annotation_custom(ggplotGrob(p_nodes_noLegend2), ymin= -74)
    }
    
    if (length(setdiff(c("African first author", "Overseas first author"), input$edge_type)) == 2){
      p <- p_base
    }
    p
  })

  #This is a comment
}

# Run the application 
shinyApp(ui = ui, server = server)
