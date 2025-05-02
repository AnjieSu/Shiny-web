library(shiny)
library(ggplot2)
library(dplyr)

# load avonet data
d <- read.csv("https://raw.githubusercontent.com/difiore/ada-datasets/refs/heads/main/AVONETdataset1.csv")

# BC transform 
myBCtransform <- function(myvector) {
  require(EnvStats)
  myindex <- which(!is.na(myvector))
  # shift scale to positive numbers and identify optimal lambda for box-cox transformation
  mylambda <- boxcox(as.numeric(myvector[myindex])-min(as.numeric(myvector[myindex]))+1, optimize = T)$lambda
  
  # transform
  myvector[myindex] <- scale(boxcoxTransform(as.numeric(myvector[myindex])-min(as.numeric(myvector[myindex]))+1, mylambda))
  return (myvector)
}

# select columns
trait_cols <- c(
  "Mass",
  "Wing.Length",
  "Tarsus.Length",
  "Beak.Length_Culmen",
  "Tail.Length"
)

# transform the data
for (i in trait_cols) {
  d[[i]] <- myBCtransform(d[[i]])
}

# define UI
ui <- fluidPage(
  titlePanel("Life History Trait Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("xvar", "Choose (X):", choices = trait_cols, selected = "Mass"),
      selectInput("yvar", "Choose (Y):", choices = trait_cols, selected = "Wing.Length"),
      checkboxInput("add_lm", "Add fitted", value = FALSE) # add an option of whether to add the fitted line
    ),
    mainPanel(
      plotOutput("scatterPlot", height = "600px")
    )
  )
)

# the server function
server <- function(input, output, session) {
  # filter NA
  df_filtered <- reactive({
    d %>%
      select(all_of(c(input$xvar, input$yvar))) %>%
      filter(
        !is.na(.data[[input$xvar]]),
        !is.na(.data[[input$yvar]])
      )
  })
  # plot
  output$scatterPlot <- renderPlot({
    df <- df_filtered()
    x <- df[[input$xvar]]
    y <- df[[input$yvar]]
    
    # create a scatter plot
    plot(
      x, y,
      pch  = 16,
      col  = rgb(0, 0, 0, 0.1),
      xlab = input$xvar,
      ylab = input$yvar,
      main = paste("Scatter:", input$yvar, "vs", input$xvar)
    )
    # if user choose to add fitted
    if (input$add_lm) {
      # fit the line
      fit  <- lm(y ~ x)
      # add the fitted line to plot
      abline(fit, lwd = 2, lty = 1, col = "red")
    }
  })
}

# SHINY!
shinyApp(ui, server)
