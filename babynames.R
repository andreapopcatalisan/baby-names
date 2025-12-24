#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinydashboard)
library(plotly)
library(data.table)
library(DT)
library(quanteda)
# Define UI for application that draws a histogram
ui <- dashboardPage(
    
    #header
    dashboardHeader(title = "babynames Data Visualization"),
    
    #side bar
    dashboardSidebar (
        selectInput("type", "Plot Criteria", c("All Babies", "Sex", "Length", 
                                                  "Unique Names", "Average Syllables"))
        ),
    #body
    dashboardBody(
        fluidRow(
            box(plotlyOutput("hist")),
            box(plotOutput("plot"))
        ),
        fluidRow(
            dataTableOutput("data")
        )
))
# Define server logic required to draw a histogram
server <- function(input, output) {
    library(babynames)
    data("babynames")
    syl <- read.csv("syllables.csv")
    babynames$syllables<-syl[,2]
    tab = data.table(babynames)
    
    
    hist_data <- reactive({
        if (input$type=="Sex"){
            hist_dat<-c(sum(tab[year>1, sex=="M"]),sum(tab[year>1, sex=="F"]))
        }else if (input$type=="Length"){
            hist_dat<-c(
                        sum(tab[year>1,nchar(tab$name)<4]),
                        sum(tab[nchar(tab$name)>=4,nchar(tab$name)<8]),
                        sum(tab[year>1,nchar(tab$name)>=8]))
        }else if (input$type=="All Babies"){
            hist_dat<-as.numeric(unlist(tab[n!=0,sum(n), by=year][,2]))
        }else if (input$type=="Unique Names"){
            hist_dat<-as.numeric(unlist(tab[n!=0, length(unique(name)), by=year][,2]))
        }else if (input$type=="Average Syllables"){
            hist_dat<-as.numeric(unlist(tab[n!=0, mean(syllables, na.rm=TRUE), by=year][,2]))
        }
        hist_dat
    })
    output$hist <- renderPlotly({
        if (input$type=="Sex"){
        plot_ly(
            x = c("Male","Female"),
            y = hist_data(),
            type = "bar") %>% 
            layout(title = "Male V.S. Female", xaxis = list(title = "Sex"),yaxis = list(title = "Number of Babies"))
        }else if (input$type=="Length"){
            plot_ly(
                x = c("Short Names (2:3 Characters)", "Medium Names (4:7 Characters)","Long Names (8:15 Characters)"),
                y = hist_data(),
                type = "bar") %>% 
                layout(title = "Length",xaxis = list(title = "Number of Characters"),yaxis = list(title = "Number of Babies"))
        }else if (input$type=="All Babies"){
            plot_ly(
                x = as.character(c(1880:2017)),
                y = hist_data(),
                type = "bar") %>% 
                layout(title = "All Babies",xaxis = list(title = "Year"),yaxis = list(title = "Number of All Babies"))
        }else if (input$type=="Unique Names"){
            plot_ly(
                x = as.character(c(1880:2017)),
                y = hist_data(),
                type = "bar") %>% 
                layout(title = "Unique Names",xaxis = list(title = "Sex"),yaxis = list(title = "Number of Unique Names"))
        }else if (input$type=="Average Syllables"){
            plot_ly(
                x = as.character(c(1880:2017)),
                y = hist_data(),
                type = "bar") %>% 
                layout(title = "Average Syllables",xaxis = list(title = "Year"),yaxis = list(title = "Average Syllables"))
        }
    })
    

    
    
    
    plot_M<-tab[sex=="M",sum(n),by=year]
    plot_F<-tab[sex=="F",sum(n),by=year]
    plot_1<-tab[nchar(tab$name)<4,sum(n),by=year]
    plot_2<-tab[nchar(tab$name)>=4&&nchar(tab$name)<8,sum(n),by=year]
    plot_3<-tab[nchar(tab$name)>=8,sum(n),by=year]
    plot_all<-tab[n!=0,sum(n), by=year]
    plot_uni<-tab[n!=0, length(unique(name)), by=year]
    plot_syl<-tab[n!=0, mean(syllables, na.rm=TRUE), by=year]

    output$plot<-renderPlot({
        if (input$type=="Sex"){
            plot(plot_M, main="Sex", type="l",col="blue", ylab = "Number of Babies")
                lines(plot_F,col="red")
                legend("topleft", c("Male","Female"), fill=c("blue","red"))
        }else if (input$type=="Length"){
            plot(plot_1, main="Number of Character Per Name", type = "l", col = "blue", ylim=c(1500,4100000), ylab = "Number of Babies")
            lines(plot_2, col="red")
            lines(plot_3, col="black")
            legend("topleft", c("Short Names (2:3 Characters)", "Medium Names (4:7 Characters)",
                                "Long Names (8:15 Characters)"), 
                   fill=c("blue","red", "black"), title = "Number of Characters")
        }else if (input$type=="All Babies"){
            plot(plot_all, main="Number of All Babies", type = "l", ylab = "Number of All Babies")
        }else if (input$type=="Unique Names"){
            plot(plot_uni, main="Unique Names", type = "l", ylab = "Number of Unique Names")
        }else if (input$type=="Average Syllables"){
            plot(plot_syl, main="Average Syllables", type = "l", ylab = "Average Syllables")
        }
    })
    
    output$data <- renderDataTable({tab}, 
                                   options = list(scrollX = TRUE))
    
    
    
    
}



# Run the application 
shinyApp(ui = ui, server = server)

