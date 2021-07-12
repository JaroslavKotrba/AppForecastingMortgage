# server
server <- function(input, output){
  
  output$payment_per_month <- renderText({
    paste0("Mortgage payment per a month: ", round((input$mortgage*(input$i.m/12)*(1+(input$i.m/12))^(12*as.numeric(input$year)))/((1+(input$i.m/12))^(12*as.numeric(input$year))-1),2), " CZK")
  })
  
  output$payment_total <- renderText({
    paste0("Mortgage payment in total: ", round((input$mortgage*(input$i.m/12)*(1+(input$i.m/12))^(12*as.numeric(input$year)))/((1+(input$i.m/12))^(12*as.numeric(input$year))-1)*(as.numeric(input$year)*12),2), " CZK")
  })
  
  output$payment_difference <- renderText({
    paste0("Will pay more in CZK: ", round((input$mortgage*(input$i.m/12)*(1+(input$i.m/12))^(12*as.numeric(input$year)))/((1+(input$i.m/12))^(12*as.numeric(input$year))-1)*(as.numeric(input$year)*12) - input$mortgage,2), " CZK")
  })
  
  output$payment_more <- renderText({
    paste0("Will pay more in %: ", round((((input$mortgage*(input$i.m/12)*(1+(input$i.m/12))^(12*as.numeric(input$year)))/((1+(input$i.m/12))^(12*as.numeric(input$year))-1)*(as.numeric(input$year)*12)/input$mortgage)-1)*100,2), " %")
  })
  
  output$payment_salary <- renderText({
    paste0("Should earn netto per a month: ", round((((input$mortgage*(input$i.m/12)*(1+(input$i.m/12))^(12*as.numeric(input$year)))/((1+(input$i.m/12))^(12*as.numeric(input$year))-1)/45)*55) + (input$mortgage*(input$i.m/12)*(1+(input$i.m/12))^(12*as.numeric(input$year)))/((1+(input$i.m/12))^(12*as.numeric(input$year))-1), 2), " CZK")
  })
  
  make_result <- function(x, y, z) {as.numeric(x)*(1-as.numeric(y))^as.numeric(z)}
  output$mortgage_inflation <- renderText({paste0("Mortgage amount after ", input$year, " years: ", round(make_result(input$mortgage, input$inflation, input$year), 2), " CZK")
  })
  
  output$plotly <- renderPlotly({
    library(ggplot2)
    library(plotly)
    year <- as.numeric(input$year)
    i.m <- input$i.m/12
    H <- input$mortgage
    a <- (H*(i.m)*(1+i.m)^(12*year))/((1+(i.m))^(12*year)-1)
    
    ggplot() +
      geom_point(data = data.frame(x = 0:(year), y = (0:(year))*(12*a)), aes(x,y,color="Total Overall Paid (Total)"), size=0.5) +
      geom_point(data = data.frame(x = 0:(year), y = (0:(year))*(12*(H/12)/year)), aes(x,y,color="Total Principal Paid (Mortgage)"), size=0.5) +
      geom_line(data = data.frame(x = 0:(year), y = (a/i.m)*(1 - (1/((1+i.m)^((year:0)*12))))), aes(x,y,color="Principal Remaining"), size=0.5) +
      geom_line(data = data.frame(x = 0:(year), y = cumsum(a*(1-(1/(1+i.m)^(0:(year*12)))))[seq(1, length(cumsum(a*(1-(1/(1+i.m)^(0:(year*12)))))), 12)]), aes(x,y,color="Total Interest Paid"), size=0.5) +
      ylab("Amount (y)") +
      xlab("Years (x)") +
      scale_color_manual(name = "", values = c("Total Overall Paid (Total)" = "red", "Total Principal Paid (Mortgage)" = "cornflowerblue", "Principal Remaining" = "#00798c", "Total Interest Paid" = "orange")) +
      theme_classic()
  })
  
  output$repo <- renderPlot({
    # Repo rate
    library(rvest)
    library(dplyr)
    
    link <- "https://www.cnb.cz/cs/casto-kladene-dotazy/Jak-se-vyvijela-dvoutydenni-repo-sazba-CNB/"
    page <- read_html(link)
    
    date <- page %>% html_nodes('td:nth-child(1)') %>% html_text()
    rate <- page %>% html_nodes('td+ td') %>% html_text()
    
    date <- stringi::stri_escape_unicode(date)
    
    Sazba <- data.frame(cbind(date, rate))
    Sazba$rate <- as.numeric(gsub(",", ".", Sazba$rate))
    Sazba$date <- gsub("\\\\u00a0", "", Sazba$date)
    Sazba$date <- as.Date(Sazba$date, "%d.%m.%Y")
    
    library(ggplot2)
    options(scipen=999)
    ggplot(data = Sazba, aes(x=date,y=rate)) +
      geom_line(color='red', size=1) +
      geom_point(color='blue', size=2) +
      ylab("Repo rate") +
      xlab("Years") +
      theme_bw()
  })
  
  output$disco <- renderPlot({
    # Discount rate
    library(rvest)
    library(dplyr)
    
    link <- "https://www.cnb.cz/cs/casto-kladene-dotazy/Jak-se-vyvijela-diskontni-sazba-CNB/"
    page <- read_html(link)
    
    date <- page %>% html_nodes('td:nth-child(1)') %>% html_text()
    rate <- page %>% html_nodes('td+ td') %>% html_text()
    
    date <- stringi::stri_escape_unicode(date)
    
    Sazba <- data.frame(cbind(date, rate))
    Sazba$rate <- as.numeric(gsub(",", ".", Sazba$rate))
    Sazba$date <- gsub("\\\\u00a0", "", Sazba$date)
    Sazba$date <- as.Date(Sazba$date, "%d.%m.%Y")
    
    library(ggplot2)
    options(scipen=999)
    ggplot(data = Sazba, aes(x=date,y=rate)) +
      geom_line(color='red', size=1) +
      geom_point(color='blue', size=2) +
      ylab("Discount rate") +
      xlab("Years") +
      theme_bw()
  })
  
  output$lombard <- renderPlot({
    # Lombard rate
    library(rvest)
    library(dplyr)
    
    link <- "https://www.cnb.cz/cs/casto-kladene-dotazy/Jak-se-vyvijela-lombardni-sazba-CNB/"
    page <- read_html(link)
    
    date <- page %>% html_nodes('td:nth-child(1)') %>% html_text()
    rate <- page %>% html_nodes('td+ td') %>% html_text()
    
    date <- stringi::stri_escape_unicode(date)
    
    Sazba <- data.frame(cbind(date, rate))
    Sazba$rate <- as.numeric(gsub(",", ".", Sazba$rate))
    Sazba$date <- gsub("\\\\u00a0", "", Sazba$date)
    Sazba$date <- as.Date(Sazba$date, "%d.%m.%Y")
    
    library(ggplot2)
    options(scipen=999)
    ggplot(data = Sazba, aes(x=date,y=rate)) +
      geom_line(color='red', size=1) +
      geom_point(color='blue', size=2) +
      ylab("Lombard rate") +
      xlab("Years") +
      theme_bw()
  })
  
  url <- a("https://jaroslavkotrba.com/", href="https://jaroslavkotrba.com/index.html")
  output$link <- renderUI({
    tagList("To see other author’s projects:", url)
  })
  
  output$link2 <- renderUI({
    tagList("To see other author’s projects:", url)
  })
}