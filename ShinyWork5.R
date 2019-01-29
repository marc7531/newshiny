

## global.R ##
#install.packages("Hmisc") #for the correlation matrix + p values / rcorr
#install.packages("corrplot")

library(shinydashboard)
library(googleVis)
library(ggvis)
library(dplyr)
library(stocks)
library(DT)
library(Hmisc)
library(corrplot)

#Data Section of global.R
betavector <- c()
masterframe <- read.csv("masterfile.csv")   #470 rows
master1 <- masterframe %>% 
  filter(EBITDA.x>0,Symbol!="BHGE") %>% #405 rows.  BHGE has to be excluded because no rev growth.  Also BF.B's
  mutate(Beta=round(Beta,2)) 

symbollist <- unique(master1$Symbol)
axis_vars <- c('Beta','Price.Earnings','Dividend.Yield','Market.Cap','Leverage','Rev_Growth')


#Function to retrieve industry ETF ticker for stock
spdretf <- function(categ){
  case_when(
    categ=="Industrials" ~ "XLI",
    categ=="Consumer Staples" ~ "XLP",
    categ=="Consumer Discretionary" ~ "XLY",
    categ=="Health Care" ~ "XLV",
    categ=="Materials" ~ "XLB",
    categ=="Communication Services" ~ "XLC",
    categ=="Utilities" ~ "XLU",
    categ=="Information Technology" ~ "XLK",
    categ=="Energy" ~ "XLE",
    categ=="Financials" ~ "XLF",
    categ=="Real Estate" ~ "XLRE"
  )
}

library(shinydashboard)

## Ui.R
ui <- fluidPage(shinyUI(
  dashboardPage(
    dashboardHeader(title = 'Beta Factor Shiny App'),
    dashboardSidebar(
      sidebarUserPanel(
        name = 'Stock Beta - S&P 500',
        subtitle = 'Jan 2019 | M. Hasson',
        image = 'beta.png'
      ),
      sidebarMenu( #icons money-bill-wave, university, 
        menuItem("Dashboard", tabName = "Dashboard", icon = icon("tachometer-alt")),
        menuItem("Graphs", tabName = "Graphs", icon = icon("chart-bar")),
        menuItem("Data", tabName = "Data", icon = icon("database")),
        menuItem("Correlation",tabName="Correlation",icon=icon("university")
          )
      ),
      selectizeInput("chosenticker",
                     "Select Ticker",
                     choice=symbollist)  #chosenticker is the input variable (used to be called selected)
    ),
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "Dashboard",   #used to be map
                fluidRow(infoBoxOutput("minBox"),tags$style("#minBox {width:600px;height: 65px;}")),
                fluidRow(infoBoxOutput("BetaBox"),tags$style("#BetaBox {width:600px;}")),
                fluidRow(),
                fluidRow(htmlOutput("Correltext")),br(),
                fluidRow(plotOutput("CorrelationSummary")), br(),
                fluidRow(dataTableOutput("correlationtable1")),
                fluidRow(dataTableOutput("correlationtable2"))
                ),
        tabItem(tabName = "Graphs",
                fluidPage(
                  titlePanel("Beta Explorer"),
                  fluidRow(
                    column(6,
                           wellPanel(
                             h4("Select"),
                             sliderInput("betafilter", "Range of Beta Values Desired:",0, 2, 0.25, value=c(0,1),sep = ""),
                             #sliderInput("divfilter", "Dividend Yield (%) Desired:", 0, 10.0, 0.5,value=c(0.0,5.0),sep=""),
                           #sliderInput("pefilter", "P/E Ratio Desired:",0, 40, 20, step = 5),
                            selectInput("sectorfilter", "Sector Desired:",
                                c("All", "Industrials","Consumer Staples","Consumer Discretionary","Health Care",
                                  "Materials","Utilities","Information Technology","Energy","Financials","Real Estate")
                    )
                  )), # end of column
                  column(6,
                  wellPanel(
                    selectInput("xvar", "X-axis variable", "Beta", selected = "Beta"),
                    selectInput("yvar", "Y-axis variable", axis_vars, selected = "Price.Earnings")
                  ),
                  wellPanel(radioButtons("col","Switch Plot",
                               choices = c("Scatter", "Histogram","Regression"),inline=TRUE,
                               selected = "Scatter"))
                  ) # end of column 
                  ),#end of fluid row
                column(12,
                       conditionalPanel(
                         condition = "input.col == 'Scatter'", ggvisOutput("plot1")),
                       conditionalPanel(
                         condition = "input.col == 'Histogram'",htmlOutput("scat")),
                       conditionalPanel(
                         condition = "input.col == 'Regression'", verbatimTextOutput("frame3"))
                )
                
                )), #this parenthesis closes tabitem = Graphs
        tabItem(tabName = "Data",
                # datatable
                fluidRow(dataTableOutput("Metrics"),tags$style("#BetaBox {width:600px;}"))),
        tabItem(tabName = "Correlation",
                selectInput("sectorfilter2", "Sector Desired:",
                            c("All", "Industrials","Consumer Staples","Consumer Discretionary","Health Care",
                              "Materials","Utilities","Information Technology","Energy","Financials","Real Estate")),
                fluidRow(verbatimTextOutput("summary")))
      ))
  )
))

server <- function(input, output, session) {


  output$Dashboard <- renderGvis({

   
    
    #CompanyPE = newdf1$Price.Earnings[newdf1$Symbol==input$chosenticker]
    #CompanyDiv = newdf1$Dividend.Yield[newdf1$Symbol==input$chosenticker]
    
  })
  # show histogram using googleVis
  output$scat <- renderGvis(

    gvisHistogram(dataforgraphs()[,input$yvar, drop=FALSE],option=list(legend="{position: 'top'}")))
  
  output$frame3 <- renderPrint({
    regressframe <- dataforgraphs() %>% select(Beta,Price.Earnings,Market.Cap,Dividend.Yield,Market.Cap,Leverage,Rev_Growth)
    fit <- lm(regressframe$Beta ~ regressframe[,input$yvar])
    #names(fit$coefficients) <- c("Intercept", input$var2)
    summary(fit)
  })
  
  output$summary <- renderPrint({
    sectoroption2 = as.character(input$sectorfilter2)
    regressframe <- master1 %>% select(Sector,Beta,Price.Earnings,Market.Cap,Dividend.Yield,Market.Cap,Leverage,Rev_Growth)
    if (sectoroption2 != "All") {
      regressframe <- regressframe %>% filter(Sector==sectoroption2)}
    
    correlatewithp <- rcorr(as.matrix(regressframe[-1]))
    correlatewithp
  })
  
 correltable1 = reactive({
   a <- master1 %>% filter(.,Symbol==input$chosenticker) %>% select(Symbol,Beta,Price.Earnings,Dividend.Yield,Rev_Growth,Leverage)
})

 correltable2 = reactive({
   CompanySector = as.character(master1$Sector[master1$Symbol==input$chosenticker])
     b <- master1 %>% 
     filter(.,Symbol != input$chosenticker) %>%
     group_by(Sector) %>% 
      filter(Sector==CompanySector) %>% 
     summarise('Avg Beta'=round(mean(Beta),2),'P/E'=round(mean(Price.Earnings),2),'Div Yield'=round(mean(Dividend.Yield),2),'Rev Growth'=round(mean(
       Rev_Growth),2),'Leverage'=round(mean(Leverage),2))
     #c <- filter(Sector==CompanySector)
 })   
   
  
  
  fulltable = reactive({
    rbind(master1 %>%
            filter(.,Symbol == input$chosenticker),
          master1 %>%
            filter(.,Symbol != input$chosenticker)) %>%
      select(Symbol,Beta,'PE Ratio'=Price.Earnings,'Dividend % Yield'=Dividend.Yield,'Leverage Ratio'=Leverage,
             'Avg Revenue Growth'=Rev_Growth,Sector) 
  })
  
  sectortable = reactive({
    rbind(master1 %>%
      filter(.,Symbol == input$chosenticker),
      master1 %>%
        filter(.,Symbol != input$chosenticker)) %>%
        filter(.,Sector == Sector[1]) %>%
        select(Symbol,Beta,'PE Ratio'=Price.Earnings,'Dividend % Yield'=Dividend.Yield,'Leverage Ratio'=Leverage,
             'Avg Revenue Growth'=Rev_Growth,Sector) 
  })
  
  correlplot = reactive({
    CompanySector = as.character(master1$Sector[master1$Symbol==input$chosenticker])
    cormaster <- master1 %>% filter(.,Sector == CompanySector) %>% select(Market.Cap,Price.Earnings,Leverage,Rev_Growth,Dividend.Yield,Beta)
    cortab <- round(cor(cormaster),2)
    #cor.test(cortab)
    #model = lm(data=cormaster,Beta ~ Dividend.Yield + Price.Earnings + Market.Cap + Rev_Growth + Leverage)
    #summary(model)
  })
  
  
  output$Metrics = renderDataTable(
    sectortable(),
    server = F,
    selection = list(mode = 'single',selected = 1),
    rownames = FALSE
  )
  
  output$CorrelationSummary = renderPlot(
    corrplot(correlplot(),type = "upper", tl.col = "black", tl.srt = 45)

  )
  
  output$Correltext <- renderUI({
    CompanySector = as.character(master1$Sector[master1$Symbol==input$chosenticker])
    str1 <- paste("The ticker you selected,",input$chosenticker,", is classified in the",CompanySector,"sector. Recent betas shown above; comparative table below.")
    str2 <- paste("A correlation plot for the",CompanySector,"sector can help visualize which factors most/least contribute to beta in this sector")
    str3 <- paste("Notably, not all S&P 500 sectors are influenced by these factors in the same way.")
    str4 <- paste("This app can help investors better understand the behavior of a sector, as well as a specific stock in that sector, relative to the market overall")
    HTML(paste(str1, str2, str3, str4, sep = '<br/>'))
  })
  
  output$correlationtable1 <- renderDataTable(
    datatable(correltable1(), escape=FALSE, 
              options = list(sDom  = ''))
  )
  output$correlationtable2 <- renderDataTable(
    datatable(correltable2(), escape=FALSE, 
              options = list(sDom  = ''))
  )
  
  
  output$minBox <- renderInfoBox({
    CompanyName = master1$Name[master1$Symbol==input$chosenticker]
    CompanySector = master1$Sector[master1$Symbol==input$chosenticker]
    infoBox((paste('Sector: ',CompanySector)),CompanyName)
  })
  output$BetaBox <- renderInfoBox({
    tickerbeta = master1$Beta[master1$Symbol==input$chosenticker]
    CompanySector = master1$Sector[master1$Symbol==input$chosenticker]
    SectorBeta = case_when(
        CompanySector =="Industrials" ~ 1.05,
        CompanySector =="Consumer Staples" ~ 0.58,
        CompanySector =="Consumer Discretionary" ~ 1.11,
        CompanySector =="Health Care" ~ 0.87,
        CompanySector =="Materials" ~ 0.92,
        CompanySector =="Communication Services" ~ 1.03,
        CompanySector =="Utilities" ~ 0.33,
        CompanySector =="Information Technology" ~ 1.34,
        CompanySector =="Energy" ~ 1.03,
        CompanySector =="Financials" ~ 0.89,
        CompanySector =="Real Estate" ~ 0.54
      )
      
    industryETF = spdretf(CompanySector)
    industrybeta = SectorBeta
    comparebetas = ifelse(tickerbeta>industrybeta,"This stock has a beta that is higher than its sector",ifelse(
            tickerbeta<industrybeta,"This stock has a beta that is lower than its sector","This stock has a beta that is comparable to its industry beta"))
    infoBox(HTML(paste("Beta for ",CompanySector," industry: ",industrybeta,'<br/>', 
                  "Specific Beta for ticker",input$chosenticker, "is:",tickerbeta,'<br/>',comparebetas)),
            icon = icon("calculator"), fill = TRUE)
  })
  
  dataforgraphs <- reactive({
    minbeta <- input$betafilter[1]
    maxbeta <- input$betafilter[2]
    sectoroption <- as.character(input$sectorfilter)
    #xvar <- 'Beta'
    #yvar <- input$yvar
    master2 <- master1 %>% 
      filter(
        Beta >= minbeta,
        Beta <= maxbeta,
        Price.Earnings<=45) %>% 
      mutate(Market.Cap=Market.Cap/1e9)
    
    #Optional: filter if sectorfilter is ALL
    if (sectoroption != "All") {
      master2 <- master2 %>% filter(Sector==sectoroption)}
    master2
    
  })
  
  
  vis1 <- reactive({
    # Lables for axes
    #xvar_name <- names(axis_vars)[axis_vars == input$xvar]
    yvar_name <- names(axis_vars)[axis_vars == input$yvar]
    
    # Normally we could do something like props(x = ~BoxOffice, y = ~Reviews),
    # but since the inputs are strings, we need to do a little more work.
    xvar <- prop("x", as.symbol(input$xvar))
    yvar <- prop("y", as.symbol(input$yvar))
    
    dataforgraphs() %>%
      ggvis(x=xvar, y=yvar,fill=~Sector) %>%
      layer_points() 
      #stroke = ~has_oscar, key := ~ID) %>%
      #add_tooltip(movie_tooltip, "hover") %>%
      #add_axis("x", title = "Beta") %>%
      #scale_numeric("x", domain = c(minbeta, maxbeta), clamp = T)
      #add_axis("y", title = yvar_name,title_offset=120) 
      #add_legend("stroke", title = "Won Oscar", values = c("Yes", "No")) %>%
      #scale_nominal("stroke", domain = c("Yes", "No"),
      #              range = c("orange", "#aaa")) %>%
      #set_options(width = 500, height = 500)
  })
  
  vis1 %>% bind_shiny("plot1")
 
 output$plot2 <- renderPlot({       ######NOT WORKING !!!!!
   
    # Lables for axes
    #xvar_name <- names(axis_vars)[axis_vars == input$xvar]
    #yvar_name <- names(axis_vars)[axis_vars == input$yvar]
    
    # Normally we could do something like props(x = ~BoxOffice, y = ~Reviews),
    # but since the inputs are strings, we need to do a little more work.
    xvar <- prop("x", as.symbol(input$xvar))
    yvar <- prop("x", as.symbol(input$yvar))

    

  })
 
vis3 <- reactive({   
   xvar <- prop("x", as.symbol(input$xvar))
   yvar <- prop("x", as.symbol(input$yvar))
   ggvis(dataforgraphs(), props(x = xvar, y = yvar)) +
     layer_point() +
     layer_smooth(method = "lm", se = TRUE)
 }) 


  
}

shinyApp(ui, server)
