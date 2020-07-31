library(shiny)
library(shinydashboard) 
library(shinydashboardPlus) 
library(dplyr)
library(stringr)
library(ggplot2)
library(readr)
library(leaflet)
library(DT)
library(plotly)
library(ggpubr)
library(tidyverse)
library(ggthemes)
library(NLP)
library(ggrepel)
library(scales)
library(quantmod)
library(tidyquant)
library(BatchGetSymbols)
library(lubridate)


# data input
dailytest <- read_csv("https://opendata.arcgis.com/datasets/fdedcfbed9af43f8b79c87c53229a0d8_0.csv")%>%
  select(-1)
totalcases <- read_csv("https://opendata.arcgis.com/datasets/0573e90adab5434f97b082590c503bc1_0.csv")%>%
  select(-1)%>%
  replace(is.na(.),0) 
totaldeaths <- read_csv("https://opendata.arcgis.com/datasets/3dbd3e633b344c7c9a0d166b1d6a2b03_0.csv")%>%
  select(-1)%>%
  replace(is.na(.),0) 
us <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv")%>%
  filter(date>=as.Date("2020-02-03")) 
MD <- read_csv("MDpop.csv") %>% select(c(1,2))
ds <- read_csv("https://raw.githubusercontent.com/Lucas-Czarnecki/COVID-19-CLEANED-JHUCSSE/master/COVID-19_CLEAN/csse_covid_19_time_series_cleaned/time_series_covid19_tidy_US.csv")%>% 
  select(-UID, -iso2, -iso3, -code3, -Country_Region, -Combined_Key,-Latitude, -Longitude)
county_map <- read_csv("county_map.csv")
names(county_map)[8]<- "FIPS"
policy<-read_csv("policy timeline.csv")

# data cleaning
totalcases$DATE<-as.Date(totalcases$DATE)
totaldeaths$DATE<-as.Date(totaldeaths$DATE)

newcases<-totalcases[-1,1] %>% 
  cbind(data.frame(diff(as.matrix(totalcases[-1])))) %>% 
  gather(variable, value, -DATE) %>% 
  rename(County=variable, newcase=value)

newdeaths <- totaldeaths[-1,1] %>% 
  cbind(data.frame(diff(as.matrix(totaldeaths[-1])))) %>% 
  gather(variable, value, -DATE)%>% 
  rename(County=variable, newdeath=value)

newtests<- dailytest[,-1]
colnames(newtests)<- c(seq.Date(from = as.Date("2020-06-15"), to=as.Date("2020-07-07"), by = 1), seq.Date(from = as.Date("2020-07-13"), length.out = ncol(dailytest)-24, by = 1))
newtests <- cbind(dailytest[,1],newtests) %>% 
  gather(variable, value, -County) %>% 
  mutate(variable = as.Date(variable)) %>% 
  rename(DATE=variable, newtest=value)  %>% 
  merge(newcases,by=c("DATE","County")) %>%
  merge(MD,by="County")

d1=us[-1,1] %>% 
  cbind(data.frame(diff(as.matrix(us[-1])))) %>%
  rename(Date=date, Us.New=cases)

## Stock data manipulation
STOCK_NAME = c("S&P 500 Index", "Dow Jones Index", "NASDAQ Index", "Selected Stock")

# import stock data from Yahoo finance
sp500 <- get.clean.data('^GSPC',
                        first.date = as.Date('2020-02-01'),
                        last.date = as.Date(Sys.Date()))
dowjones<- get.clean.data('^DJI',
                          first.date = as.Date('2020-02-01'),
                          last.date = as.Date(Sys.Date()))
NASDAQ<- get.clean.data('^IXIC',
                        first.date = as.Date('2020-02-01'),
                        last.date = as.Date(Sys.Date()))


# stock data cleaning and assign new variables
dsp=sp500[,c(7,4)] %>% rename(Date=ref.date,Price.Close =price.close)
ddj=dowjones[,c(7,4)] %>% rename(Date=ref.date, Price.Close =price.close)
dns=NASDAQ[,c(7,4)] %>% rename(Date=ref.date,Price.Close =price.close)


stockcase1 <- merge(d1, dsp, by= "Date")  %>%  gather(variable, value, -Date,-deaths)
stockcase2 <- merge(d1, ddj, by= "Date")  %>%  gather(variable, value, -Date,-deaths)
stockcase3 <- merge(d1, dns, by= "Date")  %>%  gather(variable, value, -Date,-deaths)


ui <- dashboardPagePlus(
    header = dashboardHeaderPlus(title = "Covid-19 & Stock Price"),
    sidebar = dashboardSidebar(
        sidebarMenu(
            menuItem("About", tabName = "page5", icon=icon("home")),
            menuItem("Map", tabName = "page2", icon = icon("map-o")),
            menuItem("Maryland", tabName = "page1", icon = icon("line-chart")),
            menuItem("Cases vs Stock", tabName = "page3", icon=icon("line-chart")),
            menuItem("Data", tabName = "page4", icon=icon("table"))
        )
    ),
    
    body = dashboardBody(
        tabItems(
            tabItem(tabName = "page1",
                    fluidRow(
                      tabsetPanel(type = "tabs",
                                tabPanel("New Cases", 
                                           column(3,br(),
                                                  selectizeInput("type", label = "County", choices = colnames(totalcases[-1]),multiple = TRUE),
                                                  p("Select at most 9 counties to update the plot. By selecting different counties, 
                                                    readers will be able to see daily variations of new confirmed cases in different areas in Maryland."),
                                                  p("Note: there are some negative data entries here due to the data adjustments made in the counties' published data")),
                                           column(9, plotlyOutput("plot1", height = 500))),
                                           
                                tabPanel("New Deaths",
                                           column(3,br(),
                                                  selectizeInput("type2", label = "County", choices = colnames(totalcases[-1]),multiple = TRUE),
                                                  p("Select at most 9 counties to update the plot. By selecting different counties, 
                                                    readers will be able to see daily variations of new confirmed deaths in different areas in Maryland."),
                                                  p("Note: there are some negative data entries here due to the data adjustments made in the counties' published data")),
                                           column(9,plotlyOutput("plot2", height = 500))),
                               tabPanel("Cases vs. Tests", 
                                        column(3, br(),
                                               sliderInput("date", label ="Date",
                                                           min = as.Date("2020-07-13"),  max = as.Date(Sys.Date()-days(1)),
                                               value = as.Date(Sys.Date()-days(1)), animate = TRUE),
                                               p("Due to the lack of official data, consistent data can only be traced back to July 13th, 2020.
                                                 This graph shows the ratio of daily new confirmed cases to daily new tests taken in each individual county in Maryland.
                                                 The size of each data point indicates the population")),
                                        column(9,plotlyOutput("plot3", height = 500)))
                             )
                      )
                    ),
            tabItem(tabName = "page2",
                    fluidRow(
                    tabsetPanel(type = "tabs",
                             tabPanel("Total Cases and Deaths",
                                valueBoxOutput("TotalCases", width = "100%"),
                                valueBoxOutput("TotalDeaths", width = "100%")
                             ),
                    
                            tabPanel("Map",
                              column(3,
                                sliderInput("date2", label ="Date",
                                       min = as.Date("2020-02-01"), max = as.Date(Sys.Date()-days(1)),
                                       value = as.Date(Sys.Date()-days(1)), animate = FALSE)
                                  ),
                            column(9, 
                                 box(title = strong("Covid-19 Map"), width = 12, 
                                  plotOutput("myMap"))
                    )
                    )
                    ))),
            tabItem(tabName = "page3",
                    fluidRow(
                      box(width=3,status = "danger",
                          selectInput("stockmarkets", label =  "Choose a stock",choices = STOCK_NAME),
                      conditionalPanel(
                        condition = "input.stockmarkets == 'Selected Stock'",
                        textInput("text", label = "Input a ticker", value = "AMZN")),
                      radioButtons("radio", label = "Show major policies",
                                   choices = c("Fiscal","Monetary and Macro-financial", "Other Events","None"),
                                   selected = "None")),
                      box( width =9, status = "primary",
                           plotOutput("plot4", height = 500, hover = "plot_hover"))
                    ),
                    br(),
                    fluidRow(
                      box(title="Policy Description",status = "danger", solidHeader = TRUE, collapsible = TRUE, width=12,
                      strong("Fiscal"),
                      tags$ul(
                        tags$li("PPPHCA: US$483 billion Paycheck Protection Program and Health Care Enhancement Act."),
                        tags$li("CARES Act: An estimated US$2.3 trillion (around 11% of GDP) Coronavirus Aid, Relief and Economy Security Act."),
                        tags$li("CPRSA Act: US$8.3 billion Coronavirus Preparedness and Response Supplemental Appropriations Act."),
                        tags$li("FFCRA: US$192 billion Families First Coronavirus Response Act.")),
                      strong("Monetary and Macro-financial"),
                      tags$ul(
                        tags$li("Federal funds rate were lowered by 150bp in March to 0-0.25bp. Purchase of Treasury and agency securities in the amount as needed. 
                        Expanded overnight and term repos. Lowered cost of discount window lending. Reduced existing cost of swap lines with major central banks and extended the maturity of FX operations; 
                        broadened U.S. dollar swap lines to more central banks; offered temporary repo facility for foreign and international monetary authorities.
                        Federal Reserve also introduced facilities to support the flow of credit, in some cases backed by the Treasury using funds appropriated under the CARES Act. 
                                "),
                        tags$li("Regulatory action: Lower the community bank leverage ratio to 8 percent. Provide extension transition for the Current Expected Credit Loss accounting standard.
                                And there will be a gradual phase-in of restrictions on distributions when a firm's capital buffer declines."),
                        tags$li("Supervisory action: Federal banking supervisors encouraged depository institutions to use their capital and liquidity buffers to lend, to work constructively with borrowers affected by COVID-19, 
                                and indicated COVID-19 related loan modifications would not be classified as troubled debt restructurings. 
                                Holdings of U.S. Treasury Securities and deposits at the Federal Reserve Banks could be temporarily excluded from the calculation of the supplementary leverage ratio for holding companies."),
                        tags$li("Fannie Mae and Freddie Mac have announced assistance to borrowers, including providing mortgage forbearance for 12 months and waiving related late fees, 
                                suspending reporting to credit bureaus of delinquency related to the forbearance, suspending foreclosure sales and evictions of borrowers for 60 days, and offering loan modification options."))
                    )),
                    br(),
                    fluidRow(
                     # h3("More on this topic"),
                      box(title="How has COVID-19 hit the stock market \n| Chicago Booth Review",status = "primary", 
                          solidHeader = TRUE, collapsible = TRUE,
                             HTML('<iframe width="100%" height="250" 
                      src="https://www.youtube.com/embed/z8_Vv2dhkUg" 
                      frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" 
                                    allowfullscreen></iframe>')),
                      box(title="How to Make Sense of Coronavirus Data | WSJ",status = "primary", solidHeader = TRUE,
                          collapsible = TRUE,
                          HTML('<iframe width="100%" height="250" 
                             src="https://www.youtube.com/embed/VPAabBdDyhA" frameborder="0" 
                             allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" 
                                  allowfullscreen></iframe>'))
                    )
                    ),
            tabItem(tabName = "page4",
                    selectInput("Database", label =  "Data bases",
                                choices = c("Cases in Maryland", STOCK_NAME[1:3])),
                    dataTableOutput("myTable")
                    ),
            tabItem(tabName = "page5",
                    fluidPage(
                      h2(strong("The COVID 19 and Its Impact on Stock Markets"), align = "center"),
                      
                      h3("Description", align = "left",style="color:SteelBlue"),
                      tags$ul(
                        tags$li("Part 1: we aim to visualize and study the covid-19 virus spreading trajectory in different counties in Maryland as well as the overall US covid-19 virus new cases in each state."), 
                        tags$li("Part 2: we aim to study the covid-19 pandemic’s impact and relations to the US stock market. To measure the impact, we decided to use the daily closing price of US stocks and some stock indexes as metrics. 
                                We used some major events in our graphs to show potential influences of those events on the stock prices and covid-19 cases.")),
 
                      h3("Research Questions", align = "left",style="color:SteelBlue"),
                      tags$ul(
                        tags$li("The trajectory of covid-19 new cases and deaths in different counties in Maryland"), 
                        tags$li("The relationship between the daily new confirmed cases and daily number of test taken in Maryland"), 
                        tags$li("The overall covid-19 new cases development in the US"),
                        tags$li("The potential relationships between covid-19 new cases in the US and the US stock markets"),
                        tags$li("The potential influences of major events on stock prices as well as new covid-19 cases")
                      ),
                      h3("Dataset Description",style="color:SteelBlue"),
                      tags$ul(
                        tags$li("Covid 19 datasets are all from https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series"),
                        tags$li("For daily new confirmed cases and deaths in Maryland, we selected total confirmed cases and deaths from Feb 1st, 2020 to the system date. Then we calculated the new confirmed cases and deaths by subtracting the i day from the i+1 day. Then we created dataset with the date, new confirmed cases, new deaths, and county."),
                        tags$li("For daily new tests taken, we imported the data from the website of Maryland Department of Health. Due to the lack of official data, consistent data can only be traced back to July 13th, 2020. We created dataset with daily new tests taken, date, and county."),
                        tags$li("For the map data, we directly imported csv file from class blackboard."),
                        tags$li("For stock market closing price, we directly imported from Yahoo finance.")
                      ),
                      
                      
                    
                      h3("Methods and Analysis", align = "left",style="color:SteelBlue"),
                      strong("Map"),
                      p("Method: we used the most up-to-date data from the website to plot a map to show the numbers of cases in each state and how the epicenter of the virus moved overtime."),
                      p("Analysis: as our map shows, there were more cases in the upper east coast at the beginning of the virus outbreak, but as we approach July, the cases in the lower west coast (California, to be specific) and the middle started to grow significantly."),
                      strong("Covid-19 in Maryland"),
                      p("Method: The group used the most-up-to date data on Maryland covid-19 to plot the trajectory of new cases, deaths and new confirmed death vs. tests."),
                      p("Analysis: The graphs show that the new cases and deaths caused by covid-19 have been declining since June for all counties in Maryland. There is, however, a slight increase in new cases in late July. It will be interesting to see how the daily new cases in Maryland evolve in the near future. 
                       The graph of daily new confirmed cases vs. daily tests taken has shown a positive relationship between the daily new confirmed cases and daily tests taken."),
                      # strong("Map"),
                      # p("Method: we used the most up-to-date data from the website to plot a map to show the numbers of cases in each state and how the epicenter of the virus moved overtime."),
                      # p("Analysis: as our map shows, there were more cases in the upper east coast at the beginning of the virus outbreak, but as we approach July, the cases in the lower west coast (California, to be specific) and the middle started to grow significantly."),
                      strong("Cases vs. Stocks"),
                      p("Method: The group imported data from official websites for stock closing prices as well as daily new confirmed cases in the US. We wrote a program to plot the data side by side for easy comparison. We also programmed a feature for users to search for any stocks they’re interested in and compare the stock trajectory with covid-19 new confirmed cases in the US trajectory."),
                      p("Analysis: It is very interesting to note that the overall stock market is going in the same direction as daily new confirmed cases, as one can observe by choosing S&P 500, Dow Jones or NASDAQ indexes.However, different industries react differently to the pandemic. For example, Boeing's stock has decreased dramatically, but Netflix's stock price has increased dramatically.
                        Major events such as covid-19 tests breakthrough seems to have a positive effort on the stock prices."),
                      
                      
                      
                      h3("Reference", align = "left",style="color:SteelBlue"),
                      strong("Codes and graphs inspired by"),
                      tags$ul(
                      tags$li(a("http://freerangestats.info/blog/2016/08/18/dualaxes")),
                      tags$li(a("https://stackoverflow.com/questions/34370780/r-interactive-plot-show-vertical-line-on-hover")),
                      ),
                      strong("Data Source"),
                      tags$ul(
                      tags$li(a("https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series")),
                      tags$li(a("https://github.com/nytimes/covid-19-data")),
                      tags$li(a("https://coronavirus.maryland.gov/search?collection=Dataset")),
                      tags$li("Yahoo Finance"),
                      tags$li(a("https://www.imf.org/en/Topics/imf-and-covid19/Policy-Responses-to-COVID-19#U"))
                      )),
                    
                      h3("Biography and Contact Information", align = "left",style="color:SteelBlue"),
                    
                      fluidRow(
                        infoBox(h5(strong("Guhyung Kwon")),h6( "JHU MS in Business Analytics and Risk Management\n hiro90@gmail.com"), icon = icon("user"), width = 6),
                        infoBox(title = h5(strong("Qiyun Mo")),subtitle = h6( "JHU MS in Business Analytics and Risk Management\n kelseymo220@gmail.com"), icon = icon("user"), width = 6)
                        ),
                      fluidRow(
                        infoBox(title = h5(strong("Annabelle Wang")),subtitle = h6( "JHU MS in Business Analytics and Risk Management, MS in Finance; \nBrandeis BS in Computer Science, BA in Economics, Psychology, Minor in Business \nannabellewang97@gmail.com"), icon = icon("user"),width = 6),
                        infoBox(title = h5(strong("Jiahong Xu")),subtitle = h6( "JHU MS in Business Analytics and Risk Management\n jasonxu95@gmail.com"), icon = icon("user"), width = 6)
                        
                        )
                        
                    )
              )
          ),
    
    title = "Data Visualization Group 8"
)



server <- function(input, output, session) {
  
  ## plot 1
  p <- ggplot() +
    geom_line(aes(x = DATE,y =newcase,group=County),data= newcases,colour = alpha("grey", 0.7)) +
    theme_minimal()+ labs(x = "Date") +
    theme(plot.title =element_text(size=20,face="bold", color="steelblue", lineheight=1.2), axis.title.x=element_blank(), axis.title.y=element_blank(),
          axis.text.x=element_text(size=10), axis.text.y=element_text(size=10),legend.text =element_text(size=10),
          plot.margin = unit(c(1,1,1,1),"cm"))+
    labs(y = "New Confirmed Cases",
         title = "New Confirmed Cases in Maryland")  
  
  output$plot1 = renderPlotly({
    if(is.null(input$type)){
      p
    }else{
      p <- p+
        scale_color_brewer(palette="Reds",type=rev("seq"))+
        geom_line(aes(DATE, newcase, colour = County), data = newcases %>% filter(County==input$type))
    }
    return(ggplotly(p))        
  })
  
  ## plot 2  
  p2 <- ggplot() +
    geom_line(aes(x = DATE,y =newdeath,group=County),data= newdeaths,colour = alpha("grey", 0.7)) +
    theme_minimal()+ labs(x = "Date") +
    theme(plot.title =element_text(size=20,face="bold", color="steelblue", lineheight=1.2), axis.title.x=element_blank(), axis.title.y=element_blank(),
          axis.text.x=element_text(size=10), axis.text.y=element_text(size=10),legend.text =element_text(size=10),
          plot.margin = unit(c(1,1,1,1),"cm"))+
    labs(y = "New Confirmed Deaths",
         title = "New Confirmed Deaths in Maryland") 
  
  output$plot2 = renderPlotly({
    if(is.null(input$type2)){
      p2
    }else{
      p2 <- p2+
        scale_color_brewer(palette="Purples",type="seq")+
        geom_line(aes(DATE, newdeath, colour = County), data = newdeaths %>% filter(County==input$type2))
    }
    return(ggplotly(p2)) 
  })    
  
  # plot3
  output$plot3 = renderPlotly({
    p3<-ggplot(aes(x=newtest, y=newcase, size= pop2020,color=County),data=newtests %>% 
                 filter(DATE==input$date)) +
      geom_point(alpha=0.5) +
      theme_minimal()+ 
      theme(plot.title =element_text(size=15,face="bold", color="steelblue", lineheight=1.2),
            axis.text.x=element_text(size=10), axis.text.y=element_text(size=10),legend.text =element_text(size=10),
            plot.margin = unit(c(1,1,1,1),"cm"))+
      guides(size = FALSE)+
      labs(x="New Tests",y = "New Confirmed Cases",
           title = "New Confirmed Cases vs. New Tests in Maryland")+
      xlim(0,5700)+
      ylim(0,220)
    return(ggplotly(p3))     
  })
  
    output$myMap = renderPlot({
      loc_ds= ds %>%
        na.omit() %>%
        filter(Date==input$date2) 
      loc_ds$adjustedconfirmed=case_when(loc_ds$Confirmed>=0 & loc_ds$Confirmed<10 ~ "1",
                                         loc_ds$Confirmed>=10 & loc_ds$Confirmed<100 ~ "2",
                                         loc_ds$Confirmed>=100 & loc_ds$Confirmed<500 ~ "3",
                                         loc_ds$Confirmed>=500 & loc_ds$Confirmed<1000 ~ "4",
                                         loc_ds$Confirmed>=1000 & loc_ds$Confirmed<10000 ~ "5",
                                         loc_ds$Confirmed>=10000 & loc_ds$Confirmed<100000 ~ "6",
                                         loc_ds$Confirmed>=100000 ~ "7")
      county_map$FIPS = as.numeric(county_map$FIPS)
      county_full = left_join(county_map, loc_ds, by = "FIPS")
      
      m <- ggplot(data = county_full%>% na.omit,
                  mapping = aes(
                    x = long, y = lat, 
                    fill = adjustedconfirmed,
                    group = group)) + 
        geom_polygon(color = "gray90", 
                     size = 0.01) + 
        coord_equal() +
        theme_map()+
        scale_fill_manual(values = RColorBrewer::brewer.pal(n = 7, name = "Oranges"),
                          labels = c("0-10", "10-100", "100-500", "500-1,000", "1,000-10,000", "10,000-100,000", ">100,000")) + 
        labs(fill = "Cumulative \nConfirmed Cases") +
        theme(legend.position = "bottom")
      return(m)
    })
    
    g = reactive({
      
      # validate(
      #   need(input$text!="", "Please input a ticker."))
      # 
      if(input$text==""){
        stock <- get.clean.data("AMZN",
                                first.date = as.Date('2020-02-01'),
                                last.date = as.Date(Sys.Date())) %>%
          select(7,4) %>%
          rename(Date=ref.date,Price.Close =price.close)
        stockcase4 = merge(d1, stock, by = "Date")  %>%  gather(variable, value, -Date,-deaths)
      }
      else {
      stock <- get.clean.data(input$text,
                              first.date = as.Date('2020-02-01'),
                              last.date = as.Date(Sys.Date())) %>%
        select(7,4) %>%
        rename(Date=ref.date,Price.Close =price.close)
      stockcase4 = merge(d1, stock, by = "Date")  %>%  gather(variable, value, -Date,-deaths)
      }
      
      if (input$stockmarkets==STOCK_NAME[1]){
        stockcase <- stockcase1
      }
      else if (input$stockmarkets == STOCK_NAME[2]){
        stockcase <- stockcase2
      }
      else if (input$stockmarkets == STOCK_NAME[3]){
        stockcase <- stockcase3
      }
      else if (input$stockmarkets == STOCK_NAME[4]){
        stockcase <- stockcase4
      }
      
      g <- ggplot()  +  
        geom_line(stockcase, mapping=aes(x = Date, y = value, color = variable),size=1) + 
        theme_light()+
        theme(axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position="none",
              plot.title=element_text(size=20,face="bold", color="steelblue", lineheight=1.2),
              axis.text.x=element_text(size=12), axis.text.y=element_text(size=10),
              strip.text.x = element_text(size = 12, face="bold"),
              plot.margin = unit(c(1,1,1,1),"cm"))+
        facet_wrap(~variable, scales = "free_y", ncol = 1)+
        labs(title=paste0(input$stockmarkets," During Covid-19"),
             caption = "Price.Close: Closing price from Yahoo Finance \nUs.New: New reported cases by day in the United States")
      
      if (input$radio=="Fiscal"){
        g <- g+  
          geom_vline(data=policy%>%select(1,2)%>%na.omit(), mapping=aes(xintercept = Date), color = "black",linetype="dashed")+
          geom_text(data=policy%>%select(1,2)%>%na.omit(), mapping=aes(x=Date, y=20,label=Fiscal),
                    size=4, angle=90,hjust = 0.03, vjust = -0.2)
      }
      else if (input$radio=="Monetary and Macro-financial"){
        g <- g+  
          geom_vline(data=policy%>%select(1,3)%>%na.omit(), mapping=aes(xintercept = Date), color = "black",linetype="dashed")+
          geom_text(data=policy%>%select(1,3)%>%na.omit(), mapping=aes(x=Date, y=20,label=Monetary),
                    size=3.5, angle=90,hjust = 0.01, vjust = -0.2)
      }
      else if (input$radio=="Other Events"){
        g <- g+  
          geom_vline(data=policy%>%select(1,4)%>%na.omit(), mapping=aes(xintercept = Date), color = "black",linetype="dashed")+
          geom_text(data=policy%>%select(1,4)%>%na.omit(), mapping=aes(x=Date, y=20,label=Other),
                    size=3.5, angle=90,hjust = 0.01, vjust = -0.2)
      }else {
        g}
      
      return(g)
    })
    
    output$plot4 <- renderPlot({
     return(g())

    })
    
    # plot after mouse over
    observeEvent(input$plot_hover, {
      stock <- get.clean.data(input$text,
                              first.date = as.Date('2020-02-01'),
                              last.date = as.Date(Sys.Date())) %>%
        select(7,4) %>%
        rename(Date=ref.date,Price.Close =price.close)
      stockcase4 = merge(d1, stock, by = "Date")  %>%  gather(variable, value, -Date)
      
      if (input$stockmarkets==STOCK_NAME[1]){
        stockcase <- stockcase1
      }
      else if (input$stockmarkets == STOCK_NAME[2]){
        stockcase <- stockcase2
      }
      else if (input$stockmarkets == STOCK_NAME[3]){
        stockcase <- stockcase3
      }
      else if (input$stockmarkets == STOCK_NAME[4]){
        stockcase = stockcase4
      }
      
      else {
        stockcase = NULL
        print("Stock input error in hover")
      }
      
      x = input$plot_hover$x
      y = input$plot_hover$y
      nearPoint <- nearPoints(stockcase, input$plot_hover,
                              threshold = 5, maxpoints = 1)
      
      output$plot4 <- renderPlot({

        if (nrow(nearPoint) == 1) {
          g() + 
            geom_vline(xintercept = nearPoint$Date, color="grey") +
            geom_label(data=data.frame(input$plot_hover$x,input$plot_hover$y), x= input$plot_hover$x, y=input$plot_hover$y,
                       label = paste(nearPoint$Date,
                                     "\n New Cases: ", d1$Us.New[d1$Date == nearPoint$Date],
                                     "\n Closing Price: ", round(stockcase$value[stockcase$Date == nearPoint$Date 
                                                                                 & stockcase$variable == "Price.Close"],2)
                       )) 
        }else {g()}
      })
    })
    
    output$myTable = renderDataTable({
      # should add a stock
      if(input$Database == "Cases in Maryland"){database=totalcases}
      else if (input$Database == STOCK_NAME[1]){database=sp500}
      else if (input$Database == STOCK_NAME[2]){database=dowjones}
      else if (input$Database == STOCK_NAME[3]){database=NASDAQ}
      else 
        database = NULL
               
      return(datatable(database, rownames= FALSE))  
    })
    
    output$TotalCases <- renderValueBox({
      valueBox(
        us$cases[nrow(us)], "US Total Cases", icon = icon("head-side-mask"),
        color = "yellow"
      )
    })
   
     output$TotalDeaths <- renderValueBox({
      valueBox(
        us$deaths[nrow(us)], "US Total Deaths", icon = icon("viruses"),
        color = "aqua"
      )
    })
    
    
}

shinyApp(ui = ui, server = server) 


