# load the required packages
library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(readxl)
library(RColorBrewer)
library(tidyverse)
library(lubridate)
library(plotly)

archive <- read_excel("archive.xlsx")

header <- dashboardHeader(title = "NOBEL LAUREATES")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Introduction", tabName = "introduction", icon = icon("home",lib='glyphicon')),
    
    menuItem("OverView", icon = icon("bar-chart-o"), startExpanded = TRUE,
             menuSubItem("Let's Discover", tabName = "discover"),
             menuSubItem("Let's Explore", tabName = "explore"),
             menuSubItem("Let's Dig", tabName = "dig") 
    ),
    
    menuItem("Connect with us", icon = icon("hand-point-down"), startExpanded = TRUE, 
             menuSubItem("Github",icon = icon("github"), href = "https://github.com/satyam9090"),
             menuSubItem("Medium",icon = icon("medium"), href = "https://medium.com/@sat00yam"),
             menuSubItem("Instagram",icon = icon("instagram"), href = "https://www.instagram.com/satyam.naman"))
    )
)



body <- dashboardBody(
  
  tabItems(
    tabItem(
      tabName = "introduction",
      fluidRow(
                 box(
                    title = "VISUALIZATION OF NOBEL LAUREATES USING R",width = 12, 
                    background = "black", status = "danger", 
                    p("Between 1901 and 2016, the Nobel Prizes and the Prize in Economic Sciences were awarded 579 times to 911 people and 
                      organizations. The Nobel Prize is an international award administered by the Nobel Foundation in Stockholm, 
                      Sweden, and based on the fortune of Alfred Nobel, Swedish inventor and entrepreneur. In 1968, Sveriges Riksbank 
                      established The Sveriges Riksbank Prize in Economic Sciences in Memory of Alfred Nobel, founder of the Nobel Prize. 
                      Each Prize consists of a medal, a personal diploma, and a cash award."),
                    p("A person or organization awarded the Nobel Prize is called Nobel Laureate. The word 'laureate' refers to being 
                      signified by the laurel wreath. In ancient Greece, laurel wreaths were awarded to victors as a sign of honor."),
                    p("Nobel Prize, any of the prizes (five in number until 1969, when a sixth was added) 
                      that are awarded annually from a fund bequeathed for that purpose by the Swedish inventor and industrialist Alfred Nobel. 
                      The Nobel Prizes are widely regarded as the most prestigious awards given for intellectual achievement in the world.
                      - britannica.com"))
               ),
      
      fluidRow(
        
        column( width = 3,
          box(
            title = "Year", width = 12, background = "red",
            "The year that this noble prize was awarded.", p("Example: 1904")
          ),
          box(
            title = "Category", width = 12, background = "black",
            "In which field the prize is given.", p("Example: Physics")
          ),
          box(
            title = "Prize", width = 12, background = "red",
            "Elaborative way used to describe the prize given and in which field.", p("Example: 'The Nobel Prize in Physics 1903")
          ),
          box(
            title = "Motivation",width = 12, background = "black",
            "Major contribution which resulted in winning Nobel Prize.", p("Example: 'for his investigations of the densities of the most important gases and for his discovery of argon in connection with these studies'")
          )
        ),
        
        column( width = 3,
          box(
            title = "Laureate ID", width = 12, background = "black",
            "Laureate ID's Given by the Staff", p("Example: 8")
          ),
          box(
            title = "Full Name", width = 12, background = "red",
            "Full name of the recipient.", p("Example: Lord Rayleigh (John William Strutt)")
          ),
          box(
            title = "Birth Date", width = 12, background = "black",
            "Birth date of the recipient.", p("Example: 1842-11-12")
          ),
          box(
            title = "Birth City", width = 12, background = "red",
            "Birth city of the recipient", p("Example: Langford Grove, Maldon, Essex")
          )
        ),
        
        column(width = 3,
          box(
            title = "Birth Country",width = 12, background = "red",
            "Birth country of the recipient.", p("Example: United Kingdom")
          ),
          box(
            title = "Sex",width = 12, background = "black",
            "Sex of the recipient", p("Example: Male")
          ),
          box(
            title = "Organization Name",width = 12, background = "red",
            "Name of the Organisation", p("Example: Royal Institution of Great Britain")
          ),
          box(
            title = "Organization City",width = 12, background = "black",
            "Residing city of the organisation", p("Example: London")
          )
        ),
        
        column(width = 3,
          box(
            title = "Prize Share", width = 12, background = "black",
            "The year that this noble prize was awarded.", p("Example: 01-Jan")
             ),
          box(
            title = "Organization Country",width = 12, background = "red",
            "Country of the Organisation", p("Example: United Kingdom")
          ),
          box(
            title = "Death Date",width = 12, background = "black",
            "Death date of the recipient", p("Example: 30/06/1919")
          ),
          box(
            title = "Death City",width = 12, background = "red",
            "Death city of the recipient", p("Example: Starnberg")
          ),
          box(
            title = "Death Country", width = 12, background = "black", 
            "Death Country of the recipient", p("Example: United Kingdom")
            )
        )
      )
    
    ),
  
    tabItem(
      tabName = "discover",
        fluidRow(
          box(width = 6,
            title = "Which field got most numbers of Prizes?"
            ,status = "danger"
            ,solidHeader = TRUE 
            ,collapsible = TRUE 
            ,plotOutput("plot1")
          ),
          
          valueBoxOutput("plot2", width = 3),
          
          valueBoxOutput("plot3", width = 3),
          
          box(
            title = "Top 4 Countries to have most Number of Awards",width = 6, background = "navy"
            ),
          
          valueBoxOutput("plot4", width = 3),
          
          valueBoxOutput("plot5", width = 3),
          
          valueBoxOutput("plot6", width = 3),
          
          valueBoxOutput("plot7", width = 3)
        ),
      
      fluidRow(
        box(width = 7,
            title = "Trend of Field MEDICINE Over Years"
            ,status = "danger"
            ,solidHeader = TRUE 
            ,collapsible = TRUE 
            ,plotOutput("plot8",height = 300)
        ),
        box(width = 5,
            title = "Gender Proportion of Field MEDICINE Over Years"
            ,status = "danger"
            ,solidHeader = TRUE 
            ,collapsible = TRUE 
            ,plotOutput("plot9",height = 300)
        )
      ),
      
      fluidRow(
        box(width = 12,
            title = "Country wise Nobel Prize distribution of the Field MEDICINE"
            ,status = "danger"
            ,solidHeader = TRUE 
            ,collapsible = TRUE 
            ,plotOutput("plot10",height = 400)
        )
      )
    ),
    
    tabItem(
      tabName = "explore",
      fluidRow(
        column(
          width = 9,
               box(width = 12,
                   title = "Nobel Prize Trends For Females Over The Year!"
                   ,status = "danger"
                   ,solidHeader = TRUE 
                   ,collapsible = TRUE 
                   ,plotOutput("plot11",height = 400)
               ),
               box(width = 12,
                   title = "Nobel Prize Trends For Males Over The Year!"
                   ,status = "danger"
                   ,solidHeader = TRUE 
                   ,collapsible = TRUE 
                   ,plotOutput("plot12",height = 400)
               )
        ),
        
        column(
          width = 3,
          valueBoxOutput("plot13",width = 12)
        )
      ),
      
      fluidRow(
        column(
          width = 6,
          box(width = 12,
              title = "When did USA actually started Dominating?"
              ,status = "danger"
              ,solidHeader = TRUE 
              ,collapsible = TRUE 
              ,plotOutput("plot14",height = 300)
          ),
          
          box(
            width = 12, background = "red",
            "We can observe it was going little steady in the first 20 Years but in 1930 it was turning the 
            table around. We can also observe that after World War 1 it showed a most jump and 
            then showing again after World War 2. What does it Tell? We get to know form this 
            Brain Drain was the problem other countries were facing. Resulting in the 
            advancement of USA."
          )
        ),
        
        column(
          width = 6,
          box(width = 12,
              title = "Is it always Men Dominating?"
              ,status = "danger"
              ,solidHeader = TRUE 
              ,collapsible = TRUE 
              ,plotOutput("plot15",height = 300)
          ),
          box(
            width = 12, background = "red",
            "We calculated the proportion which means if the point is at 25% that means in that year 
            total percentage of Men winning were 100% - 25% = 75% and female winning 25% that is if 
            4 awards were given that year 3 will be with Male's and 1 with Female. We can clearly see 
            that there is one field female dominated over Men. 'Peace' category in 2010 decade showed 
            that more number of Female were awarded with Peace prize rather than the Males who were 
            dominating since the beginning."
          )
        )
      )
    ),
    
    tabItem(
      tabName = "dig",
      fluidRow(
        column(
          width = 6,
            box(width = 12,
                title = "What age were they when they received prize?"
                ,status = "danger"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotOutput("plot16",height = 400)
          ),
          
          box(
            title = "Who is the 'Youngest' Nobel Prize Winner?", width = 12, background = "teal"
          ),
          
          box(
            title = "Malala Yousafzai, is the Youngest Nobel prize winner. 
            She received the prize in 2014 for category of Peace at the age of 17.", width = 6, background = "blue"
          ),
          valueBoxOutput("plot18",width = 6)
        ),
        
        column(
          width = 6,
            box(width = 12,
                title = "What age were they when they received prize?"
                ,status = "danger"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotOutput("plot17",height = 400)
            ),
          
          box(
            title = "Who is the 'Oldest' Nobel Prize Winner?", width = 12, background = "blue"
          ),
          
          valueBoxOutput("plot19",width = 6),
          
          box(
            title = "Leonid Hurwicz, is the Oldest Nobel prize winner. He received the prize in 2007 for 
            category of Economics, at age of 90.", width = 6, background = "teal"
          )
        
        )
        
      )
    )
  )
)

ui <- dashboardPage(title = 'Nobel Laureates Visualisation', header, sidebar, body, skin='red')

server <- function(input, output) { 
  
  category <- archive %>%
    group_by(Category) %>%
    summarise(count=n())
  
  temp <- category$count
  total = sum(temp)
  temp <- (temp/total)*100
  category$count_percent <- temp
  coul = brewer.pal(6, "BuPu") 
  lbls1 = paste(category$Category,"count =",category$count)
  
  output$plot1 <- renderPlot({
    pie(category$count_percent,labels = lbls1,main = "Number of Nobel Prizes Distributed in Each Field",col = coul)
  })
  
  output$plot2 <- renderValueBox({
    valueBox(
      sum(category$count), "Total Nobel Prizes", icon = icon("award"),
      color = "teal"
    )
  })
  
  output$plot3 <- renderValueBox({
    valueBox(
      max(category$count), "Maximum: Medicine", icon = icon("stethoscope"),
      color = "olive"
    )
  })
  
  top20_countries <- archive %>% 
    count(Birth_Country) %>%
    arrange(desc(n)) %>%
    head(20)
  
  output$plot4 <- renderValueBox({
    valueBox(
      top20_countries[1,2],top20_countries[1,1], icon = icon("globe-americas"),
      color = "maroon"
    )
  })
  
  output$plot5 <- renderValueBox({
    valueBox(
      top20_countries[2,2],top20_countries[2,1], icon = icon("globe-americas"),
      color = "purple"
    )
  })
  
  output$plot6 <- renderValueBox({
    valueBox(
      top20_countries[3,2],top20_countries[3,1], icon = icon("globe-americas"),
      color = "purple"
    )
  })
  
  output$plot7 <- renderValueBox({
    valueBox(
      top20_countries[4,2],top20_countries[4,1], icon = icon("globe-americas"),
      color = "maroon"
    )
  })
  
  archive2 <- archive %>%
    mutate(decade = (archive$Year - archive$Year%%10))
  
  Medicine <- archive2 %>%
    filter(archive$Category == "Medicine") 
  medicine_decade <- as.data.frame(table(Medicine$decade))
  medicine_decade$Var1 = levels(droplevels(medicine_decade$Var1))
  
  output$plot8 <- renderPlot({
    plot(medicine_decade$Var1,medicine_decade$Freq, type="b" , lwd=3 , col=rgb(0.1,0.7,0.7,0.8) , ylab="Number of Nobel Prize" , xlab="Nobel Prize trends in the field of MEDICINE over the years" , bty="l" , pch=20 , cex=4)
    abline(h=seq(0,100,10) , col="grey", lwd=0.8)
  })
  
  med_gender <- Medicine %>%
    mutate(Sex = Medicine$Sex)
  
  med_gender <- med_gender %>%
    group_by(med_gender$Sex) %>%
    summarise(count = n())
  
  med_gender$percent <- (med_gender$count/sum(med_gender$count))*100
  med_gender$percent <- round(med_gender$percent,digits = 2)
  
  lbls2 <- med_gender$`med_gender$Sex`
  #lbls <- levels(droplevels(lbls))
  lbls2 <- paste(lbls2," ",med_gender$percent,"%",sep = '') 
  
  output$plot9 <- renderPlot({
    pie(med_gender$percent,labels = lbls2,main = "Gender Proportion of Nobel Prize in Medicine Sector",
        col = c(rgb(1,0,0.4),rgb(0.3,0.6,0.7)))
  })
  
  med_country <- Medicine %>%
    mutate(Birth_country = Medicine$Birth_Country)
  
  med_country <- med_country %>%
    group_by(med_country$Birth_country) %>%
    summarise(count = n())
  
  order_med_country <- med_country[order(med_country$count),]
  
  output$plot10 <- renderPlot({
    ggplot(data = order_med_country,aes(x=order_med_country$`med_country$Birth_country`,y=order_med_country$count)) + 
      geom_segment(data = order_med_country,aes(x=order_med_country$`med_country$Birth_country`,xend=order_med_country$`med_country$Birth_country`,y=0,yend=order_med_country$count),color="skyblue",size=1.5) + 
      geom_point(color="blue",size=3,alpha=0.6) + coord_flip() + theme_light() + 
      theme(panel.grid.major.y = element_blank(), panel.border = element_blank(), axis.ticks.y = element_blank()) + 
      ylab("Number of Nobel Prize Winners") + xlab("Birth Country of Winners")
  })
  
  female_prize <- archive %>%
    filter(archive$Year>1900, archive$Year<2017, archive$Sex=="Female") %>%
    group_by(Year,Sex) %>%
    count()
  
  output$plot11 <- renderPlot({
    ggplot(data=female_prize,aes(x=Year,y=n)) + geom_point(col=rgb(0,0.4,0.3),size=4) + 
      geom_line(col=rgb(0,0.4,0.3),size=1.25,alpha=0.4) +  xlab("Year 1901-2016") + 
      ylab("Number of Female Nobel Prize Winners")
  })
  
  
  male_prize <- archive %>%
    filter(archive$Year>1900, archive$Year<2017, archive$Sex=="Male") %>%
    group_by(Year,Sex) %>%
    count()
  
  output$plot12 <- renderPlot({
    ggplot(data=male_prize,aes(x=Year,y=n)) + geom_point(col=rgb(1,0.3,0),size=3,alpha=0.4) + 
      geom_line(col=rgb(1,0.5,0),size=1,alpha=0.6) + geom_smooth(color = "orange") +xlab("Year 1901-2016") + 
      ylab("Number of Male Nobel Prize Winners in that Year")
  })
  
  first_female <- archive %>%
    filter(Sex=='Female') %>%
    top_n(1,desc(Year))
  
  output$plot13 <- renderValueBox({
    valueBox(
      first_female$Year,paste("First Women to get a Nobel Prize, Marie Curie for ", first_female$Category, sep = '' ), icon = icon("female"),
      color = "purple"
    )
  })
  
  prop_usa <- archive %>%
    mutate(usa_born_winner = ifelse(Birth_Country == 'United States of America',TRUE,FALSE)) %>%
    mutate(Decade = Year - Year %% 10) %>%
    group_by(Decade) %>%
    summarize(Proportion = mean(usa_born_winner,na.rm=TRUE))
  
  options(repr.plot.width=7, repr.plot.height=4)
  
  output$plot14 <- renderPlot(
    ggplot(prop_usa, aes(x = Decade, y = Proportion)) + geom_line(col=rgb(0.2,0.7,0.5),size=1,alpha=0.6) + 
      geom_point(size=4,col=rgb(0.2,0.6,0.3)) + 
      scale_y_continuous(labels = scales::percent, limits=c(0.0,1.0),expand=c(0,0)) + theme_minimal()
  )
  
  prop_female <- archive %>%
    mutate(female_winner = ifelse(Sex=='Female',TRUE,FALSE)) %>%
    mutate(Decade = Year - Year %% 10) %>%
    group_by(Decade, Category) %>%
    summarize(proportion = mean(female_winner,na.rm=TRUE))
  
  output$plot15 <- renderPlot(
    ggplot(prop_female, aes(x = Decade, y = proportion, color=Category)) + 
      geom_line() + geom_point(size=2,alpha=0.7) + 
      scale_y_continuous(labels = scales::percent, limits=c(0.0,1.0),expand=c(0,0))
  )
  
  nobel <- read.csv("nobel.csv")
  
  nobel_age <- nobel %>%
    mutate(age = year - year(as.Date(birth_date)))
  
  output$plot16 <- renderPlot(
    ggplot(nobel_age,aes(x=year,y=age)) + 
      geom_point(size = 2, alpha = 0.7) + geom_smooth() + xlab("Year of Nobel Prize Distribution") + ylab("Age while Receiving the Prize")
  )
  
  output$plot17 <- renderPlot(
    ggplot(nobel_age,aes(x=year,y=age)) + geom_point() + geom_smooth(aes(colour = category, fill = category)) + facet_wrap(~ category)
  )
  
  youngest <- nobel_age %>%
    top_n(1,desc(age))
  
  youngest <- droplevels(youngest)
  
  output$plot18 <- renderValueBox({
    valueBox(
      youngest$age,youngest$full_name, icon = icon("award"),
      color = "blue"
    )
  })
  
  oldest = nobel_age %>%
    top_n(1,(age))
  
  oldest <- droplevels(oldest)
  
  output$plot19 <- renderValueBox({
    valueBox(
      oldest$age,oldest$full_name, icon = icon("award"),
      color = "teal"
    )
  })
  
}

shinyApp(ui, server)