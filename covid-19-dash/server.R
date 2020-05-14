library(shiny)
library(shinydashboard)
library(ggplot2)
library(readr)
library(tidyverse)
library(maps)
library(RCurl)
library(ggplot2)
library(tibble)
library(sf)
library(maps)
library(plotly)
library(maps)
library(tidyverse)
library(RCurl)
library(scales) 
library(ggthemes)
library(data.table)
library(lubridate)
shinyServer(function(input, output, session) {
    df_table <- reactiveFileReader(
        intervalMillis = 10000,
        session = session,
        filePath = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv",
        readFunc = fread)
    df_deaths <- reactiveFileReader(
        intervalMillis = 10000,
        session = session,
        filePath = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",
        readFunc = fread)
    df_reovered <- reactiveFileReader(
        intervalMillis = 10000,
        session = session,
        filePath = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv",
        readFunc = fread)
    df_ann <- reactiveFileReader(
        intervalMillis = 10000,
        session = session,
        filePath = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
        readFunc = fread)
    df <- reactiveFileReader(
        intervalMillis = 10000, 
        session = session,
        filePath ='https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv',
        readFunc = fread)
    df_world <- reactiveFileReader(
        intervalMillis = 10000, 
        session = session,
        filePath ='https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv',
        readFunc = fread)
    country.pop <- reactiveFileReader(
        intervalMillis = 10000, 
        session = session,
        filePath ='https://raw.githubusercontent.com/datasets/population/master/data/population.csv',
        readFunc = read_csv)
    
    output$mydata <- renderDT({
      df <- df_table()
      df <- df %>% filter(state == 'Florida') %>% select(date, county,cases, deaths) %>% mutate(date=ymd(date))
      df <- df %>% filter(date == max(date)) %>% arrange(desc(cases))
      return(df)
    })
    tolisten <- reactive({
      list(input$day, input$County)
    })
    
    output$vbox <- renderValueBox({
      df_box <- df_table() %>% filter(state == 'Florida') %>% select(date, county,cases, deaths) %>% mutate(date=ymd(date))
      
      tot.cases <- df_box %>% filter(date == max(date))%>% select(cases) %>% summarise(Cases = sum(cases))    #%>% filter(county %in% req(input$County1212))
      valueBox(
        subtitle = tags$p("# of Covid-19 Cases in Florida", style = "font-size: 175%"),
        value = tags$p(paste(prettyNum(tot.cases, big.mark = ",", scientific = F)), style = "font-size: 100%"),
        icon = icon("n"))
      
    })
    
    output$vbox2 <- renderValueBox({
      df_box <- df_table() %>% filter(state == 'Florida') %>% select(date, county,cases, deaths) %>% mutate(date=ymd(date))
      
      tot.deaths<- df_box %>% filter(date == max(date))%>% select(deaths) %>% summarise(Cases = sum(deaths))
      #%>% filter(county %in% req(input$County1212))
      valueBox(
        subtitle = tags$p("# of Covid-19 Deaths in Florida", style = "font-size: 175%"),
        value = tags$p(paste(prettyNum(tot.deaths, big.mark = ",", scientific = F)), style = "font-size: 100%"),
        icon = icon("health"))
    })
    
    output$world_plot <- renderPlotly({
        dta <- df_world()
        corona <- dta
        names(corona)[115] <- "may11"
        names(corona)[2] <- "Country.Region"
        covid <- corona %>%
            select("may11", "Country.Region") %>%
            group_by(Country.Region) %>% summarise(sum = sum(may11))
        worlddata <- map_data("world")
        ## Second, rename some countries in world bank data:
        covid <- covid %>% mutate(Country.Region = fct_recode(Country.Region, 'USA' = "US", "Democratic Republic of the Congo" = "Congo (Kinshasa)",
                                                              'Republic of Congo' = 'Congo (Brazzaville)', 'Ivory Coast' = "Cote d'Ivoire" ))
        # Merging the two datasets
        wbmap <- left_join(covid, worlddata, by = c(Country.Region = "region"))
        countryandworld <- country.pop()
        country.pop <- countryandworld
        if(input$worldmap == "Total"){
            pp <- ggplot(wbmap, aes(x = long, y = lat, group = group, fill = log(sum), text = paste("Country: ",Country.Region, "<br>",  "Total Cases:",scales::comma(sum, 1)))) +
                geom_polygon(color = "black", size = 0.1) +
                theme_minimal() +
                coord_equal() + scale_fill_viridis_c(labels = comma) +
                labs(x ="", y = "") + theme_map() +
                scale_fill_continuous(labels = comma,name = "Number of Cases") +
                scale_fill_gradientn(colours = hcl.colors(7,palette = "Emrld", rev = T)) +
                theme(legend.position = "none")
            return(ggplotly(pp, tooltip = c("text")))
        }
        if(input$worldmap == "Million"){
            country.pop <- filter(country.pop, Year == '2018')
            print(head(country.pop))
            names(country.pop)[1] <- "Country.Name"
            country.pop <- country.pop %>% mutate(Country.Name = fct_recode(Country.Name, 'USA' = 'United States',"Ivory Coast" = "Cote d'Ivoire", "Egypt" = "Egypt, Arab Rep.", "Iran" = "Iran, Islamic Rep.","Russia" = "Russian Federation", "Venezuela" = "Venezuela, RB", "Democratic Republic of the Congo" = "Congo, Dem. Rep.", "Republic of Congo" = "Congo, Rep.", "Czechia" = "Czech Republic", "Slovakia" = "Slovak Republic", "Yemen" = "Yemen, Rep.", "Syria" = "Syrian Arab Republic", "Kyrgyzstan" = "Kyrgyz Republic", "Laos" = "Lao PDR", "Burma" = "Myanmar"))
            #country.pop$Country.Name
            pop.and.covid <- left_join(covid, country.pop, by = c(Country.Region = "Country.Name"))
            names(pop.and.covid)[5] <- "Population"
            pop.and.covid <- pop.and.covid %>% mutate(Country.Region = fct_recode(Country.Region, "Democratic Republic of the Congo" = "Congo (Kinshasa)",'Republic of Congo' = 'Congo, Rep.',"USA" = "US", "Ivory Coast" = "Cote d'Ivoire", 'Republic of Congo' = 'Congo (Brazzaville)', "Myanmar" = "Burma", "Czech Republic" = "Czechia", "Slovakia" = "Slovakia", "Macedonia" = "North Macedonia", "UK" = "United Kingdom"))
            pop.world <- left_join(pop.and.covid, worlddata, by = c(Country.Region = "region"))
            pop.world <- transform(pop.world, sumpop = (sum /Population)*1000000)
            #print(head(pop.world))
            p1 <- ggplot(pop.world, aes(x = long, y = lat, group = group, fill = log(sumpop), text = paste("Country: ",Country.Region, "<br>",  "Cases per Million:",scales::comma(sumpop, 1)))) + geom_polygon(color = "black", size = 0.1) +
                theme_minimal() +
                coord_equal() +
                scale_fill_viridis_c(labels = comma) + labs(title = "Total Corona Cases Per Country", subtitle = "Comparing Countries Affected",
                                                            x ="", y = "")  + scale_fill_continuous(labels = comma, name = "Log Number of Cases") +
                scale_fill_gradientn(colours = hcl.colors(2,palette = "Emrld", rev = T, fixup = T))
            ggplotly(p1,tooltip = c("text"))
        }
        
    })
    output$bar <- renderPlotly({
        dfff <- df_table()
        #print(input$County1212)
        dta <- dfff %>% filter(state == 'Florida') %>% select(date, county, cases, deaths)  %>% mutate(date=ymd(date))
        dta <- dta %>% filter(county %in% req(input$County1212))
        #print(head(dta))
        # recent case by county
        county_recent <- dta %>% group_by(county, date) %>%
            summarise(Confirmed = sum(cases),
                      Deaths = sum(deaths)) %>%
            mutate('New Case' = Confirmed - lag(Confirmed, 1)) %>%
            filter(date == max(date))
        # aggregate in date level
        ts_all_date_county <- dta %>%
            rename(County = county) %>%
            group_by(date) %>%
            summarise(Cases = sum(cases),
                      Deaths = sum(deaths)) %>%
            mutate('New Case' = Cases - lag(Cases, 1))
        ts_recent_county <- ts_all_date_county %>%
            filter(date == max(date))
        # cumulative deaths in all florida counties
        ts_date_long_county <- ts_all_date_county %>% 
            select(-c(Cases, `New Case`)) %>%
            pivot_longer(cols = -date, names_to = "Status", values_to = "Cases")
        # creating barchart of cases and deaths in all florida counties
        barchart <- ggplot(data = ts_all_date_county, aes(x = date)) +
            geom_bar(aes(y = Cases), position = "dodge", stat = "identity", fill = "#d47274") +
            geom_bar(data = ts_date_long_county, aes(y = Cases, fill = Status), position = "dodge", stat = "identity") +
            scale_fill_manual(values = c("#000000", "#d47274")) +
            scale_y_continuous(labels = scales::label_number_si(accuracy = 0.1)) +
            #theme_solarized(base_size = 10, light = TRUE)+
            theme(plot.margin = margin(0, 0, 0, 0, "pt"),
                  panel.background = element_rect(fill = "White"),
                  legend.position = "bottom",
                  axis.title = element_blank(),
                  #axis.text.y = element_blank(),
                  axis.ticks = element_blank()) +
            ggtitle("US County COVID-19 Cumulative Cases by Day")
        return(ggplotly(barchart, height = 530) %>% 
                   layout(legend = list(orientation = 'h')))
    })

	output$annomate <- renderImage({
    dff <- df_table()
    dff <- dff %>% filter(state == 'Florida') %>% select(date, county, cases, deaths) %>% mutate(date=ymd(date))
    # recent case by county
    county_recent <- dff %>% group_by(county, date) %>%
      summarise(Confirmed = sum(cases),
                Deaths = sum(deaths)) %>%
      mutate('New Case' = Confirmed - lag(Confirmed, 1)) %>%
      filter(date == max(date))
    # aggregate in date level
    ts_all_date_county <- dff %>%
      rename(County = county) %>%
      group_by(date) %>%
      summarise(Cases = sum(cases),
                Deaths = sum(deaths)) %>%
      mutate('New Case' = Cases - lag(Cases, 1))
    ts_recent_county <- ts_all_date_county %>%
      filter(date == max(date))
    ts_increment_long_county <- ts_all_date_county %>%
      select(-`New Case`) %>%
      mutate(Cases = Cases - lag(Cases,1),
             Deaths = Deaths - lag(Deaths,1)) %>%
      filter(date != min(date)) %>%
      pivot_longer(-date, names_to = "Case", values_to = "Increment")
    # TS for florida

	})
    
 })
