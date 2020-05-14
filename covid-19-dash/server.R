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
    
    
 })
