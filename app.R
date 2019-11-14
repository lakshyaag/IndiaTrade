library(tidyverse)
library(plotly)
library(shiny)
library(shinythemes)

#### Import data and preliminaries ####

export <-
    read.csv('2018-2010_export.csv') %>% mutate(value = replace_na(value, 0))
import <-
    read.csv('2018-2010_import.csv') %>% mutate(value = replace_na(value, 0))

# Create list of commodites
export_com <-
    export %>% select(HSCode, Commodity) %>% mutate(comname = str_c(HSCode, " - ", Commodity)) %>% select(comname) %>% unique()
import_com <-
    import %>% select(HSCode, Commodity) %>% mutate(comname = str_c(HSCode, " - ", Commodity)) %>% select(comname) %>% unique()

list_of_commodities <-
    unique((export_com %>% rbind(import_com))$comname)

# Create list of countries
export_country <- export %>% select(country) %>% unique()
import_country <- import %>% select(country) %>% unique()

list_of_countries <-
    unique((export_country %>% rbind(import_country))$country)

form_yaxis <- list(tickfont = list(size = 14))



#### Plotting functions start here ####

export_commodity_average <- function(HSC) {
    comtrend <- export %>% group_by(HSCode, Commodity, year) %>%
        summarise(`Average Value` = mean(value)) %>%
        filter(HSCode %in% HSC)
    
    g <-
        ggplot(comtrend, aes(x = year, y = `Average Value`, color = Commodity)) +
        geom_line() +
        labs(
            y = 'Value (in million US$)',
            x = 'Year',
            title = paste0(
                '<b>Exports of commodity</b>',
                '<br><sup>',
                'Average value of exports (in million US$)',
                '</sup>'
            )
        )
    
    
    p <-
        ggplotly(g,
                 dynamicTicks = TRUE,
                 tooltip = c('colour', 'y')) %>% layout(autosize = TRUE,
                                                        hovermode = 'compare',
                                                        yaxis = form_yaxis) %>% hide_legend() %>% config(displayModeBar = F)
    
    return(p)
}

import_commodity_average <- function(HSC) {
    comtrend <- import %>% group_by(HSCode, Commodity, year) %>%
        summarise(`Average Value` = mean(value)) %>%
        filter(HSCode %in% HSC)
    
    g <-
        ggplot(comtrend, aes(x = year, y = `Average Value`, color = Commodity)) +
        geom_line() +
        labs(
            y = 'Value  (in million US$)',
            x = 'Year',
            title = paste0(
                '<b>Imports of commodity</b>',
                '<br><sup>',
                'Average value of imports (in million US$)',
                '</sup>'
            )
        )
    
    p <-
        ggplotly(g,
                 dynamicTicks = TRUE,
                 tooltip = c('colour', 'y')) %>% layout(autosize = TRUE,
                                                        hovermode = 'compare',
                                                        yaxis = form_yaxis) %>% hide_legend() %>% config(displayModeBar = F)
    
    return(p)
}

top_export_countries <- function(HSC) {
    country_trend <- export %>% filter(HSCode %in% HSC) %>%
        group_by(year, HSCode) %>% top_n(1, value)
    
    g <-
        ggplot(country_trend, aes(x = year, y = value, fill = country)) +
        geom_col() +
        facet_wrap( ~ Commodity, scales = 'free', dir = 'v') +
        labs(
            y = 'Value  (in million US$)',
            x = 'Year',
            title = paste0('<b>Top countries exported to</b>'),
            fill = 'Country'
        ) +
        scale_fill_discrete()
    
    p <-
        ggplotly(g, dynamicTicks = TRUE) %>% layout(autosize = T) %>% config(displayModeBar = F)
    return(p)
}

top_import_countries <- function(HSC) {
    country_trend <- import %>% filter(HSCode %in% HSC) %>%
        group_by(year, HSCode) %>% top_n(1, value)
    
    g <-
        ggplot(country_trend, aes(x = year, y = value, fill = country)) +
        geom_col() +
        facet_wrap( ~ Commodity, scales = 'free', dir = 'v') +
        labs(
            y = 'Value  (in million US$)',
            x = 'Year',
            title = paste0('<b>Top countries imported from</b>'),
            fill = 'Country'
        ) +
        scale_fill_discrete()
    
    p <-
        ggplotly(g, dynamicTicks = TRUE) %>% layout(autosize = T) %>% config(displayModeBar = F)
    return(p)
}

plot_totals <- function(u_country) {
    export_country <- export %>% filter(country == u_country)
    import_country <- import %>% filter(country == u_country)
    
    totals <- export_country %>% group_by(year) %>%
        summarise(value = sum(value)) %>%
        inner_join((
            import_country %>%
                group_by(year) %>%
                summarise(value = sum(value))
        ), by = 'year') %>%
        rename('Exports' = value.x, 'Imports' = value.y) %>%
        mutate('Balance' = Exports - Imports) %>%
        gather(key = 'trade_flow', value = 'value',-year)
    
    
    g <- ggplot(totals, aes(x = year, y = value, fill = trade_flow)) +
        geom_col(position = position_dodge2()) +
        scale_fill_manual(
            values = c(
                'Balance' = 'grey50',
                'Exports' = 'lightslateblue',
                'Imports' = 'firebrick2'
            )
        ) +
        labs(
            y = 'Value  (in million US$)',
            x = 'Year',
            title = paste0('<b>Trade flow with ', u_country, '</b>'),
            fill = ''
        ) +
        scale_y_continuous(labels = scales::comma)
    
    
    p <-
        ggplotly(g, tooltip = 'y') %>% layout(autosize = T) %>% config(displayModeBar = F)
    
    return(p)
}

export_country_commodity <- function(u_country, u_HSC) {
    export_country <-
        export %>% filter(country == u_country) %>% filter(HSCode %in% u_HSC)
    
    g <-
        ggplot(export_country %>% rename(Value = value),
               aes(x = year, y = Value, color = Commodity)) +
        geom_line() +
        labs(
            y = 'Value  (in million US$)',
            x = 'Year',
            title = paste0('<b>Exports</b> to <b>', u_country, '</b>')
        )
    
    p <-
        ggplotly(g,
                 dynamicTicks = TRUE,
                 tooltip = c('colour', 'y')) %>% layout(autosize = TRUE,
                                                        hovermode = 'compare',
                                                        yaxis = form_yaxis) %>% hide_legend() %>% config(displayModeBar = F)
    return(p)
    
}

import_country_commodity <- function(u_country, u_HSC) {
    import_country <-
        import %>% filter(country == u_country) %>% filter(HSCode %in% u_HSC)
    
    g <-
        ggplot(import_country %>% rename(Value = value),
               aes(x = year, y = Value, color = Commodity)) +
        geom_line() +
        labs(
            y = 'Value  (in million US$)',
            x = 'Year',
            title = paste0('<b>Imports</b> from <b>', u_country, '</b>')
        )
    
    p <-
        ggplotly(g,
                 dynamicTicks = TRUE,
                 tooltip = c('colour', 'y')) %>% layout(autosize = T,
                                                        hovermode = 'compare',
                                                        yaxis = form_yaxis) %>% hide_legend() %>% config(displayModeBar = F)
    return(p)
    
}


#### App starts here ####

# Define UI
ui <- fluidPage(
    theme = shinytheme('flatly'),
    
    titlePanel("India Trade Dashboard"),
    
    tabsetPanel(
        tabPanel('Overview',
                 fluidRow(
                     column(
                         12,
                         h3('Click on the tabs above to get started.'),
                         br(),
                         wellPanel(
                             h3(
                                 "This dashboard visualises the multilateral trade flow of India between 2010 and 2018"
                             ),
                             h5(
                                 '1. Commodity-wise shows the flow of trade based on selected commodities.
                                Specifically, it shows the trend of exports/imports and the top countries for the selected commodities.'
                             ),
                             h5(
                                 '2. Country-wise shows the flow of trade with the selected country.
                                Specifically, it shows the trade balance with the country and allows for commodity-level information as well.'
                             )
                         ),
                         br(),
                         wellPanel(
                             HTML(
                                 '<h4>Created by <a href = "https://github.com/lakshyaag/" target = "_blank">Lakshya Agarwal.</a></h4>'
                             ),
                             h4(
                                 'All values are in US$ million. The information has been gathered from Deparment of Commerce, Govt. of India.'
                             ),
                             HTML(
                                 '<h6>For more information on how the data was collected,
                                     click <a href=https://github.com/lakshyaag/India-Trade-Data/>here</a></h6>'
                             )
                         )
                     )
                 )),
        tabPanel(
            'Commodity-level',
            fluidRow(column(
                12,
                selectizeInput(
                    'commodity_select',
                    'Commodity',
                    choices = NULL,
                    width = '100%',
                    multiple = T,
                    options = list(maxItems = 5, placeholder = 'Select commodities (max. 5)')
                )
            )),
            
            fluidRow(column(
                6, plotlyOutput('export_commodity_average')
            ),
            column(
                6, plotlyOutput('import_commodity_average')
            )),
            
            fluidRow(column(6, plotlyOutput(
                'top_export_countries'
            )),
            column(6, plotlyOutput(
                'top_import_countries'
            )))
        ),
        tabPanel(
            'Country-level',
            fluidRow(column(
                3,
                selectizeInput(
                    'country_select',
                    'Country',
                    choices = NULL,
                    width = '100%',
                    multiple = F,
                    options = list(placeholder = 'Select country')
                )
            ),
            column(
                9,
                selectizeInput(
                    'country_commodity_select',
                    'Commodity',
                    choices = NULL,
                    width = '100%',
                    multiple = T,
                    options = list(maxItems = 5, placeholder = 'Select commodities (max. 5)')
                )
            )),
            fluidRow(column(12,
                            plotlyOutput('plot_totals'))),
            fluidRow(column(
                6,
                plotlyOutput('export_country_commodity')
            ),
            column(
                6,
                plotlyOutput('import_country_commodity')
            ))
        )
    )
)

# Define server
server <- function(input, output, session) {
    updateSelectizeInput(
        session,
        'commodity_select',
        choices = list_of_commodities,
        server = TRUE,
        selected = c(list_of_commodities[35], list_of_commodities[25])
    )
    
    updateSelectizeInput(
        session,
        'country_select',
        choices = list_of_countries,
        server = TRUE,
        selected = list_of_countries[1]
    )
    
    updateSelectizeInput(
        session,
        'country_commodity_select',
        choices = list_of_commodities,
        server = TRUE,
        selected = c(list_of_commodities[35], list_of_commodities[25])
    )
    
    selectedHSC <-
        reactive({
            as.integer(str_extract(input$commodity_select, '(\\d*)'))
        })
    selectedHSC_country <-
        reactive({
            as.integer(str_extract(input$country_commodity_select, '(\\d*)'))
        })
    
    output$export_commodity_average <- renderPlotly({
        export_commodity_average({
            selectedHSC()
        })
    })
    
    output$import_commodity_average <- renderPlotly({
        import_commodity_average({
            selectedHSC()
        })
    })
    
    output$top_export_countries <- renderPlotly({
        top_export_countries({
            selectedHSC()
        })
    })
    
    output$top_import_countries <- renderPlotly({
        top_import_countries({
            selectedHSC()
        })
    })
    
    output$plot_totals <- renderPlotly({
        plot_totals(input$country_select)
    })
    
    output$export_country_commodity <- renderPlotly({
        export_country_commodity(input$country_select, {
            selectedHSC_country()
        })
    })
    
    output$import_country_commodity <- renderPlotly({
        import_country_commodity(input$country_select, {
            selectedHSC_country()
        })
    })
}

# Run the application
shinyApp(ui = ui, server = server)
