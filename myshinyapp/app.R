library(bslib)
library(geojsonio)
library(leaflet)
library(shiny)
library(shinyWidgets)
library(sp)

load(file = "projectData.rdata") #loads full.df and states (see documentation for details)

sex <- df$Gender
race <- df$Race_Ethnicity

# Define UI for application
ui <- fluidPage(
    theme = bs_theme(version = 4, bootswatch = "minty"),
    
    
    titlePanel(
        "Heart Disease Mortality Data Among US Adults (35+) by State – 2016-2018"
    ),
    
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput(
                "sexInput",
                "Display values by sex",
                choices = unique(sex),
                # choices = c("male","female"),
                selected = unique(sex)
                # selected = "male"
            ),
            checkboxGroupInput(
                "raceInput",
                "Display values by race",
                choices = unique(race),
                selected = unique(race)
            )
        ),
        mainPanel(tabsetPanel(
            type = "tabs",
            tabPanel("Map", leafletOutput("map")),
            tabPanel("Data", tableOutput("table"))
        ))
    )
)

# Define server logic
server <- function(input, output) {
    US_State_data <- reactive({
        df = subset(
            full.df,
            full.df$Gender %in% input$sexInput &
                full.df$Race_Ethnicity %in% input$raceInput
        )
        
        DataLookup <-
            aggregate(df$Data_Value, by = list(df$LocationDesc), sum)
        colnames(DataLookup) = c("State", "Data_Value")
        
        data_values <- DataLookup$Data_Value
        state_names <- DataLookup$State
        names(data_values) <- state_names
        
        
        US_States <-
            states[states$NAME %in% full.df$LocationDesc,]
        US_States$DataValue <- data_values[US_States$NAME]
        
        
        return(US_States)
    })
    
    labels_data <- reactive({
        labels <- sprintf(
            "<strong>%s</strong><br/>%g deaths per 100,000 population</sup>",
            US_State_data()$NAME,
            US_State_data()$DataValue
        ) %>% lapply(htmltools::HTML)
        return(labels)
    })
    
    pal_data <- reactive({
        # bins <- c(0, 200, 300, 400, 500, 1000, 2000, 3000, 4000, 5000)
        pal <-
            colorNumeric("YlOrRd",
                         domain = US_State_data()$DataValue)
        return(pal)
    })
    
    output$table <- renderTable({
        tab <- US_State_data()
        out <- tab[, c("NAME", "DataValue")]
        out = as.data.frame(out)
        colnames(out) = c("State",
                          "Deaths by Heart Disease (per 100,000 population) ")
        out
    })
    
    output$map <- renderLeaflet({
        leaflet(US_State_data()) %>%
            setView(-96, 37.8, 4) %>% addPolygons(
                fillColor = ~ pal_data()(DataValue),
                weight = 2,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE
                ),
                label = labels_data(),
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"
                )
            ) %>%
            addLegend(
                pal = pal_data(),
                values = ~ DataValue,
                opacity = 0.7,
                title = NULL,
                position = "bottomright"
            )
    })
}

# Run the application
shinyApp(ui = ui, server = server)