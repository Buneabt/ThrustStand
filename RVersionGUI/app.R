# VLEO Chamber Verification - Shiny App
# GWU Systems Engineering Capstone Project
library(shiny)
library(shinyjs)
library(rvest)
library(dplyr)

# Physical constants
R_AIR <- 287.05  # Specific gas constant for air (J/kg·K)
NOZZLE_AREA <- pi * (0.09238/2)^2 

# Load Earth atmospheric data
load_atmospheric_data <- function(csv_file = 'data/msise_atmospheric_data_clean.csv') {
    df <- read.csv(csv_file)
    df$density_kg_m3 <- df$Total_mass_density * 1000
    return(df)
}

# Load Mars atmospheric data
load_mars_data <- function() {
    url_mars <- "http://www.braeunig.us/space/atmmars.htm"
    df <- read_html(url_mars) %>% 
        html_table() %>% 
        .[[3]] %>% 
        .[-2,]
    colnames(df) <- df[1,]
    df <- df[-1,]
    
    df$Altitude_km <- as.numeric(gsub(",", "", df$`Altitude(meters)`)) / 1000
    df$density_kg_m3 <- as.numeric(df$`Density(kg/m3)`) 
    
    return(df)
}

# Load voltage-temperature calibration data
load_voltage_temp_data <- function(csv_file = 'data/dummy_voltageVtemp.csv') {
    df <- read.csv(csv_file)
    # Convert Celsius to Kelvin
    df$Temperature_K <- df$Temp_C + 273.15
    return(df)
}

# Convert voltage to temperature
get_temp_from_voltage <- function(voltage_v, voltage_temp_data) {
    approx(voltage_temp_data$Voltage_V, voltage_temp_data$Temperature_K, voltage_v)$y
}

# Calculate exit velocity from mass flow rate
# Formula: v = (m_dot * R * T) / (P * A)
calculate_exit_velocity <- function(mass_flow_kg_s, pressure_pa, temp_k) {
    (mass_flow_kg_s * R_AIR * temp_k) / (pressure_pa * NOZZLE_AREA)
}

# Load data at startup
atm_data_earth <- load_atmospheric_data()
atm_data_mars <- load_mars_data()
voltage_temp_data <- load_voltage_temp_data()

get_altitude_for_density <- function(density_kg_m3, planet = "Earth") {
    if (planet == "Mars") {
        approx(atm_data_mars$density_kg_m3, atm_data_mars$Altitude_km, density_kg_m3)$y
    } else {
        approx(atm_data_earth$density_kg_m3, atm_data_earth$Altitude_km, density_kg_m3)$y
    }
}

get_density_for_altitude <- function(altitude_km, planet = "Earth") {
    if (planet == "Mars") {
        approx(atm_data_mars$Altitude_km, atm_data_mars$density_kg_m3, altitude_km)$y
    } else {
        approx(atm_data_earth$Altitude_km, atm_data_earth$density_kg_m3, altitude_km)$y
    }
}

ui <- fluidPage(
    useShinyjs(),
    titlePanel("VLEO Chamber Verification"),
    h4("GWU Systems Engineering Capstone Project"),
    hr(),
    
    tags$head(tags$style(HTML("
        .solve-buttons .btn {
            width: 100px;
            height: 60px;
            margin: 5px;
            font-size: 14px;
            font-weight: bold;
        }
        .solve-buttons {
            text-align: center;
            margin-bottom: 20px;
        }
        .btn-active {
            background-color: #337ab7;
            color: white;
        }
    "))),
    
    tabsetPanel(
        tabPanel("Chamber Setup",
                 fluidRow(
                     column(4,
                            wellPanel(
                                h4("Chamber Conditions"),
                                
                                selectInput("planet",
                                            "Atmospheric Body:",
                                            choices = c("Earth", "Mars"),
                                            selected = "Earth"),
                                hr(),
                                
                                h5("Solve for:"),
                                div(class = "solve-buttons",
                                    actionButton("solve_altitude", "Altitude"),
                                    actionButton("solve_voltage", "Voltage"),
                                    actionButton("solve_pressure", "Pressure")
                                ),
                                
                                hr(),
                                
                                conditionalPanel(
                                    condition = "output.solve_mode != 'pressure'",
                                    h5("Chamber Pressure (torr):"),
                                    fluidRow(
                                        column(12,
                                               numericInput("chamber_torr_text",
                                                            NULL,
                                                            value = 3)
                                        )
                                    )
                                ),
                                
                                conditionalPanel(
                                    condition = "output.solve_mode != 'voltage'",
                                    h5("Heating Cartridge Voltage (V):"),
                                    fluidRow(
                                        column(12,
                                               sliderInput("voltage_slider",
                                                           NULL,
                                                           min = 0,
                                                           max = 120,
                                                           value = 24,
                                                           step = 1)
                                        )
                                    ),
                                    fluidRow(
                                        column(12,
                                               numericInput("voltage_text",
                                                            NULL,
                                                            value = 24,
                                                            min = 0,
                                                            max = 120,
                                                            step = 1)
                                        )
                                    )
                                ),
                                
                                conditionalPanel(
                                    condition = "output.solve_mode != 'altitude'",
                                    h5("Target Altitude (km):"),
                                    fluidRow(
                                        column(12,
                                               sliderInput("target_alt_slider",
                                                           NULL,
                                                           min = 0,
                                                           max = 400,
                                                           value = 127,
                                                           step = 1)
                                        )
                                    ),
                                    fluidRow(
                                        column(12,
                                               numericInput("target_alt_text",
                                                            NULL,
                                                            value = 127,
                                                            min = 0,
                                                            max = 400,
                                                            step = 1)
                                        )
                                    )
                                ),
                                
                                hr(),
                                p("Density = P/(R × T)", 
                                  style = "font-size: 0.9em; color: #666;"),
                                p("T from voltage-temperature calibration", 
                                  style = "font-size: 0.8em; color: #999;")
                            )
                     ),
                     
                     column(8,
                            h4("Results"),
                            
                            conditionalPanel(
                                condition = "output.solve_mode == 'altitude'",
                                wellPanel(
                                    h4("Simulated Altitude"),
                                    h3(textOutput("altitude"), style = "color: #0066cc;")
                                )
                            ),
                            
                            conditionalPanel(
                                condition = "output.solve_mode == 'voltage'",
                                wellPanel(
                                    h5("Required Heating Voltage"),
                                    h3(textOutput("required_voltage"), style = "color: #0066cc;")
                                )
                            ),
                            
                            conditionalPanel(
                                condition = "output.solve_mode == 'pressure'",
                                wellPanel(
                                    h5("Required Chamber Pressure"),
                                    h3(textOutput("required_pressure"), style = "color: #0066cc;")
                                )
                            ),
                            
                            wellPanel(
                                h4("Chamber Conditions"),
                                fluidRow(
                                    column(4,
                                           strong("Pressure (Pa):"),
                                           h4(textOutput("pressure_pa"))
                                    ),
                                    column(4,
                                           strong("Nozzle Exit Temperature (K):"),
                                           h4(textOutput("temperature_k"))
                                    ),
                                    column(4,
                                           strong("Density (kg/m³):"),
                                           h4(textOutput("density"))
                                    )
                                )
                            ),
                            
                            wellPanel(
                                h4("Nozzle Performance"),
                                fluidRow(
                                    column(6,
                                           h5("Inlet Mass Flow Rate (kg/s):"),
                                           numericInput("input_mass_flow",
                                                        NULL,
                                                        value = 0.001,
                                                        min = 0,
                                                        max = 1,
                                                        step = 0.0001)
                                    ),
                                    column(6,
                                           strong("Exit Velocity (m/s):"),
                                           h4(textOutput("exit_velocity"))
                                    )
                                ),
                                hr(),
                                p(sprintf("Nozzle exit area: %.2f mm²", NOZZLE_AREA * 1e6), 
                                  style = "font-size: 0.9em; color: #666;"),
                                p("v = (ṁ × R × T) / (P × A)", 
                                  style = "font-size: 0.8em; color: #999;")
                            )
                     )
                 )
        ),
        
        tabPanel("Chamber Results",
                 fluidRow(
                     column(12,
                            h1("Results Here")
                     )
                 )
        )
    )
)

server <- function(input, output, session) {
    
    solve_mode <- reactiveVal("altitude")
    
    observe({
        addClass("solve_altitude", "btn-active")
    })
    
    observeEvent(input$solve_altitude, {
        solve_mode("altitude")
        removeClass("solve_voltage", "btn-active")
        removeClass("solve_pressure", "btn-active")
        addClass("solve_altitude", "btn-active")
    })
    
    observeEvent(input$solve_voltage, {
        solve_mode("voltage")
        removeClass("solve_altitude", "btn-active")
        removeClass("solve_pressure", "btn-active")
        addClass("solve_voltage", "btn-active")
    })
    
    observeEvent(input$solve_pressure, {
        solve_mode("pressure")
        removeClass("solve_altitude", "btn-active")
        removeClass("solve_voltage", "btn-active")
        addClass("solve_pressure", "btn-active")
    })
    
    output$solve_mode <- reactive({ solve_mode() })
    outputOptions(output, "solve_mode", suspendWhenHidden = FALSE)
    
    observeEvent(input$chamber_torr_slider, {
        updateNumericInput(session, "chamber_torr_text", value = input$chamber_torr_slider)
    })
    
    observeEvent(input$voltage_slider, {
        updateNumericInput(session, "voltage_text", value = input$voltage_slider)
    })
    
    observeEvent(input$voltage_text, {
        updateSliderInput(session, "voltage_slider", value = input$voltage_text)
    })
    
    observeEvent(input$target_alt_slider, {
        updateNumericInput(session, "target_alt_text", value = input$target_alt_slider)
    })
    
    observeEvent(input$target_alt_text, {
        updateSliderInput(session, "target_alt_slider", value = input$target_alt_text)
    })
    
    results <- reactive({
        mode <- solve_mode()
        
        if (mode == "altitude") {
            chamber_pressure_pa <- input$chamber_torr_text * 133.322
            temp_k <- get_temp_from_voltage(input$voltage_text, voltage_temp_data)
            actual_density <- chamber_pressure_pa / (R_AIR * temp_k)
            expected_altitude <- get_altitude_for_density(actual_density, input$planet)
            
            list(
                mode = "altitude",
                altitude = expected_altitude,
                pressure_pa = chamber_pressure_pa,
                temperature_k = temp_k,
                density = actual_density,
                planet = input$planet
            )
            
        } else if (mode == "voltage") {
            target_altitude <- input$target_alt_text
            target_density <- get_density_for_altitude(target_altitude, input$planet)
            chamber_pressure_pa <- input$chamber_torr_text * 1e-6 * 133.322
            
            required_temp_k <- chamber_pressure_pa / (R_AIR * target_density)
            required_voltage <- approx(voltage_temp_data$Temperature_K, 
                                       voltage_temp_data$Voltage_V, 
                                       required_temp_k)$y
            
            list(
                mode = "voltage",
                required_voltage = required_voltage,
                required_temp_k = required_temp_k,
                pressure_pa = chamber_pressure_pa,
                density = target_density,
                planet = input$planet
            )
            
        } else {
            target_altitude <- input$target_alt_text
            target_density <- get_density_for_altitude(target_altitude, input$planet)
            temp_k <- get_temp_from_voltage(input$voltage_text, voltage_temp_data)
            
            required_pressure_pa <- target_density * R_AIR * temp_k
            required_pressure_torr <- (required_pressure_pa / 133.322) / 1e-6
            
            list(
                mode = "pressure",
                required_pressure_pa = required_pressure_pa,
                required_pressure_torr = required_pressure_torr,
                temperature_k = temp_k,
                density = target_density,
                planet = input$planet
            )
        }
    })
    
    # Calculate exit velocity
    exit_vel <- reactive({
        pressure_pa <- if(results()$mode == "pressure") {
            results()$required_pressure_pa
        } else {
            results()$pressure_pa
        }
        
        temp_k <- if(results()$mode == "voltage") {
            results()$required_temp_k
        } else {
            results()$temperature_k
        }
        
        calculate_exit_velocity(input$input_mass_flow, pressure_pa, temp_k)
    })
    
    output$altitude <- renderText({
        sprintf("%.1f km", results()$altitude)
    })
    
    output$required_voltage <- renderText({
        sprintf("%.1f V (→ %.0f K)", results()$required_voltage, results()$required_temp_k)
    })
    
    output$required_pressure <- renderText({
        sprintf("%.2e torr", results()$required_pressure_torr * 1e-6)
    })
    
    output$pressure_pa <- renderText({
        if (results()$mode == "pressure") {
            sprintf("%.2e", results()$required_pressure_pa)
        } else {
            sprintf("%.2e", results()$pressure_pa)
        }
    })
    
    output$temperature_k <- renderText({
        if (results()$mode == "voltage") {
            sprintf("%.0f", results()$required_temp_k)
        } else {
            sprintf("%.0f", results()$temperature_k)
        }
    })
    
    output$exit_velocity <- renderText({
        sprintf("%.0f m/s", exit_vel())
    })
    
    output$density <- renderText({
        sprintf("%.2e kg/m³", results()$density)
    })
}

shinyApp(ui = ui, server = server)