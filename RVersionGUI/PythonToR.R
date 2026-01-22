# VLEO Aerodynamic Testing Chamber - Simplified R Version
# GWU Systems Engineering Capstone Project

library(dplyr)

# Physical constants
SURFACE_AREA <- pi * ((0.05/2) ^ 2)  # Test surface area (m²)
R_AIR <- 287.05  # Specific gas constant for air (J/kg·K)
ORBITAL_VELOCITY <- 8000  # Typical VLEO orbital velocity (m/s)

# Load atmospheric data from CSV
load_atmospheric_data <- function(csv_file = 'data/msise_atmospheric_data_clean.csv') {
    df <- read.csv(csv_file)
    df$density_kg_m3 <- df$Total_mass_density * 1000
    return(df)
}

# Global variable to store atmospheric data
atm_data <- load_atmospheric_data()

get_altitude_for_density <- function(density_kg_m3) {
    approx(atm_data$density_kg_m3, atm_data$Altitude_km, density_kg_m3)$y
}

get_density_for_altitude <- function(altitude_km) {
    approx(atm_data$Altitude_km, atm_data$density_kg_m3, altitude_km)$y
}


verify_chamber_conditions <- function(chamber_torr, exit_velocity_ms, temp_k = 150) {
    #' Verify chamber conditions and calculate expected altitude
    #'
    #' @param chamber_torr Chamber pressure in torr
    #' @param exit_velocity_ms Nozzle exit velocity in m/s
    #' @param temp_k Gas temperature in Kelvin (default 150K)
    #' @return List containing altitude and pressure results
    
    # Convert torr to Pa
    chamber_pressure_pa <- chamber_torr * 133.322
    
    # Calculate density from ideal gas law
    actual_density <- chamber_pressure_pa / (R_AIR * temp_k)
    
    # Calculate mass flow rate
    mfr <- actual_density * exit_velocity_ms * SURFACE_AREA
    
    # Find expected altitude based on density
    expected_altitude <- get_altitude_for_density(actual_density)
    target_density <- get_density_for_altitude(expected_altitude)
    
    # Calculate dynamic pressures
    chamber_dynamic_pressure <- 0.5 * actual_density * exit_velocity_ms^2
    orbital_dynamic_pressure <- 0.5 * actual_density * ORBITAL_VELOCITY^2
    
    # Density error
    density_error <- (actual_density - target_density) / target_density * 100
    
    return(list(
        expected_altitude_km = expected_altitude,
        chamber_pressure_torr = chamber_torr,
        chamber_pressure_pa = chamber_pressure_pa,
        chamber_density = actual_density,
        mass_flow_rate = mfr,
        chamber_dynamic_pressure = chamber_dynamic_pressure,
        orbital_dynamic_pressure = orbital_dynamic_pressure,
        density_error_percent = density_error
    ))
}


# Test at 3e-6 torr and 1350 m/s
results <- verify_chamber_conditions(
    chamber_torr = 3e-6,
    exit_velocity_ms = 1350,
    temp_k = 300
)

cat("\nChamber Verification Results:\n")
cat(sprintf("Expected Altitude: %.1f km\n", results$expected_altitude_km))
cat(sprintf("Chamber Pressure: %.2e torr (%.2e Pa)\n", 
            results$chamber_pressure_torr, results$chamber_pressure_pa))
cat(sprintf("Density: %.2e kg/m³\n", results$chamber_density))
cat(sprintf("Mass Flow Rate: %.2e kg/s\n", results$mass_flow_rate))
cat(sprintf("Chamber Dynamic Pressure: %.2e Pa\n", results$chamber_dynamic_pressure))
cat(sprintf("Orbital Dynamic Pressure: %.2e Pa\n", results$orbital_dynamic_pressure))
cat(sprintf("Chamber Pressure is X Orbital Pressure: %.5f", results$chamber_dynamic_pressure/results$orbital_dynamic_pressure))




