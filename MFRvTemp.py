import pandas as pd
import numpy as np
import math

# Physical constants
SURFACE_AREA = math.pi * ((0.05/2) ** 2)  # Test surface area (m²)
R_AIR = 287.05  # Specific gas constant for air (J/kg·K)
ORBITAL_VELOCITY = 8000  # Typical VLEO orbital velocity (m/s)


def load_atmospheric_data(csv_file='data/msise_atmospheric_data_clean.csv'):
    """
    Load and process MSIS-E atmospheric density data.
    
    Input:
        csv_file (str): Path to atmospheric data CSV with columns:
                       ['Altitude_km', 'Total_mass_density', ...]
    
    Output:
        DataFrame: Atmospheric data with added 'density_kg_m3' column
    """
    df = pd.read_csv(csv_file)
    df['density_kg_m3'] = df['Total_mass_density'] * 1000
    return df


def target_altitude_conditions(altitude_km, csv_file='data/msise_atmospheric_data_clean.csv', temp_k=300):
    """
    Calculate required chamber conditions to simulate a target VLEO altitude.
    
    This function determines the chamber pressure, mass flow rate, and dynamic pressure
    needed to replicate atmospheric density at the specified altitude.
    
    Inputs:
        altitude_km (float): Target altitude to simulate (100-500 km typical range)
        csv_file (str): Path to atmospheric data CSV
        temp_k (float): Chamber gas temperature (K) - used for pressure calculation only
    
    Outputs:
        dict: Contains:
            - altitude_km: Actual altitude from data (nearest to requested)
            - required_pressure_torr: Chamber pressure needed
            - required_velocity_ms: Nozzle exit velocity (= ORBITAL_VELOCITY for reference)
            - chamber_temp_k: Gas temperature used
            - expected_mfr_kg_s: Mass flow rate at orbital velocity
            - dynamic_pressure_pa: Dynamic pressure at orbital conditions
            - target_density_kg_m3: Atmospheric density at altitude
    """
    df = load_atmospheric_data(csv_file)
    
    # Find closest altitude in database
    closest_idx = np.argmin(np.abs(df['Altitude_km'] - altitude_km))
    target_row = df.iloc[closest_idx]
    actual_altitude = target_row['Altitude_km']
    
    target_density = target_row['density_kg_m3']
    target_velocity = ORBITAL_VELOCITY
    
    # Calculate required chamber conditions
    required_pressure_pa = target_density * R_AIR * temp_k
    required_torr = required_pressure_pa / 133.322
    
    expected_mfr = target_density * target_velocity * SURFACE_AREA
    dynamic_pressure = 0.5 * target_density * target_velocity**2
    
    print(f"\nTARGET ALTITUDE: {actual_altitude:.1f} km")
    print(f"  Required Chamber Pressure: {required_torr:.2e} torr")
    print(f"  Target Density:            {target_density:.2e} kg/m³")
    print(f"  Expected MFR (at 8000 m/s): {expected_mfr:.2e} kg/s")
    print(f"  Orbital Dynamic Pressure:  {dynamic_pressure:.2e} Pa\n")
    
    return {
        'altitude_km': actual_altitude,
        'required_pressure_torr': required_torr,
        'required_velocity_ms': target_velocity,
        'chamber_temp_k': temp_k,
        'expected_mfr_kg_s': expected_mfr,
        'dynamic_pressure_pa': dynamic_pressure,
        'target_density_kg_m3': target_density
    }


def verify_test_conditions(measured_mfr_kg_s, nozzle_velocity_ms, 
                          csv_file='data/msise_atmospheric_data_clean.csv',
                          cubesat_area_m2=0.01, drag_coefficient=2.2):
    """
    Verify actual test conditions and calculate drag measurement capability.
    
    This is the primary verification function for the VLEO aerodynamic testing chamber.
    It calculates the simulated altitude from measured parameters and assesses the
    feasibility of drag force measurement on the magnetic levitation platform.
    
    NOTE: The drag_coefficient parameter is a REFERENCE VALUE for estimating expected
    forces. The actual purpose of the test chamber is to MEASURE the real C_D 
    experimentally, since C_D in VLEO is:
    - Unknown for most satellite geometries
    - Variable due to surface degradation (atomic oxygen)
    - Dependent on accommodation coefficients that change with material erosion
    - Affected by flow regime transitions (continuum → free molecular)
    
    Inputs:
        measured_mfr_kg_s (float): Measured mass flow rate through nozzle (kg/s)
        nozzle_velocity_ms (float): Nozzle exit velocity (m/s), typically 1350 m/s
        csv_file (str): Path to atmospheric data CSV
        cubesat_area_m2 (float): CubeSat cross-sectional area (m²), default 0.01 m² (10cm×10cm)
        drag_coefficient (float): REFERENCE C_D for force estimation, default 2.2
                                  (Actual C_D will be measured experimentally)
    
    Outputs:
        dict: Contains:
            - simulated_altitude_km: Altitude being simulated based on density match
            - actual_density_kg_m3: Calculated chamber density
            - actual_velocity_ms: Nozzle velocity (input parameter)
            - actual_mfr_kg_s: Mass flow rate (input parameter)
            - actual_dynamic_pressure_pa: Dynamic pressure in chamber
            - orbital_equivalent_dynamic_pressure_pa: Dynamic pressure at orbital velocity
            - density_error_percent: Error between actual and target density
            - velocity_error_percent: Velocity deficit compared to orbital
            - molecular_flux_kg_m2_s: Molecular flux for erosion rate calculations
            - expected_drag_force_n: ESTIMATED drag force on CubeSat (N) using reference C_D
            - orbital_drag_force_n: Equivalent drag force at orbital velocity (N)
            - drag_scaling_factor: Multiplier to convert chamber drag to orbital
            - drag_measurable: Boolean indicating if drag is measurable
    """
    # Calculate actual density from mass conservation
    actual_density = measured_mfr_kg_s / (nozzle_velocity_ms * SURFACE_AREA)
    actual_dynamic_pressure = 0.5 * actual_density * nozzle_velocity_ms**2
    
    df = load_atmospheric_data(csv_file)
    
    # Find altitude that matches our DENSITY
    closest_idx = np.argmin(np.abs(df['density_kg_m3'] - actual_density))
    simulated_altitude = df.iloc[closest_idx]['Altitude_km']
    target_density = df.iloc[closest_idx]['density_kg_m3']
    
    density_error = (actual_density - target_density) / target_density * 100
    velocity_error = (nozzle_velocity_ms - ORBITAL_VELOCITY) / ORBITAL_VELOCITY * 100
    
    # Calculate orbital equivalence
    orbital_dynamic_pressure = 0.5 * actual_density * ORBITAL_VELOCITY**2
    target_dynamic_pressure = 0.5 * target_density * ORBITAL_VELOCITY**2
    
    # Calculate molecular flux
    molecular_flux = actual_density * nozzle_velocity_ms
    
    # Calculate drag forces
    expected_drag_force = 0.5 * drag_coefficient * actual_density * nozzle_velocity_ms**2 * cubesat_area_m2
    orbital_drag_force = 0.5 * drag_coefficient * actual_density * ORBITAL_VELOCITY**2 * cubesat_area_m2
    drag_scaling_factor = (ORBITAL_VELOCITY / nozzle_velocity_ms) ** 2
    
    # Assess if drag is measurable (>100 µN is feasible with lab sensors)
    drag_measurable = expected_drag_force > 1e-4
    
    print(f"\n{'='*70}")
    print(f"VLEO AERODYNAMIC TESTING CHAMBER - TEST VERIFICATION")
    print(f"{'='*70}")
    
    print(f"\nMEASURED CONDITIONS:")
    print(f"  Mass Flow Rate:       {measured_mfr_kg_s:.2e} kg/s")
    print(f"  Nozzle Velocity:      {nozzle_velocity_ms:.0f} m/s")
    print(f"  Calculated Density:   {actual_density:.2e} kg/m³")
    print(f"  Molecular Flux:       {molecular_flux:.2e} kg/m²·s")
    
    print(f"\nSIMULATED ALTITUDE:     {simulated_altitude:.1f} km")
    print(f"  Target Density:       {target_density:.2e} kg/m³")
    print(f"  Density Match:        {density_error:+.1f}% error")
    
    print(f"\nDYNAMIC PRESSURE:")
    print(f"  Chamber (at {nozzle_velocity_ms} m/s):  {actual_dynamic_pressure:.2e} Pa")
    print(f"  Orbital (at {ORBITAL_VELOCITY} m/s):    {orbital_dynamic_pressure:.2e} Pa")
    print(f"  Ratio:                        {(actual_dynamic_pressure/orbital_dynamic_pressure)*100:.2f}%")
    
    print(f"\nDRAG FORCE MEASUREMENT:")
    print(f"  CubeSat Area:         {cubesat_area_m2} m² (C_D = {drag_coefficient})")
    print(f"  Expected Drag Force:  {expected_drag_force:.2e} N ({expected_drag_force*1e6:.1f} µN)")
    print(f"  Orbital Drag Force:   {orbital_drag_force:.2e} N ({orbital_drag_force*1e3:.2f} mN)")
    print(f"  Scaling Factor:       {drag_scaling_factor:.1f}×")
    
    if drag_measurable:
        print(f"  ✅ MEASURABLE - Force within sensor range")
        print(f"     (Requires precision force balance with <10 µN resolution)")
    else:
        print(f"  ⚠️  CHALLENGING - Force below typical sensor threshold")
        print(f"     (Consider increasing velocity or using material testing mode)")
    
    print(f"\n{'='*70}\n")
    
    return {
        'simulated_altitude_km': simulated_altitude,
        'actual_density_kg_m3': actual_density,
        'actual_velocity_ms': nozzle_velocity_ms,
        'actual_mfr_kg_s': measured_mfr_kg_s,
        'actual_dynamic_pressure_pa': actual_dynamic_pressure,
        'orbital_equivalent_dynamic_pressure_pa': orbital_dynamic_pressure,
        'density_error_percent': density_error,
        'velocity_error_percent': velocity_error,
        'molecular_flux_kg_m2_s': molecular_flux,
        'expected_drag_force_n': expected_drag_force,
        'orbital_drag_force_n': orbital_drag_force,
        'drag_scaling_factor': drag_scaling_factor,
        'drag_measurable': drag_measurable
    }


def calculate_drag_coefficient(measured_drag_force_n, density_kg_m3, 
                               velocity_ms, area_m2):
    """
    Calculate drag coefficient from measured drag force.
    
    This is a key capability of the test chamber - measuring C_D experimentally
    rather than assuming a theoretical value. In VLEO, C_D is NOT constant due to:
    - Flow regime transitions (continuum → transitional → free molecular)
    - Surface degradation from atomic oxygen erosion
    - Accommodation coefficient changes with surface chemistry
    - Temperature-dependent gas-surface interactions
    
    Inputs:
        measured_drag_force_n (float): Measured drag force from maglev platform (N)
        density_kg_m3 (float): Chamber atmospheric density (kg/m³)
        velocity_ms (float): Nozzle exit velocity (m/s)
        area_m2 (float): CubeSat reference area (m²)
    
    Outputs:
        float: Measured drag coefficient C_D (dimensionless)
    
    Example:
        >>> # Measure drag on 10cm×10cm face at 1350 m/s, ρ=9.27e-9
        >>> F_measured = 186e-6  # 186 µN measured on force sensor
        >>> C_D = calculate_drag_coefficient(F_measured, 9.27e-9, 1350, 0.01)
        >>> print(f"Measured C_D: {C_D:.2f}")
        Measured C_D: 2.20
    """
    dynamic_pressure = 0.5 * density_kg_m3 * velocity_ms**2
    C_D_measured = measured_drag_force_n / (dynamic_pressure * area_m2)
    
    return C_D_measured


def compare_drag_coefficients(pristine_force_n, degraded_force_n, 
                              density_kg_m3, velocity_ms, area_m2,
                              exposure_time_hours):
    """
    Compare drag coefficients before and after surface degradation.
    
    This function quantifies how atomic oxygen exposure changes aerodynamic
    properties - a key unknown in VLEO satellite design.
    
    Inputs:
        pristine_force_n (float): Drag force on pristine surface (N)
        degraded_force_n (float): Drag force after AO exposure (N)
        density_kg_m3 (float): Chamber density (kg/m³)
        velocity_ms (float): Nozzle velocity (m/s)
        area_m2 (float): Reference area (m²)
        exposure_time_hours (float): Duration of AO exposure
    
    Outputs:
        dict: Contains C_D values and degradation metrics
    """
    C_D_pristine = calculate_drag_coefficient(pristine_force_n, density_kg_m3, 
                                             velocity_ms, area_m2)
    C_D_degraded = calculate_drag_coefficient(degraded_force_n, density_kg_m3,
                                             velocity_ms, area_m2)
    
    C_D_increase_percent = ((C_D_degraded - C_D_pristine) / C_D_pristine) * 100
    drag_increase_percent = ((degraded_force_n - pristine_force_n) / pristine_force_n) * 100
    
    print(f"\nSURFACE DEGRADATION ANALYSIS")
    print(f"="*70)
    print(f"Exposure time: {exposure_time_hours:.1f} hours")
    print(f"\nPristine Surface:")
    print(f"  Drag force: {pristine_force_n*1e6:.1f} µN")
    print(f"  C_D:        {C_D_pristine:.3f}")
    print(f"\nDegraded Surface (after AO exposure):")
    print(f"  Drag force: {degraded_force_n*1e6:.1f} µN")
    print(f"  C_D:        {C_D_degraded:.3f}")
    print(f"\nChange:")
    print(f"  ΔC_D:       {C_D_degraded - C_D_pristine:+.3f} ({C_D_increase_percent:+.1f}%)")
    print(f"  Δ Drag:     {(degraded_force_n - pristine_force_n)*1e6:+.1f} µN ({drag_increase_percent:+.1f}%)")
    print(f"\nImplication for orbital lifetime:")
    if drag_increase_percent > 0:
        lifetime_reduction = (100 / (100 + drag_increase_percent) - 1) * 100
        print(f"  Orbit decay rate increases by {drag_increase_percent:.1f}%")
        print(f"  Mission lifetime reduced by ~{abs(lifetime_reduction):.1f}%")
    print(f"="*70)
    
    return {
        'C_D_pristine': C_D_pristine,
        'C_D_degraded': C_D_degraded,
        'C_D_change_percent': C_D_increase_percent,
        'drag_change_percent': drag_increase_percent
    }


# Example usage for 127 km altitude simulation
if __name__ == "__main__":
    print("\n" + "="*70)
    print("VLEO AERODYNAMIC TESTING CHAMBER")
    print("GWU Systems Engineering Capstone Project")
    print("="*70)
    
    # Verify test conditions at 1350 m/s for drag measurement
    results = verify_test_conditions(
        measured_mfr_kg_s=1.09e-8,
        nozzle_velocity_ms=1350,
        cubesat_area_m2=0.01,  # 10cm × 10cm face of 3U CubeSat
        drag_coefficient=2.2   # Reference value for force estimation
    )
    
    print("\n" + "="*70)
    print("EXAMPLE: MEASURING DRAG COEFFICIENT FROM EXPERIMENTAL DATA")
    print("="*70)
    
    # Simulate experimental measurement
    print("\nScenario: Measure actual C_D of CubeSat in chamber")
    print("  - Chamber conditions: 127 km simulation, 1350 m/s")
    print("  - Density: 9.27e-9 kg/m³ (matched to altitude)")
    print("  - CubeSat area: 0.01 m² (10cm × 10cm)")
    
    # Simulate force sensor reading
    measured_force = 186e-6  # 186 µN from force sensor on maglev
    
    C_D_measured = calculate_drag_coefficient(
        measured_drag_force_n=measured_force,
        density_kg_m3=results['actual_density_kg_m3'],
        velocity_ms=1350,
        area_m2=0.01
    )
    
    print(f"\nMeasured drag force: {measured_force*1e6:.1f} µN")
    print(f"Calculated C_D:      {C_D_measured:.3f}")
    print(f"\nThis measured C_D can now be used to predict orbital drag:")
    
    orbital_drag = 0.5 * C_D_measured * results['actual_density_kg_m3'] * 8000**2 * 0.01
    print(f"  Predicted orbital drag: {orbital_drag*1e3:.2f} mN")
    print(f"  (using MEASURED C_D, not assumed value)")
    
    # Example: Surface degradation study
    print("\n" + "="*70)
    print("EXAMPLE: TRACKING C_D CHANGE DUE TO SURFACE DEGRADATION")
    print("="*70)
    
    # Simulate before/after measurements
    pristine_force = 186e-6    # µN on Day 0
    degraded_force = 205e-6    # µN after 100 hours AO exposure
    
    degradation_results = compare_drag_coefficients(
        pristine_force_n=pristine_force,
        degraded_force_n=degraded_force,
        density_kg_m3=results['actual_density_kg_m3'],
        velocity_ms=1350,
        area_m2=0.01,
        exposure_time_hours=100
    )
