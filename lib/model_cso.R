##########################################################################################################
###                                                                                                    ###
### Hydrological Modeling of Combined Sewer Overflows (CSOs)                                           ###
### An R-Based Implementation of Quaranta et al. 2022                                                  ###
### by Nina Kleemeyer, B.Sc. reviewed and partly updated/rewritten by Steffen Kittlaus                 ###
###                                                                                                    ###
##########################################################################################################

cso_model <- function(
    population,                         # numeric. Population of the catchment area. Must be > 0.
    area,                               # numeric Catchment area in km². Must be > 0.
    pop_density,                        # population density in persons per km² impervious area. If missing is calculated from impervious ares and population.
    time,                               # vector containing the timestamps as posixct datatype.
    precipitation,                      # numeric. vector containing precipitation per timestep [mm].
    dwf_per_capita = 0.2,               # gDry Weather Flow (DWF) per capita [m³/day/person]. Default: 0.2 m³/day/person (Quaranta et al. 2022).
    catchment_surface_storage = 1.5,    # W0 Maximum surface storage capacity of the catchment (water retained on impervious surfaces
                                        # before runoff begins) [mm]. Default: 1.5 mm (Quaranta et al. 2022)
    rate_constant_surface_storage = 0.3,# k0 Reservoir constant for surface storage [1/timestep].Default: 0.3/(3 hours) (represents depletion of surface storage during dry periods).
    network_dwf_dilution_rate = 9.1,    # Dilution rate of the sewer network [-]. Defines the maximum network capacity relative to DWF. Default: 9.1.
    tank_dwf_dilution_rate = 9,         # Dilution rate of the tank [-]. Defines the maximum tank outflow capacity relative to DWF. Default: 9.
    network_storage = 1,                # Network storage capacity [mm]. Default: 1 mm (storage capacity of the sewer network before overflow).
    tank_storage = 0.45,                # Tank storage capacity [mm]. Default: 0.45 mm (capacity of the retention tank before overflow).
    tank_volume_init = 0.444624746      # Initial volume in the retention tank [mm]. Default: 0.444624746 mm (initial state variable for tank storage).
    ){

    ## Check input data type
    require(checkmate)
    assert_count(population)
    assert_number(area, na.ok = FALSE, lower = 0, finite = TRUE)
    if (!(area > 0)) stop(sprintf("area must be > 0, but is %s", area))
    assert_posixct(time, any.missing = FALSE, min.len = 8, unique = TRUE, null.ok = FALSE, sorted = TRUE)
    assert_numeric(precipitation, lower = 0, finite = TRUE, any.missing = FALSE, all.missing = FALSE, min.len = 8)
    assert_numeric(pop_density, lower = 0, finite = TRUE, any.missing = FALSE, all.missing = TRUE)
    assert_number(dwf_per_capita, na.ok = FALSE, lower = 0.05, upper = 2,  null.ok = FALSE)
    assert_number(catchment_surface_storage, na.ok = FALSE, lower = 0.2, upper = 5,  null.ok = FALSE)
    assert_number(rate_constant_surface_storage, na.ok = FALSE, lower = 0.05, upper = 2,  null.ok = FALSE)
    assert_number(network_dwf_dilution_rate, na.ok = FALSE, lower = 3, upper = 20,  null.ok = FALSE)
    assert_number(tank_dwf_dilution_rate, na.ok = FALSE, lower = 3, upper = 20,  null.ok = FALSE)
    assert_number(network_storage, na.ok = FALSE, lower = 0.05, upper = 5,  null.ok = FALSE)
    assert_number(tank_storage, na.ok = FALSE, lower = 0.05, upper = 5,  null.ok = FALSE)
    assert_number(tank_volume_init, na.ok = FALSE, lower = 0.05, upper = 5,  null.ok = FALSE)


    ### basic parameter calculation
    time_step <- min(diff(time))
    timesteps_per_day <- 24/as.integer(time_step)
    if(is.na(pop_density)){
    pop_density <- population / (area * 100 * 0.72)} # Personen/ha_imp # Why factor 0.72 or 0.75  (Barcelona) here? Share imperviousness?
    qdwf <- pop_density * dwf_per_capita / timesteps_per_day / 10 # mm/timestep/m²
    rate_constant_network_storage <- network_dwf_dilution_rate * qdwf / network_storage # Netzwerkspeicher Rate ((timestep)-1) (k1)
    rate_constant_tank_storage <- tank_dwf_dilution_rate * qdwf / tank_storage # Tank Rate ((timestep)-1) (k2)
    network_max_conveyance <- qdwf * network_dwf_dilution_rate # 12.75098313 # Maximum conveyance of the network (mm/timestep/m²) according to Pistocci and Dorati 2018
    network_storage_mult_rate <- network_storage * rate_constant_network_storage # Maximum conveyance of the network k1*W1

    # Create model dat object
    require(data.table)
    data <- data.table(time, precipitation, key = "time")

    ### Surface Storage S(t) ### Equation 1
    fun_surface_storage <- function(precipitation,
                                    catchment_surface_storage,
                                    rate_constant_surface_storage) {
        v <- numeric(length(precipitation))
        v[1] <- pmin(precipitation[1], catchment_surface_storage)
        for (t in 2:length(v)) {
                v[t] <- min(
                    catchment_surface_storage,
                    ifelse(
                        precipitation[t] > 0,
                        precipitation[t] + v[t - 1],
                        v[t - 1] * exp(-rate_constant_surface_storage)
                    )
                )
        }
        return(v)
        }

    data[, surface_storage := fun_surface_storage(precipitation, catchment_surface_storage, rate_constant_surface_storage)]

    ### surface_losses ### #Not defined in publication, not used later in model.
    # fun_surface_losses <- function(surface_storage,
    #                               rate_constant_surface_storage) {
    #    v <- numeric(length(surface_storage))
    #    v[1] <- 0
    #    for (t in 2:length(v)) {
    #            v[t] <- surface_storage[t - 1] * (1 - exp(-rate_constant_surface_storage))
    #        }
    #    return(v)
    #}
    # data[ ,surface_losses := fun_surface_losses(surface_storage, rate_constant_surface_storage)]

    ### Runoff (Rainfall that reaches the network) R(t) ### Equation 2
    fun_runoff <- function(precipitation,
                           surface_storage,
                           catchment_surface_storage) {
        v <- numeric(length(precipitation))
        v[1] <- pmax(0, precipitation[2] - catchment_surface_storage + surface_storage[2] - surface_storage[1]) # Not clear, why next timestep is used here. Not given in publication. Remove?
        for (t in 2:length(v)) {
            v[t] <- max(0, precipitation[t] - (catchment_surface_storage - surface_storage[t - 1]))
        }
        return(v)
        }

    data[, runoff := fun_runoff(precipitation, surface_storage, catchment_surface_storage)]

    ### Ft Network Flow ### Equation (4)
    fun_network_flow <- function(runoff,
                                 qdwf,
                                 rate_constant_network_storage) {
        v <- numeric(length(runoff))
        v[1] <- runoff[1] + qdwf
        for (t in 2:length(v)) {
            v[t] <- (runoff[t] + qdwf) * (1 - exp(-rate_constant_network_storage)) + v[t - 1] * exp(-rate_constant_network_storage)
            }
        return(v)
    }

    data[, network_flow := fun_network_flow(runoff, qdwf, rate_constant_network_storage)]

    ### Worst case overflow E(t) ### Equation 3
    data[ , worst_case_overflow := pmax(runoff + qdwf - network_storage_mult_rate, 0)]
    # Formula for Vienna more complicated. Ask Nina.


    ### Scenario in one step ### Implementation checked. In accordance with publication.
    fun_scenario_all <- function(runoff,
                                 network_flow,
                                 qdwf,
                                 network_storage_mult_rate,
                                 rate_constant_network_storage) {
        v <- character(length(network_flow))
        v[1] <- NA
        for (t in 2:length(v)) {
            A <- runoff[t] + qdwf - network_storage_mult_rate
            B <- runoff[t] + qdwf - network_flow[t - 1]
            v[t] <- fcase( ( B >= 0 ) & ( A >= B ) | ( B < 0 ) & ( A >= 0 ), "a",
                           ( B >= 0 ) & ( A < B )  &  ( A >= B * exp(-rate_constant_network_storage)), "b",
                           ( B < 0 ) & ( A >= B ) & ( A < B * exp(-rate_constant_network_storage)), "c",
                           default = "d")
        }
        return(v)
    }
    data[, scenario := fun_scenario_all(runoff, network_flow, qdwf, network_storage_mult_rate, rate_constant_network_storage)]


    ### Network overflow E'(t) ### Equation 5
    fun_network_overflow <- function(runoff,
                                     network_flow,
                                     scenario,
                                     qdwf,
                                     network_storage_mult_rate,
                                     rate_constant_network_storage) {
        v <- numeric(length(network_flow))
        v[1] <- NA_real_
        for (t in 2:length(v)) {
            A <- runoff[t] + qdwf - network_storage_mult_rate
            B <- runoff[t] + qdwf - network_flow[t - 1]
            #f1 <- max(0, B/A) # to avoid warnings for negative ratios B/A
            v[t] <- fcase(scenario[t] == "a", A                                                  - B / rate_constant_network_storage * (1 - exp( - rate_constant_network_storage)),
                          # In appendix A Et is used in this equation instead of A. As Et is max(A, 0) and scenario A is limited to A >= 0 A can be used here.
                          scenario[t] == "b", A * (1 - (1 + log(B/A)) /rate_constant_network_storage) + B / rate_constant_network_storage * exp(- rate_constant_network_storage),
                          scenario[t] == "c", A / rate_constant_network_storage * (1 + log(B/A) ) - B / rate_constant_network_storage,
                          default = 0)
        }
        return(v)
    }

    data[, network_overflow := pmax(0, fun_network_overflow(runoff, network_flow, scenario, qdwf, network_storage_mult_rate, rate_constant_network_storage))]

    ### tau1 ###
    fun_tau1 <- function(runoff,
                       network_flow,
                       scenario,
                       qdwf,
                       network_storage_mult_rate,
                       rate_constant_network_storage) {
        v <- numeric(length(network_flow))
        v[1] <- NA_real_
        for (t in 2:length(v)) {
            B <- runoff[t] + qdwf - network_flow[t - 1]
            A <- runoff[t] + qdwf - network_storage_mult_rate
            #f1 <- pmax(0, A / B)
            v[t] <- fifelse(scenario[t] == "a" | scenario[t] == "d", 0, -log(A / B) / rate_constant_network_storage)
        }
        return(v)
    }

    data[, tau1 := fun_tau1(runoff, network_flow, scenario, qdwf, network_storage_mult_rate, rate_constant_network_storage)]


    ### virtual volume and tank volume ###
    fun_virtual_volume_and_tank_volume <- function(runoff,
                                                   network_flow,
                                                   tau1,
                                                   rate_constant_tank_storage,
                                                   qdwf,
                                                   rate_constant_network_storage,
                                                   network_storage,
                                                   network_storage_mult_rate,
                                                   tank_volume_init,               # Was tank_storage in Ninas implementation. According to Barcelona it is tank volume.
                                                   scenario) {
        length_needed <- length(runoff)
        va <- numeric(length_needed)
        vd <- numeric(length_needed)
        vb_t1 <- numeric(length_needed)
        vc_t1 <- numeric(length_needed)
        vb <- numeric(length_needed)
        vc <- numeric(length_needed)
        tv <- numeric(length_needed)
        va[1] <- NA_real_
        vd[1] <- NA_real_
        vb_t1[1] <- NA_real_
        vc_t1[1] <- NA_real_
        vb[1] <- NA_real_
        vc[1] <- NA_real_
        tv[1] <- network_flow[1] / rate_constant_tank_storage

        for (t in 2:length_needed) {
                va[t] <- network_storage_mult_rate / rate_constant_tank_storage * (1 - exp(-rate_constant_tank_storage)) + tv[t - 1] * exp(-rate_constant_tank_storage)

                vd[t] <- (runoff[t] + qdwf) / rate_constant_tank_storage * (1 - exp(-rate_constant_tank_storage)) - (runoff[t] + qdwf - network_flow[t - 1]) /
                    (rate_constant_tank_storage - rate_constant_network_storage) * (exp(-rate_constant_network_storage) - exp(-rate_constant_tank_storage)) + tv[t - 1] * exp(-rate_constant_tank_storage)

                vb_t1[t] <- (runoff[t] + qdwf) / rate_constant_tank_storage * (1 - exp(-rate_constant_tank_storage * tau1[t]))-
                    (runoff[t] + qdwf - network_flow[t - 1]) / (rate_constant_tank_storage - rate_constant_network_storage) * (exp(-rate_constant_network_storage * tau1[t]) - exp(-rate_constant_tank_storage * tau1[t]))+
                    tv[t - 1] * exp(-rate_constant_tank_storage * tau1[t])

                vc_t1[t] <- rate_constant_network_storage * network_storage / rate_constant_tank_storage * (1 - exp(-rate_constant_tank_storage * 1)) + tv[t - 1] * exp(-rate_constant_tank_storage * 1)

                vb[t] <- vb_t1[t] * exp(-rate_constant_tank_storage * (1 - tau1[t])) + rate_constant_network_storage * network_storage / rate_constant_tank_storage * (1 - exp(-rate_constant_tank_storage * (1 - tau1[t])))

                vc[t] <- (runoff[t] + qdwf) / rate_constant_tank_storage * (1 - exp(-rate_constant_tank_storage * (1 - tau1[t]))) + (runoff[t] + qdwf - network_flow[t - 1]) / (rate_constant_tank_storage - rate_constant_network_storage) *
                            (exp(-rate_constant_tank_storage * (1 - tau1[t])) - exp(-rate_constant_network_storage * (1 - tau1[t]))) + vc_t1[t] * exp(-rate_constant_tank_storage * (1 - tau1[t]))

                tv[t] <- min(fcase(scenario[t] == "a", va[t],
                                    scenario[t] == "b", vb[t],
                                    scenario[t] == "c", vc[t],
                                    scenario[t] == "d", vd[t]),
                              tank_volume_init)
        }
        return(list(va, vd, vb_t1, vc_t1, vb, vc, tv))
    }
    data[, c("virtual_volume_a",
             "virtual_volume_d",
             "virtual_volume_b_t1",
             "virtual_volume_c_t1",
             "virtual_volume_b",
             "virtual_volume_c",
             "tank_volume") := fun_virtual_volume_and_tank_volume(runoff, network_flow, tau1, rate_constant_tank_storage, qdwf, rate_constant_network_storage,
                                                                  network_storage, network_storage_mult_rate, tank_volume_init, scenario)]


    ### tau2 ### equation 11 in appendix B
    fun_tau2 <- function(tank_volume,
                       rate_constant_tank_storage,
                       tank_storage,
                       network_storage_mult_rate) {
        v <- numeric(length(tank_volume))
        v[1] <- NA_real_
        for (t in 2:length(v)) {
            A <- network_storage_mult_rate - rate_constant_tank_storage * tank_storage
            B <- network_storage_mult_rate - tank_volume[t - 1] * rate_constant_tank_storage
            v[t] <- fifelse(A != 0 & B > 0, max(0, min(1, log(B/A)/rate_constant_tank_storage)), 0)
        }
        return(v)
    }

    data[, tau2 := fun_tau2(tank_volume, rate_constant_tank_storage, tank_storage, network_storage_mult_rate)]


    ### Ea(t) ### Equation 12 in appendix B
    data[, ea := (1 - tau2) * (network_storage_mult_rate - (rate_constant_tank_storage * tank_storage))]

    ### t2_1 ###
    fun_t2_1 <- function(tank_volume,
                         virtual_volume_d,
                         network_storage_mult_rate,
                         rate_constant_tank_storage,
                         tank_storage) {
        v <- numeric(length(tank_volume))
        v[1] <- NA_real_
        for (t in 2:length(v)) {
            v[t] <- max(0, min(1,
                               fcase(tank_volume[t - 1] == tank_storage & virtual_volume_d[t] >= tank_storage, 0,
                                     tank_volume[t - 1] == tank_storage,                                       (tank_volume[t - 1] - tank_storage) / (tank_volume[t - 1] - virtual_volume_d[t]),
                                     tank_volume[t - 1] == virtual_volume_d[t],                                1,
                                     virtual_volume_d[t] >= tank_storage,                                      (tank_volume[t - 1] - tank_storage) / (tank_volume[t - 1] - virtual_volume_d[t]),
                                     default = 1)
                         ))
        }
        return(v)
    }

    data[, t2_1 := fun_t2_1(tank_volume, virtual_volume_d, network_storage_mult_rate, rate_constant_tank_storage, tank_storage)]

    ### ed ###
    fun_ed <- function(tank_volume,
                       runoff,
                       network_flow,
                       t2_1,
                       network_storage_mult_rate,
                       rate_constant_tank_storage,
                       tank_storage,
                       qdwf,
                       rate_constant_network_storage) {
        v <- numeric(length(tank_volume))
        v[1] <- NA_real_
        for (t in 2:length(v)){
            f1 <- network_storage_mult_rate
            f2 <- rate_constant_tank_storage * tank_storage
            v[t] <- ifelse(tank_volume[t - 1] < tank_storage,
                           (runoff[t] + qdwf - rate_constant_tank_storage * tank_storage) * (1 - t2_1[t]) +
                               (runoff[t] + qdwf - network_flow[t - 1]) / rate_constant_network_storage * (exp(-rate_constant_network_storage) - exp(-rate_constant_network_storage * t2_1[t])),
                           (runoff[t] + qdwf - rate_constant_tank_storage * tank_storage) * (t2_1[t]) +
                               (t2_1[t] + qdwf - network_flow[t - 1]) / rate_constant_network_storage * (-1 + exp(-rate_constant_network_storage * t2_1[t]))
            )
        }
        return(v)
    }

    data[, ed := fun_ed(tank_volume, runoff, network_flow, t2_1, network_storage_mult_rate, rate_constant_tank_storage, tank_storage, qdwf, rate_constant_network_storage)]

    ### t2_2 ###
    fun_t2_2 <- function(tank_volume,
                         virtual_volume_b,
                         tau1,
                         tank_storage) {
        v <- numeric(length(tank_volume))
        v[1] <- NA_real_
        for (t in 2:length(v)) {
                v[t] <- pmin(1, (tank_volume[t - 1] - tank_storage) / (tank_volume[t - 1] - virtual_volume_b[t])) * tau1[t]
            }
        return(v)
    }

    data[, t2_2 := fun_t2_2(tank_volume, virtual_volume_b, tau1, tank_storage)]


    ### eb ###
    fun_eb <- function(runoff,
                       network_flow,
                       tau1,
                       t2_2,
                       virtual_volume_b_t1,
                       network_storage_mult_rate,
                       rate_constant_tank_storage,
                       tank_storage,
                       qdwf,
                       rate_constant_network_storage) {
        v <- numeric(length(runoff))
        v[1] <- NA_real_
        for (t in 2:length(v)) {
            f1 <- network_storage_mult_rate
            f2 <- rate_constant_tank_storage * tank_storage
            f3 <- (f1 - virtual_volume_b_t1[t] * rate_constant_tank_storage) / (f1 - f2)
            if (f3 >= 0) {
                v[t] <- (runoff[t] + qdwf - f2) * (tau1[t] - t2_2[t]) + (runoff[t] + qdwf - network_flow[t - 1]) /
                    rate_constant_network_storage * (exp(-rate_constant_network_storage * tau1[t]) - exp(-rate_constant_network_storage * t2_2[t])) +
                    (1 - pmin(1, tau1[t] + pmax(0, log(f3) / rate_constant_tank_storage))) * (f1 - f2)
            } else {
                # Negative Value for loG() is Undefined
                v[t] <- NA_real_
            }
        }
        return(v)
    }

    data[, eb := fun_eb(runoff, network_flow, tau1, t2_2, virtual_volume_b_t1, network_storage_mult_rate, rate_constant_tank_storage, tank_storage, qdwf, rate_constant_network_storage)]


    ### t2_3 ###
    data[, t2_3 := fcase(virtual_volume_c_t1 < tank_storage, tau1,
                         virtual_volume_c_t1 == tank_volume, 1,
                         rep(TRUE,.N), tau1 + (1 - tau1) * (virtual_volume_c_t1 - tank_storage) / (virtual_volume_c_t1 - tank_volume)
                         )]
    ### ec ###
    data[, ec := pmax(0, (runoff + qdwf - (rate_constant_tank_storage * tank_storage)) * (t2_3 - tau1) + (runoff + qdwf - network_flow) / rate_constant_network_storage *
                          (exp(-rate_constant_network_storage * t2_3) - exp(-rate_constant_network_storage * tau1)) +
                          (network_storage_mult_rate - (rate_constant_tank_storage * tank_storage)) * (tau1 - pmin(tau1, tau2)))]


    ### tank_overflow ###
    data[, tank_overflow := pmax(0, fcase(scenario == "a", ea,
                                          scenario == "b", eb,
                                          scenario == "c", ec,
                                          scenario == "d", ed))]


    ### Duration ###
    data[, duration := fifelse(pmax(network_overflow, tank_overflow) > 0, 1, 0)]

    ### Rain_event ###
    fun_rain_event <- function(precipitation) {
        v <- integer(length(precipitation))
        v[1] <- fifelse(precipitation[1] > 0, 1, 0)
        for (t in 2:length(v)) {
            v[t] <- fcase(precipitation[t - 1] == 0 & precipitation[t] > 0, 1,
                          precipitation[t - 1] > 0 & precipitation[t] > 0, 2,
                          default= 0)
        }
        return(v)
    }

    data[, rain_event := fun_rain_event(precipitation)]

    ### Frequency overflow ###
    fun_frequency_overflow <- function(duration) {
        v <- integer(length(duration))
        for (t in 1:(length(v)-1)) {
            v[t] <- fifelse(duration[t] == 1 & duration[t + 1] == 0, 1, 0)
        }
        return(v)
    }

    data[, frequency_overflow := fun_frequency_overflow(duration)]

    ### Total overflow ###
    data[, total_overflow := tank_overflow + network_overflow]

    ### Cumulate rain ###
    fun_cumulate_rain_per_event <- function(precipitation, rain_event) {
        v <- numeric(length(precipitation))
        v[1] <- 0
        for (t in 2:length(v)) {
            v[t] <- fifelse(rain_event[t] == 2, v[t - 1] + precipitation[t], 0)
            }
        return(v)
    }

    data[, cumulate_rain_per_event := fun_cumulate_rain_per_event(precipitation, rain_event)]

    ### cso_volume_per_event ###
    fun_cso_volume_per_event <- function(cumulate_rain_per_event,
                                         total_overflow,
                                         rain_event) {
        v <- numeric(length(cumulate_rain_per_event))
        v[1] <- 0
        for (t in 2:(length(v)-4)) {
            v[t] <- fcase(cumulate_rain_per_event[t] > 0 & cumulate_rain_per_event[t - 1] > 0, total_overflow[t] + v[t - 1],
                          cumulate_rain_per_event[t] > 0 & cumulate_rain_per_event[t - 1] == 0, total_overflow[t],
                          default = 0) +
                fifelse(total_overflow[t + 1] > 0 & cumulate_rain_per_event[t + 1] == 0 & rain_event[t] == 2 & rain_event[t + 1] == 0, total_overflow[t + 1], 0) +
                fifelse(total_overflow[t + 2] > 0 & cumulate_rain_per_event[t + 2] == 0 & rain_event[t] == 2 & rain_event[t + 1] == 0, total_overflow[t + 2], 0) +
                fifelse(total_overflow[t + 3] > 0 & cumulate_rain_per_event[t + 3] == 0 & rain_event[t] == 2 & rain_event[t + 1] == 0, total_overflow[t + 3], 0) +
                fifelse(total_overflow[t + 4] > 0 & cumulate_rain_per_event[t + 4] == 0 & rain_event[t] == 2 & rain_event[t + 1] == 0, total_overflow[t + 4], 0)
        }
        v[length(v)-3] <- v[t] <- fcase(cumulate_rain_per_event[t] > 0 & cumulate_rain_per_event[t - 1] > 0, total_overflow[t] + v[t - 1],
                                        cumulate_rain_per_event[t] > 0 & cumulate_rain_per_event[t - 1] == 0, total_overflow[t],
                                        default = 0) +
            fifelse(total_overflow[t + 1] > 0 & cumulate_rain_per_event[t + 1] == 0 & rain_event[t] == 2 & rain_event[t + 1] == 0, total_overflow[t + 1], 0) +
            fifelse(total_overflow[t + 2] > 0 & cumulate_rain_per_event[t + 2] == 0 & rain_event[t] == 2 & rain_event[t + 1] == 0, total_overflow[t + 2], 0) +
            fifelse(total_overflow[t + 3] > 0 & cumulate_rain_per_event[t + 3] == 0 & rain_event[t] == 2 & rain_event[t + 1] == 0, total_overflow[t + 3], 0)

        v[length(v)-2] <- v[t] <- fcase(cumulate_rain_per_event[t] > 0 & cumulate_rain_per_event[t - 1] > 0, total_overflow[t] + v[t - 1],
                                        cumulate_rain_per_event[t] > 0 & cumulate_rain_per_event[t - 1] == 0, total_overflow[t],
                                        default = 0) +
            fifelse(total_overflow[t + 1] > 0 & cumulate_rain_per_event[t + 1] == 0 & rain_event[t] == 2 & rain_event[t + 1] == 0, total_overflow[t + 1], 0) +
            fifelse(total_overflow[t + 2] > 0 & cumulate_rain_per_event[t + 2] == 0 & rain_event[t] == 2 & rain_event[t + 1] == 0, total_overflow[t + 2], 0)

        v[length(v)-1] <- v[t] <- fcase(cumulate_rain_per_event[t] > 0 & cumulate_rain_per_event[t - 1] > 0, total_overflow[t] + v[t - 1],
                                        cumulate_rain_per_event[t] > 0 & cumulate_rain_per_event[t - 1] == 0, total_overflow[t],
                                        default = 0) +
            fifelse(total_overflow[t + 1] > 0 & cumulate_rain_per_event[t + 1] == 0 & rain_event[t] == 2 & rain_event[t + 1] == 0, total_overflow[t + 1], 0)

        v[length(v)] <- v[t] <- fcase(cumulate_rain_per_event[t] > 0 & cumulate_rain_per_event[t - 1] > 0, total_overflow[t] + v[t - 1],
                                        cumulate_rain_per_event[t] > 0 & cumulate_rain_per_event[t - 1] == 0, total_overflow[t],
                                        default = 0)
        return(v)
    }

    data[, cso_volume_per_event := fun_cso_volume_per_event(cumulate_rain_per_event, total_overflow, rain_event)]

    ### Rain volumen per event ###
    fun_total_rain_volume_per_event <- function(rain_event,
                                                cumulate_rain_per_event) {
        v <- numeric(length(rain_event))
        for (t in 1:(length(v) - 1)) {
            v[t] <- fifelse(rain_event[t] == 2 & rain_event[t + 1] == 0, cumulate_rain_per_event[t], 0)
        }
        v[length(v)-1] <- fifelse(rain_event[t] == 2, cumulate_rain_per_event[t], 0)
        return(v)
    }

    data[, total_rain_volume_per_event := fun_total_rain_volume_per_event(rain_event, cumulate_rain_per_event)]

    ### Cso total volume per event ###
    fun_cso_total_volume_per_event <- function(rain_event, cso_volume_per_event) {
        v <- numeric(length(rain_event))
        for (t in 1:(length(v) - 1)) {
            v[t] <- fifelse(rain_event[t] == 2 & rain_event[t + 1] == 0, cso_volume_per_event[t], 0)
            }
        v[length(v)] <- fifelse(rain_event[t] == 2, cso_volume_per_event[t], 0)
        return(v)
    }

    data[, cso_total_volume_per_event := fun_cso_total_volume_per_event(rain_event, cso_volume_per_event)]

    ### ww_network_rejection ###

    # cat("Waste water network rejection...\n")
    #
    # fun_ww_network_rejection <- function(fua, network_rejection, precipitation, surface_storage, qdwf) {
    #   v <- numeric(length(network_rejection))
    #
    #   f <- "-"
    #   for (t in 1:length(v)) {
    #     if (fua[t] != f) {
    #       f <- fua[t]
    #       v[t] <- 0
    #     } else {
    #        v[t] <- network_rejection[t] / (1 + (precipitation[t] - max(0, surface_storage[t] - surface_storage[t - 1])) / qdwf[t])
    #     }
    #   }
    #   v
    # }
    # data$ww_network_rejection <- fun_ww_network_rejection(data$fua, data$network_rejection, data$precipitation, data$surface_storage, data$qdwf)
    #
    #
    #
    # ### ww_worst_case_overflow##
    #
    # cat("Waste water worst case overflow...\n")
    #
    # fun_ww_worst_case_overflow <- function(fua, worst_case_overflow, precipitation, surface_storage) {
    #   v <- numeric(length(worst_case_overflow))
    #
    #   f <- "-"
    #   for (t in 1:length(v)) {
    #     if (fua[t] != f) {
    #       f <- fua[t]
    #       v[t] <- 0
    #     } else {
    #        v[t] <- worst_case_overflow[t] / (1 + (precipitation[t] - max(0, surface_storage[t] - surface_storage[t - 1])) / param_qdwf
    #     )
    #     }
    #   }
    #   v
    # }
    #
    # data$ww_worst_case_overflow <- fun_ww_worst_case_overflow(data$fua, data$worst_case_overflow, data$precipitation, data$surface_storage)
    #
    #
    # ### ww_attenuated_worst_case_overflow ###
    #
    # cat("Waste water attenuated worst case overflow...\n")
    #
    # data$ww_attenuated_worst_case_overflow <- data$attenuated_worst_case_overflow / (1 + data$drainage / data$qdwf)
    #
    # ### ww_tank_overflow ###
    #
    # cat("Waste water tank overflow...\n")
    #
    # data$ww_tank_overflow <- data$tank_overflow / (1 + data$drainage / data$qdwf)
    return(data)
}
