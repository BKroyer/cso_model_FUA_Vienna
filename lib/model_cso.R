##########################################################################################################
###                                                                                                    ###
### Hydrological Modeling of Combined Sewer Overflows (CSOs)                                           ###
### An R-Based Implementation of Quaranta et al. 2022                                                  ###
### by Nina Kleemeyer, B.Sc. reviewed and partly updated/rewritten by Steffen Kittlaus                 ###
### restructured including Rcpp, checked and simplified by Bettina Kroyer                              ###
###                                                                                                    ###
##########################################################################################################

cso_model <- function(
    population = NA,                    # numeric. Population of the catchment area discharging into the combined sewer (connected people). Must be > 0. If not given, pop_density must be given.
    design_population = NA,             # numeric. Population the WWTP was designed for. "Bemessungswert"
    area = NA,                          # numeric impervious catchment area in km². Must be > 0. If not given, pop_density must be given.
    pop_density = NA,                   # population density in persons per km² impervious area served by CS. If missing is calculated from impervious area, population and share_served_by_CS.
    share_served_by_CS = NA,            # share of population served by CS. According to Quaranta et al. (2022) mostly as the national average. If not given, pop_density (by impervious area!) must be given.
    time,                               # vector containing the timestamps as posixct datatype.
    precipitation,                      # numeric. vector containing precipitation per timestep [mm].
    max_item = NA,                      # option to subset data, integer of maximum item index to be used, e.g. 100 for first 100 time steps
    dwf_per_capita = 0.2,               # Dry Weather Flow (DWF) per capita [m³/day/person]. Default: 0.2 m³/day/person (Quaranta et al. 2022).
    qdwf = NA,                          # The DWF in mm/m²/timestep for entire population
    W0 = 1.5,                           # W0 Maximum surface storage capacity of the catchment (water retained on impervious surfaces before runoff begins) [mm]. Default: 1.5 mm (Quaranta et al. 2022)
    k0 = 0.3,                           # k0 Reservoir constant for surface storage [1/timestep].Default: 0.3/(3 hours) (represents depletion of surface storage during dry periods).
    dn = 7,                             # Dilution rate of the sewer network [-]. Defines the maximum network capacity relative to DWF. Default: 7.
    dt = 4,                             # Dilution rate of the tank [-]. Defines the maximum tank outflow capacity relative to DWF. Default: 4.
    W1 = 5,                             # Network storage capacity [mm]. Default: 5 mm (storage capacity of the sewer network before overflow).
    W2 = 2,                             # Tank storage capacity [mm]. Default: 2 mm (capacity of the retention tank before overflow).
    print_params = FALSE,
    ln_A_B = FALSE
    ){

    ## Check input data type
    if (!is.na(population)) assert_numeric(population)#assert_count(population)
    if (!is.na(design_population)) assert_numeric(design_population)#assert_count(population)
    assert_number(area, na.ok = TRUE, lower = 0, finite = TRUE)
    if (!is.na(area) & !(area > 0)) stop(sprintf("area must be > 0, but is %s", area))
    assert_posixct(time, any.missing = FALSE, min.len = 8, unique = TRUE, null.ok = FALSE, sorted = TRUE)
    assert_numeric(precipitation, lower = 0, finite = TRUE, any.missing = FALSE, all.missing = FALSE, min.len = 8)
    if (!is.na(pop_density)) assert_numeric(pop_density, lower = 0, finite = TRUE, any.missing = FALSE, all.missing = TRUE)
    assert_number(dwf_per_capita, na.ok = FALSE, lower = 0.05, upper = 2,  null.ok = FALSE)
    assert_number(W0, na.ok = FALSE, lower = 0.2, upper = 5,  null.ok = FALSE)
    assert_number(k0, na.ok = FALSE, lower = 0.05, upper = 2,  null.ok = FALSE)
    assert_number(dn, na.ok = FALSE, lower = 3, upper = 40,  null.ok = FALSE) # max was 20; stuttgart is 40
    assert_number(dt, na.ok = FALSE, lower = 1.5, upper = 24,  null.ok = FALSE) # max was 20; min was 3 Santiago says 1.5
    assert_number(W1, na.ok = FALSE, lower = 0.05, upper = 13,  null.ok = FALSE) # max was 5, Ecully says 13
    assert_number(W2, na.ok = FALSE, lower = 0.05, upper = 10,  null.ok = FALSE) # max was 5

    ### basic parameter calculation

    if (length(unique(diff(time))) != 1) {

        time_step <- min(diff(time))
        time_full <- seq(min(time), max(time), by = time_step)

        dt_temp <- data.table(time = time, prec = precipitation)
        dt_full <- dt_temp[data.table(time = time_full), on = "time"]

        # fill missing precipitation with previous value
        dt_full[, prec := nafill(prec, type = "locf")]

        time <- dt_full$time
        precipitation <- dt_full$prec
    }

    time_step <- min(diff(time))
    if (time_step < 0){
        errorCondition("Check time step! Is below 0.")
    }
    timesteps_per_day <- 24/as.integer(time_step)
    if ((is.na(area) | is.na(population) | is.na(share_served_by_CS)) & is.na(pop_density)){
        errorCondition("Either population density [cap/km²_imp] or all of population [cap], area [km²] and share_served_by_CS [-] must be given.")
    }
    if (!is.na(share_served_by_CS) & share_served_by_CS==0){
        share_served_by_CS <- 0.0000000001
    }
    if(is.na(pop_density)){
        pop_density <- population / (area * share_served_by_CS)} # Personen/km²_imp (as km² imp served by CS) # in Excel often ha used as unit, factor 1/100
    if (is.na(qdwf)){
        if (is.na(pop_density) | is.na(dwf_per_capita)){
            errorCondition("Either qdwf or all of pop_density and dwf_per_capita must be given (or impossible to calculate).")
        }
        qdwf <- pop_density * dwf_per_capita / timesteps_per_day / 1000 # mm/timestep; factor 1000 as area supposedly given in km², not ha
        if (qdwf == 0){
            qdwf <- 0.0000000001
        }
    }

    if (!is.na(design_population)){
        if (design_population == 0){
            design_population <- 0.0000000001
        }
        if (population == 0){
            population <- 0.0000000001
        }

        util_rate <- population / (design_population * (125/200)) # factor considering industry and gw infiltration

        # less utilisation means higher dilution rate triggering overflow
        dn <- dn / util_rate
        dt <- dt / util_rate
    }

    k1 <- dn * qdwf / W1 # Netzwerkspeicher Rate ((timestep)-1) (k1)
    k2 <- dt * qdwf / W2 # Tank Rate ((timestep)-1) (k2)

    network_max_conveyance <- qdwf * dn # Maximum conveyance of the network (mm/timestep/m²) according to Pistocci and Dorati 2018
    k1W1 <- W1 * k1 # Maximum conveyance of the network k1*W1
    k2W2 <- W2 * k2
    e_k1 <- exp(-k1)
    e_k2 <- exp(-k2)

    if (print_params){
        print(paste0("network max conveyance according to Pistoccio: ", network_max_conveyance, " and k1W1: ", k1W1))


        print(paste0("k0 is ", k0, " 1/timestep"))
        print(paste0("k1 is ", round(k1, 4), " 1/timestep"))
        print(paste0("k2 is ", round(k2, 4), " 1/timestep"))

        print(paste0("W0 is ", W0, " mm"))
        print(paste0("W1 is ", W1, " mm"))
        print(paste0("W2 is ", W2, " mm"))

        print(paste0("dt is ", dt, ""))
        print(paste0("dn is ", dn, ""))

        print(paste0("qdwf is ", qdwf))
        print(paste0("time step is ", time_step," hours"))
        print(paste0("the population density is ", round(pop_density, 2), " cap/km²_imp"))
        print(paste0("CS share is ", round(share_served_by_CS, 4)))
    }

    #print(paste0("qdwf is ", qdwf))


    # Create model dat object
    if (is.na(max_item)){
        data <- data.table(time = time, precipitation = precipitation, key = "time")
    }else{
        data <- data.table(time = time[1:max_item], precipitation = precipitation[1:max_item], key = "time")
    }

    if (print_params){
        print(paste0("The mean annual precipitation is ", round(mean(data$precipitation, na.rm=T) * 8 * 365, 2)," mm/year"))
    }


    # save area (the impervious one!) to data for validation purposes later
    data[, area := area * share_served_by_CS]


    data[, surface_storage := surface_storage_cpp(precipitation, W0, k0)]

    ### Runoff (Rainfall that reaches the network) R(t) ### Equation 2
    data[, runoff := pmax(0, precipitation - (W0 - data.table::shift(surface_storage)))]
    data[1, runoff := pmax(0, data$precipitation[2] - W0 + data$surface_storage[2] - data$surface_storage[1])] # S0-1 not known. Instead Runoff estimated via what must have been excess at t=1 to get from S0 to S1 (rain first logic: P (Rain), St, R)


    data[, network_flow := network_flow_cpp(data$runoff, qdwf, k1)]



    ### Worst case overflow E(t) ### Equation 3 (overflow volume if no buffering capacity of the sewer network)
    # and adding A and B terms to check results
    network_flow_lag <- data.table::shift(data$network_flow)

    data[, `:=`(
        A = runoff + qdwf - k1 * W1,
        worst_case_overflow = pmax(runoff + qdwf - k1W1, 0),
        B = runoff + qdwf - data.table::shift(network_flow)
    )]


    ### Scenario in one step ### Implementation checked by Steffen, just changed from function to direct data.table assignment
    data[, scenario :=
             fcase(  ( B >= 0 ) & ( A >= B ) | ( B <= 0 ) & ( A >= 0 ), "a", # B<=0 here in appendix, means network overflow 0, but important distinction for tank overflow
                     ( B >= 0 ) & ( A <= B )  &  ( A >= B * e_k1), "b", # due to mail: A>=Bexp, equal returns exactly 0, but scenario matters for the tank overflow calculation
                     ( B <= 0 ) & ( A >= B ) & ( A <= B * e_k1), "c", # B <= 0 in appendix, again exactly 0 should still not be d for tank overflow calculations
                     default = "d"
    )]


    ### tau1 ###
    # used in tank overflow calculations
    data[, tau1 := fifelse(
        scenario %in% c("a", "d"), 0,
        log(B / A) / k1
    )]



    data[, network_overflow := network_overflow_rcpp(
        A = A,
        B = B,
        scenario = scenario,
        k1 = k1,
        ln_A_B = ln_A_B,   # or TRUE if needed
        e_k1 = e_k1
    )]






    # ### virtual volume and tank volume ###
    # converting scenarios to integers for C++, apparently faster than characters
    data[, scenario_id :=
             fcase(
                 scenario == "a", 1L,
                 scenario == "b", 2L,
                 scenario == "c", 3L,
                 default = 4L # this is "d"
             )
    ]

    virtual_volume_cols <- virtual_volume_and_tank_volume_cpp(
        runoff        = data$runoff,
        network_flow  = data$network_flow,
        tau1          = data$tau1,
        k2            = k2,
        qdwf          = qdwf,
        k1            = k1,
        W1            = W1,
        k1W1          = k1W1,
        W2            = W2,
        scenario      = data$scenario_id
    )

    data[, names(virtual_volume_cols) := virtual_volume_cols] # virtual volumes and tank volume




    ### tau2 ### equation 11 in appendix B
    tank_volume_W_lag <- data.table::shift(data$tank_volume_W)


    data[, `:=`(
        tau2 = fifelse(
            (k2W2 != 0 & (k1W1 - k2 * tank_volume_W_lag)/k2W2 > 0),
            pmax(0, pmin(1, log((k1W1 - k2 * tank_volume_W_lag)/k2W2) / k2)),
            0
        ),
        Ea = (1 - fifelse(
            (k2W2 != 0 & (k1W1 - k2 * tank_volume_W_lag)/k2W2 > 0),
            pmax(0, pmin(1, log((k1W1 - k2 * tank_volume_W_lag)/k2W2) / k2)),
            0
        )) * (k1W1 - k2W2)
    )]



    data[, t2_1 := t2_1_cpp(tank_volume_W, virtual_volume_d, W2)]


    ### Ed(t) ### Equation 20 (?) in appendix B

    data[, Ed := fifelse(
        data.table::shift(data$tank_volume) < W2,
        # case: tank not full at previous timestep
        (runoff + qdwf - k2 * W2) * (1 - t2_1) +
            (runoff + qdwf - network_flow_lag) / k1 *
            (e_k1 - exp(-k1 * t2_1)),

        # case: tank full at previous timestep
        (runoff + qdwf - k2 * W2) * t2_1 +
            (t2_1 + qdwf - network_flow_lag) / k1 *
            (exp(-k1 * t2_1) - 1)
    )]



    ### t2_2 ###
    data[ ,t2_2 := pmin(1, (tank_volume_W_lag - W2) / (tank_volume_W_lag - virtual_volume_b)) * tau1]


    ### Eb ###
    data[, Eb :=
             fifelse(
                 ((W1 * k1 - k2 * W2) != 0 & ((W1 * k1 - virtual_volume_b_t1 * k2)/(W1 * k1 - k2 * W2)) > 0),
                 (runoff + qdwf - k2 * W2) * (tau1 - t2_2) + (runoff + qdwf - network_flow_lag) / k1 *
                     (exp(-k1 * tau1) - exp(-k1 * t2_2)) +
                     (W1 * k1 - k2 * W2) *
                     (1 - pmin(1, tau1 + pmax(0, log((W1 * k1 - virtual_volume_b_t1 * k2) / # tau1 capped at 1 here
                                                         (W1 * k1 - k2 * W2)) / k2))),
                 0 # same logic as with tau2; tank conveyance should be smaller than network conveyance and inserting 0 should not happen
             )]



    ### t2_3 ### # implementation fits the Excel version and makes sense with the appendix
    data[, t2_3 := fcase(virtual_volume_c_t1 < W2, tau1, # here the same as condition tau > tau1; if tau2''' == tau1, no overflow for that integral, Ec collapses to just the tau<=tau1 part
                         virtual_volume_c_t1 == tank_volume_W, 1, # if the virtual volume at tau1 the same as the tank volume at the end of the timestep, tau1 must have been 1 and tau2''' becomes 1 in its formula
                         rep(TRUE,.N), tau1 + (1 - tau1) * (virtual_volume_c_t1 - W2) / (virtual_volume_c_t1 - tank_volume_W) # this is basically the else part
                         )]
    ### Ec ###
    data[, Ec := pmax(0, (runoff + qdwf - (k2 * W2)) * (t2_3 - tau1) + ((runoff + qdwf - network_flow) / k1) *
                          (exp(-k1 * t2_3) - exp(-k1 * tau1)) +
                          ((W1 * k1) - (k2 * W2)) * (tau1 - pmin(tau1, tau2)))]




    ### tank_overflow ###
    data[, tank_overflow := pmax(0, fcase(scenario == "a", Ea,
                                          scenario == "b", Eb,
                                          scenario == "c", Ec,
                                          scenario == "d", Ed))]


    ### Duration ###
    data[, duration := fifelse(pmax(network_overflow, tank_overflow) > 0, 1, 0)] # if one (or both) of network and tank overflow, then this timestep is 1 and counts towards the duration

    ### Rain_event ### # == spill numbers in Excel; 1 = start; 2 = rain continues; 0 = no rain
    data[, rain_event := {
        prev <- data.table::shift(precipitation, type = "lag", n = 1L, fill = 1) # assuming precipitation the time stamp before

        fifelse(
            precipitation > 0 & prev == 0, 1,
            fifelse(
                precipitation > 0 & (prev > 0 | is.na(prev)), 2,
                0
            )
        )
    }]

    ### Frequency overflow ### # not used further, not found in Excel, marks overflow events, so can be used later to calculate how many events there were
    data[, frequency_overflow := fifelse(duration == 1 & data.table::shift(duration, type = "lead") == 0, 1, 0)] # no , fill = 0 means NA in first row

    ### Total overflow ###
    data[, total_overflow := tank_overflow + network_overflow]


    # mark rain event ends
    data[, rain_end := fifelse(rain_event != 0 & data.table::shift(rain_event, type = "lead", fill = 0) == 0, 1L, 0L)]

    # assign a unique event ID per rain event
    data[, event_id := rleid(rain_event)][rain_event == 0, event_id := NA_integer_]

    # compute cso and rain volume per event at event end
    event_sums <- data[!is.na(event_id), .(
        cso_volume_per_event = sum(total_overflow, na.rm = TRUE),
        rain_volume_per_event = sum(precipitation, na.rm = TRUE),
        row_end = .I[.N]  # last row of the event
    ), by = event_id]

    # initialize columns
    data[, cso_volume_per_event := 0]
    data[, rain_volume_per_event := 0]

    # assign sums at the end row of each event
    data[event_sums$row_end, c("cso_volume_per_event", "rain_volume_per_event") :=
             .(event_sums$cso_volume_per_event, event_sums$rain_volume_per_event)]

    data[, ':=' (total_overflow_m3 = total_overflow * area * 1000,
             precipitation_m3 = precipitation * area * 1000)]


    data[, DWF_content := qdwf/(qdwf + runoff)]
    #print(paste0("The mean dwf content is: ", mean(data$DWF_content, na.rm=T)))
    data[, ':=' (DWF_volume_mm = total_overflow * DWF_content,
                 DWF_volume_m3 = total_overflow_m3 * DWF_content)]
    #print(paste0("The mean dwf volume is: ", mean(data$DWF_volume_m3, na.rm=T)))


    # not checked yet, not needed for model validation with Excel
    # not used in thesis, focus on correct CSO volume and DWF volume within

    ### ww_network_rejection ### # should not have DWF content, only rain water

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
    # data$ww_attenuated_worst_case_overflow <- data$attenuated_worst_case_overflow / (1 + data$drainage / data$qdwf)
    #
    # ### ww_tank_overflow ###
    #
    # data$ww_tank_overflow <- data$tank_overflow / (1 + data$drainage / data$qdwf)



    if (!(sum(is.na(data$Eb)) <= 1 & sum(is.na(data$tau1)) <= 1 & sum(is.na(data$tau2)) <= 1)){
        print("NA warnings NOT ok for Eb, tau1 and/or tau2")

    }#else{
        #print("NA warnings ok for Eb, tau1, tau2: Only first row affected") # first row is always NA in Eb & tau2, might not be in tau1 if scenario a or d
    #}

    if (sum(is.na(data$network_overflow)) != 0){
        print("NA warnings NOT ok for network_overflow")

    }#else{
     #   print("NA warnings ok for network_overflow: No rows affected, all scenarios get evaluated") # first row is always NA in Eb & tau2, might not be in tau1 if scenario a or d

    #}

    if (data[, any(sapply(.SD, is.infinite))]) {
        stop("Error: data contains Inf or -Inf values. Most probably due to a log expression being 0.")
    }

    #saveRDS(data, file.path(path_intermediate_res, paste0("E_data_dn", dn)))

    return(data)
}
