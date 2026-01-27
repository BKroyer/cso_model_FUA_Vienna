##########################################################################################################
###                                                                                                    ###
### Hydrological Modeling of Combined Sewer Overflows (CSOs)                                           ###
### An R-Based Implementation of Quaranta et al. 2022                                                  ###
### by Nina Kleemeyer, B.Sc. reviewed and partly updated/rewritten by Steffen Kittlaus                 ###
###                                                                                                    ###
##########################################################################################################

cso_model <- function(
    population = NA,                    # numeric. Population of the catchment area. Must be > 0. If not given, pop_density must be given.
    area = NA,                          # numeric impervious catchment area in km². Must be > 0. If not given, pop_density must be given.
    pop_density = NA,                   # population density in persons per km² impervious area served by CS. If missing is calculated from impervious area, population and share_served_by_CS.
    share_served_by_CS = NA,            # share of population served by CS. According to Quaranta et al. (2022) mostly as the national average. If not given, pop_density (by impervious area!) must be given.
    time,                               # vector containing the timestamps as posixct datatype.
    precipitation,                      # numeric. vector containing precipitation per timestep [mm].
    max_item = NA,                      # option to subset data, integer of maximum item index to be used, e.g. 100 for first 100 time steps
    dwf_per_capita = 0.2,               # Dry Weather Flow (DWF) per capita [m³/day/person]. Default: 0.2 m³/day/person (Quaranta et al. 2022).
    qdwf = NA,                          # The DWF in mm/timestep for entire area and population
    W0 = 1.5,                           # W0 Maximum surface storage capacity of the catchment (water retained on impervious surfaces before runoff begins) [mm]. Default: 1.5 mm (Quaranta et al. 2022)
    k0 = 0.3,                           # k0 Reservoir constant for surface storage [1/timestep].Default: 0.3/(3 hours) (represents depletion of surface storage during dry periods).
    dn = 4,                             # Dilution rate of the sewer network [-]. Defines the maximum network capacity relative to DWF. Default: 4.
    dt = 7,                             # Dilution rate of the tank [-]. Defines the maximum tank outflow capacity relative to DWF. Default: 7.
    W1 = 5,                             # Network storage capacity [mm]. Default: 5 mm (storage capacity of the sewer network before overflow).
    W2 = 2,                             # Tank storage capacity [mm]. Default: 2 mm (capacity of the retention tank before overflow).
    print_params = FALSE
    ){

    ## Check input data type
    if (!is.na(population)) assert_count(population)
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
    assert_number(W2, na.ok = FALSE, lower = 0.05, upper = 7,  null.ok = FALSE) # max was 5

    ### basic parameter calculation
    if (length(unique(diff(time))) != 1){
        errorCondition(cat("Irregular time steps: ", unique(diff(time))))
    }
    time_step <- min(diff(time))
    timesteps_per_day <- 24/as.integer(time_step)
    if ((is.na(area) | is.na(population) | is.na(share_served_by_CS)) & is.na(pop_density)){
        errorCondition("Either population density [cap/km²_imp] or all of population [cap], area [km²] and share_served_by_CS [-] must be given.")
    }
    if(is.na(pop_density)){
        pop_density <- population / (area * share_served_by_CS)} # Personen/km²_imp (as km² imp served by CS) # in Excel often ha used as unit, factor 1/100
    if (is.na(qdwf)){
        if (is.na(pop_density) | is.na(dwf_per_capita)){
            errorCondition("Either qdwf or all of pop_density and dwf_per_capita must be given (or possible to calculate).")
        }
        qdwf <- pop_density * dwf_per_capita / timesteps_per_day / 1000 # mm/timestep; factor 1000 as area supposedly given in km², not ha
    }

    k1 <- dn * qdwf / W1 # Netzwerkspeicher Rate ((timestep)-1) (k1)
    k2 <- dt * qdwf / W2 # Tank Rate ((timestep)-1) (k2)

    network_max_conveyance <- qdwf * dn # Maximum conveyance of the network (mm/timestep/m²) according to Pistocci and Dorati 2018
    k1W1 <- W1 * k1 # Maximum conveyance of the network k1*W1

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




    # Create model dat object
    require(data.table)
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


    ### Surface Storage S(t) ### Equation 1
    v <- numeric(length(precipitation))
    v[1] <- pmin(precipitation[1], W0)
    for (t in 2:length(v)) {
        v[t] <- min(
            W0,
            if (precipitation[t] > 0) precipitation[t] + v[t - 1] else v[t - 1] * exp(-k0)
        )
    }
    data[, surface_storage := v]

    ### Runoff (Rainfall that reaches the network) R(t) ### Equation 2
    data[, runoff := pmax(0, precipitation - (W0 - data.table::shift(surface_storage)))]
    data[1, runoff := pmax(0, data$precipitation[2] - W0 + data$surface_storage[2] - data$surface_storage[1])] # S0-1 not known. Instead Runoff estimated via what must have been excess at t=1 to get from S0 to S1 (rain first logic: P (Rain), St, R)


    ### Ft Network Flow ### Equation (4)
    v <- numeric(nrow(data))
    v[1] <- data$runoff[1] + qdwf  # initialize first element as runoff plus qdwf

    for (t in 2:nrow(data)) {
        v[t] <- (data$runoff[t] + qdwf) * (1 - exp(-k1)) +
            v[t - 1] * exp(-k1)

    # Barcelona / Santiago (but author confirmed eq 4 is the correct one)
    # for (t in 2:nrow(data)) {
    #     v[t] <- (data$runoff[t] + qdwf)


    }
    data[, network_flow := v]


    ### Worst case overflow E(t) ### Equation 3 (overflow volume if no buffering capacity of the sewer network)
    data[ , worst_case_overflow := pmax(runoff + qdwf - k1W1, 0)]
    # Formula for Vienna more complicated. Ask Nina. ?? does not look any different in the Excel file


    # adding A and B terms to check results
    data[, A := runoff + qdwf - k1 * W1]
    data[, B := runoff + qdwf - data.table::shift(network_flow, 1L)] # default is type 'lag', so 1L means one previous timestep


    ### Scenario in one step ### Implementation checked by Steffen, just changed from function to direct data.table assignment
    data[, scenario :=
             fcase(  ( B >= 0 ) & ( A >= B ) | ( B <= 0 ) & ( A >= 0 ), "a", # B<=0 here in appendix, means network overflow 0, but important distinction for tank overflow
                     ( B >= 0 ) & ( A <= B )  &  ( A >= B * exp(-k1)), "b", # due to mail: A>=Bexp, equal returns exactly 0, but scenario matters for the tank overflow calculation
                     ( B <= 0 ) & ( A >= B ) & ( A <= B * exp(-k1)), "c", # B <= 0 in appendix, again exactly 0 should still not be d for tank overflow calculations
                     default = "d"
    )]


    ### tau1 ###
    # used in tank overflow calculations
    data[, tau1 := fcase(
        scenario %in% c("a", "d"), 0,
        scenario %in% c("b", "c"), log(B/A) / k1
    )]


    ### Network overflow E'(t) ### Equation 5
    fun_network_overflow <- function(A,
                                     B,
                                     scenario,
                                     k1) {

        ratio <- B / A
        # safe_ratio <- ifelse(ratio > 0, ratio, NA_real_) # to avoid warnings for log(B/A) when negative

        ratio_A_B <- A / B
        safe_ratio_A_B <- ifelse(ratio_A_B > 0, ratio_A_B, NA_real_)


        Eprime_b <- if (!ln_A_B) {
            A * (1 - (1 + log(ratio)) / k1) + B / k1 * exp(-k1)
        } else {
            A * (1 - (1 + log(safe_ratio_A_B)) / k1) + B / k1 * exp(-k1)
        }


        Eprime <- fcase(

            # scenario a:
            scenario == "a",
            A - B / k1 * (1 - exp(-k1)),

            # scenario b:
            ## ln(A/B) in the Excel files, I think B/A is correct (and seems like it, see email), but set to A/B to compare to Excel
            scenario == "b",
            Eprime_b,

            # scenario c:
            scenario == "c",
            A / k1 * (1 + log(ratio)) -
                B / k1,

            # scenario d (no overflow)
            default = 0
        )

        return(Eprime)
    }

    data[, network_overflow := pmax(0, fun_network_overflow(A, B, scenario, k1))]







    ### virtual volume and tank volume ###
    fun_virtual_volume_and_tank_volume <- function(
        runoff,
        network_flow,
        tau1,
        k2,                   # k2
        qdwf,
        k1,                   # k1
        W1,                   # W1
        k1W1,                 # k1W1
        W2,                   # max volume the tank can be filled with
        scenario
    ) {

        n <- length(runoff)

        va      <- numeric(n)
        vd      <- numeric(n)
        vb_t1   <- numeric(n)
        vc_t1   <- numeric(n)
        vb      <- numeric(n)
        vc      <- numeric(n)
        tv      <- numeric(n)

        tv[1] <- network_flow[1] / k2 # initial tank volume

        for (t in 2:n) {

            # --- scenario a
            va[t] <- k1W1 / k2 * (1 - exp(-k2)) + tv[t - 1] * exp(-k2)

            # --- scenario d
            vd[t] <- ((runoff[t] + qdwf) / k2) * (1 - exp(-k2)) -
                ((runoff[t] + qdwf) - network_flow[t - 1]) / (k2-k1) * (exp(-k1) - exp(-k2)) +
                tv[t - 1] * exp(-k2) # using the "real" tank volume as tv[t-1], in line with Excel

            # --- scenario b: tau = tau1
            vb_t1[t] <- ((runoff[t] + qdwf) / k2) * (1 - exp(-k2 * tau1[t])) -
                (((runoff[t] + qdwf) - network_flow[t - 1]) / (k2-k1)) * (exp(-k1 * tau1[t]) - exp(-k2 * tau1[t])) +
                tv[t - 1] * exp(-k2 * tau1[t])

            # --- scenario c: tau = tau1
            #vc_t1[t] <- k1W1 / k2 * (1 - exp(-k2 * tau1[t])) + tv[t - 1] * exp(-k2 * tau1[t]) # here they used 1 instead of tau1 in Excel, Nina also did, as if tau1 is 1 always
            #I think using tau1 is correct, but leave as 1 for now to compare to the Excel
            vc_t1[t] <- k1W1 / k2 * (1 - exp(-k2 * 1)) + tv[t - 1] * exp(-k2 * 1)
            # vc_t1[t] <- k1W1 / k2 * (1 - exp(-k2 * tau1)) + tv[t - 1] * exp(-k2 * tau1)

            # --- scenario b: part tau > tau1
            vb[t] <- vb_t1[t] * exp(-k2 * (1- tau1[t])) +
                k1W1 / k2 * (1 - exp(-k2 * (1- tau1[t])))

            # --- scenario c: part tau > tau1
            vc[t] <- ((runoff[t] + qdwf) / k2) * (1 - exp(-k2 * (1- tau1[t]))) +
                ((runoff[t] + qdwf) - network_flow[t - 1]) / (k2-k1) * (exp(-k2 * (1- tau1[t])) - exp(-k1 * (1 - tau1[t]))) +
                vc_t1[t] * exp(-k2 * (1- tau1[t]))

            # --- tank volume W (capped by tank capacity W2); always the state at end of time step
            tv[t] <- min(
                fifelse(scenario[t] == "a", va[t],
                        fifelse(scenario[t] == "b", vb[t],
                                fifelse(scenario[t] == "c", vc[t], vd[t]))),
                W2
            )
        }

        return(list(va, vd, vb_t1, vc_t1, vb, vc, tv))
    }

    data[, c("virtual_volume_a",
             "virtual_volume_d",
             "virtual_volume_b_t1",
             "virtual_volume_c_t1",
             "virtual_volume_b",
             "virtual_volume_c",
             "tank_volume_W") := fun_virtual_volume_and_tank_volume(runoff, network_flow, tau1, k2, qdwf, k1,
                                                                  W1, k1W1, W2, scenario)]


    ### tau2 ### equation 11 in appendix B
    data[, part1 := k1 * W1 - k2 * data.table::shift(tank_volume_W)]
    data[, part2 := k1 * W1 - k2 * W2]
    data[, tau2 :=  fifelse((part2 != 0 & (part1/part2) > 0), pmax(0, pmin(1, log(part1/part2) / k2)), 0)]
    # The if conditions should never happen, it makes no sense to build a tank with greater conveyance than the network
    # as that would mean no storage --> what it is set to (here 0) does not matter

    if (any(data$part2 <= 0 | data$part1 < 0, na.rm = TRUE)) {
        stop("Tank conveyance numerically greater than or equal to network conveyance, does not make sense in construction.")
    }


    data[, c("part1", "part2") := NULL] # not needed further


    ### Ea(t) ### Equation 12 in appendix B
    data[, Ea := (1 - tau2) * (k1W1 - (k2 * W2))]

    ### t2_1 ###
    fun_t2_1 <- function(tank_volume,
                         virtual_volume_d,
                         k1W1,
                         k2,
                         W2) {
        v <- numeric(length(tank_volume))
        v[1] <- NA_real_
        for (t in 2:length(v)) {
            v[t] <- max(0, min(1,
                               fcase(tank_volume[t - 1] == W2 & virtual_volume_d[t] >= W2, 0, # tank full from beginning and stays full -> overflow from time 0
                                     tank_volume[t - 1] == W2,                                       (tank_volume[t - 1] - W2) / (tank_volume[t - 1] - virtual_volume_d[t]), # the ratio
                                     tank_volume[t - 1] == virtual_volume_d[t],                                1, # if tau can't be determined (and Wt-1 < W2), tau2 is 1 (case where it is 0 covered by first expression)
                                     virtual_volume_d[t] >= W2,                                      (tank_volume[t - 1] - W2) / (tank_volume[t - 1] - virtual_volume_d[t]), # the ratio
                                     default = 1)
                         ))
        }
        return(v)
    }

    data[, t2_1 := fun_t2_1(tank_volume_W, virtual_volume_d, k1W1, k2, W2)]


    ### Ed(t) ### Equation 20 (?) in appendix B

    data[, Ed := fifelse(
        data.table::shift(data$tank_volume) < W2,
        # case: tank not full at previous timestep
        (runoff + qdwf - k2 * W2) * (1 - t2_1) +
            (runoff + qdwf - data.table::shift(data$network_flow)) / k1 *
            (exp(-k1) - exp(-k1 * t2_1)),

        # case: tank full at previous timestep
        (runoff + qdwf - k2 * W2) * t2_1 +
            (t2_1 + qdwf - data.table::shift(data$network_flow)) / k1 *
            (exp(-k1 * t2_1) - 1)
    )]



    ### t2_2 ###
    data[ ,t2_2 := pmin(1, (data.table::shift(tank_volume_W) - W2) / (data.table::shift(tank_volume_W) - virtual_volume_b)) * tau1]


    ### Eb ###
    data[, Eb :=
             fifelse(
                 ((W1 * k1 - k2 * W2) != 0 & ((W1 * k1 - virtual_volume_b_t1 * k2)/(W1 * k1 - k2 * W2)) > 0),
                 (runoff + qdwf - k2 * W2) * (tau1 - t2_2) + (runoff + qdwf - data.table::shift(network_flow)) / k1 *
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
    data[, rain_event := fifelse(
        data.table::shift(precipitation) == 0 & precipitation > 0, 1L,
        fifelse(
            data.table::shift(precipitation) > 0 & precipitation > 0, 2L,
            0L
        )
    )]

    ### Frequency overflow ### # not used further, not found in Excel, marks overflow events, so can be used later to calculate how many events there were
    data[, frequency_overflow := fifelse(duration == 1 & data.table::shift(duration, type = "lead") == 0, 1, 0)] # no , fill = 0 means NA in first row

    ### Total overflow ###
    data[, total_overflow := tank_overflow + network_overflow]

    ### CSO and Rain volumes per rain event
    # already gives total at event end, was cumulative and then total before; this is per RAIN event as in Barcelona
    # marking end of rain event to simplify the cso volume per event calculation (where rain_event==2 or 1 transitions to 0)
    data[, rain_end := fifelse(rain_event != 0 & data.table::shift(rain_event, type = "lead", fill = 0) == 0, 1L, 0L)]

    data[, cso_volume_per_event := 0]  # initialize
    data[, rain_volume_per_event := 0]
    # for each event, sum total_overflow over the event and write it at the end
    event_starts <- which(data$rain_event == 1)
    for (start in event_starts) {

        # find next rain_end
        rel_end <- which(data$rain_end[start:nrow(data)] == 1)

        if (length(rel_end) > 0) {
            # normal case: event ends
            end <- start + rel_end[1] - 1
        } else {
            # no end → store result at last row of the dataset
            end <- nrow(data)
        }

        # write totals at the event end
        data$cso_volume_per_event[end]  <- sum(data$total_overflow[start:end], na.rm = TRUE)
        data$rain_volume_per_event[end] <- sum(data$precipitation[start:end], na.rm = TRUE)
    }









    # not checked yet, not needed for model validation with Excel
    ### ww_network_rejection ###

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

    if (sum(is.na(data$Eb)) == 1 & sum(is.na(data$tau1)) <= 1 & sum(is.na(data$tau2)) == 1){
        print("NA warnings ok for Eb, tau1, tau2: Only first row affected") # first row is always NA in Eb & tau2, might not be in tau1 if scenario a or d
    }else{
        print("NA warnings NOT ok for Eb, tau1 and/or tau2")
    }

    if (sum(is.na(data$network_overflow)) == 0){
        print("NA warnings ok for network_overflow: No rows affected, all scenarios get evaluated") # first row is always NA in Eb & tau2, might not be in tau1 if scenario a or d
    }else{
        print("NA warnings NOT ok for network_overflow")
    }


    if (data[, any(sapply(.SD, is.infinite))]) {
        stop("Error: data contains Inf or -Inf values. Most probably due to a log expression being 0.")
    }


    return(data)
}
