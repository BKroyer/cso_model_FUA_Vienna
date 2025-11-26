##########################################################################################################
###                                                                                                    ###
### Hydrological Modeling of Combined Sewer Overflows (CSOs)                                           ###
### An R-Based Implementation of Quaranta et al. 2022                                                  ###
### by Nina Kleemeyer, B.Sc. reviewed and partly updated/rewritten by Steffen Kittlaus                 ###
###                                                                                                    ###
##########################################################################################################

cso_model <- function(
    population,                         # numeric. Population of the catchment area. Must be > 0.
    area,                               # numeric impervious catchment area in km². Must be > 0.
    pop_density = NA,                   # population density in persons per km² impervious area served by CS. If missing is calculated from impervious area, population and share_served_by_CS.
    share_served_by_CS,                 # share of population served by CS. According to Quaranta et al. (2022) mostly as the national average
    time,                               # vector containing the timestamps as posixct datatype.
    precipitation,                      # numeric. vector containing precipitation per timestep [mm].
    max_item = NA,                      # option to subset data, integer of maximum item index to be used, e.g. 100 for first 100 time steps
    dwf_per_capita = 0.2,               # gDry Weather Flow (DWF) per capita [m³/day/person]. Default: 0.2 m³/day/person (Quaranta et al. 2022).
    catchment_surface_storage = 1.5,    # W0 Maximum surface storage capacity of the catchment (water retained on impervious surfaces
                                        # before runoff begins) [mm]. Default: 1.5 mm (Quaranta et al. 2022)
    rate_constant_surface_storage = 0.3,# k0 Reservoir constant for surface storage [1/timestep].Default: 0.3/(3 hours) (represents depletion of surface storage during dry periods).
    network_dwf_dilution_rate = 9.1,    # Dilution rate of the sewer network [-]. Defines the maximum network capacity relative to DWF. Default: 9.1.

    # maybe delete again
    network_max_capacity = NA,          # Maximum capacity of the network in L/(s * ha) = ... if known, then network flow capped. If unknown, no cap. Note that excess water is not accounted for in the CSO volumes yet.

    tank_dwf_dilution_rate = 9,         # Dilution rate of the tank [-]. Defines the maximum tank outflow capacity relative to DWF. Default: 9.
    network_storage = 1,                # Network storage capacity [mm]. Default: 1 mm (storage capacity of the sewer network before overflow).
    tank_storage = 0.45                 # Tank storage capacity [mm]. Default: 0.45 mm (capacity of the retention tank before overflow).
    ){

    ## Check input data type
    require(checkmate)
    assert_count(population)
    assert_number(area, na.ok = FALSE, lower = 0, finite = TRUE)
    if (!(area > 0)) stop(sprintf("area must be > 0, but is %s", area))
    assert_posixct(time, any.missing = FALSE, min.len = 8, unique = TRUE, null.ok = FALSE, sorted = TRUE)
    assert_numeric(precipitation, lower = 0, finite = TRUE, any.missing = FALSE, all.missing = FALSE, min.len = 8)
    if (!is.na(pop_density)) assert_numeric(pop_density, lower = 0, finite = TRUE, any.missing = FALSE, all.missing = TRUE)
    assert_number(dwf_per_capita, na.ok = FALSE, lower = 0.05, upper = 2,  null.ok = FALSE)
    assert_number(catchment_surface_storage, na.ok = FALSE, lower = 0.2, upper = 5,  null.ok = FALSE)
    assert_number(rate_constant_surface_storage, na.ok = FALSE, lower = 0.05, upper = 2,  null.ok = FALSE)
    assert_number(network_dwf_dilution_rate, na.ok = FALSE, lower = 3, upper = 25,  null.ok = FALSE) # max was 20
    assert_number(tank_dwf_dilution_rate, na.ok = FALSE, lower = 3, upper = 24,  null.ok = FALSE) # max was 20
    assert_number(network_storage, na.ok = FALSE, lower = 0.05, upper = 5,  null.ok = FALSE)
    assert_number(tank_storage, na.ok = FALSE, lower = 0.05, upper = 7,  null.ok = FALSE) # max was 5

    ### basic parameter calculation
    time_step <- min(diff(time))
    timesteps_per_day <- 24/as.integer(time_step)
    if(is.na(pop_density)){
        pop_density <- population / (area * share_served_by_CS)} # Personen/km²_imp (as km² imp served by CS) # in Excel often ha used as unit, factor 1/100
    qdwf <- pop_density * dwf_per_capita / timesteps_per_day / 1000 # mm/timestep; factor 1000 as area supposedly given in km², not ha
    rate_constant_network_storage <- network_dwf_dilution_rate * qdwf / network_storage # Netzwerkspeicher Rate ((timestep)-1) (k1)
    k1 <- rate_constant_network_storage

    # tank_dwf_dilution_rate in Barcelona calculated via assumed maximum tank conveyance capacity of 11 m³/s

    rate_constant_tank_storage <- tank_dwf_dilution_rate * qdwf / tank_storage # Tank Rate ((timestep)-1) (k2)
    k2 <- rate_constant_tank_storage
    network_max_conveyance <- qdwf * network_dwf_dilution_rate # 12.75098313 # Maximum conveyance of the network (mm/timestep/m²) according to Pistocci and Dorati 2018
    network_storage_mult_rate <- network_storage * rate_constant_network_storage # Maximum conveyance of the network k1*W1

    # Create model dat object
    require(data.table)
    if (is.na(max_item)){
        data <- data.table(time = time, precipitation = precipitation, key = "time")
    }else{
        data <- data.table(time = time[1:max_item], precipitation = precipitation[1:max_item], key = "time")
    }

    ### Surface Storage S(t) ### Equation 1
    v <- numeric(length(precipitation))
    v[1] <- pmin(precipitation[1], catchment_surface_storage)
    for (t in 2:length(v)) {
        v[t] <- min(
            catchment_surface_storage,
            if (precipitation[t] > 0) precipitation[t] + v[t - 1] else v[t - 1] * exp(-rate_constant_surface_storage)
        )
    }
    data[, surface_storage := v]

    ### Runoff (Rainfall that reaches the network) R(t) ### Equation 2
    data[, runoff := pmax(0, precipitation - (catchment_surface_storage - shift(surface_storage)))]
    data[1, runoff := pmax(0, data$precipitation[2] - catchment_surface_storage + data$surface_storage[2] - data$surface_storage[1])] # S0-1 not known. Instead Runoff estimated via what must have been excess at t=1 to get from S0 to S1 (rain first logic: P (Rain), St, R)


    ### Ft Network Flow ### Equation (4)
    v <- numeric(nrow(data))
    v[1] <- data$runoff[1] + qdwf  # initialize first element as runoff plus qdwf

    for (t in 2:nrow(data)) {
        v[t] <- (data$runoff[t] + qdwf) * (1 - exp(-rate_constant_network_storage)) +
            v[t - 1] * exp(-rate_constant_network_storage)
    }
    data[, network_flow := v]


    ### Worst case overflow E(t) ### Equation 3 (overflow volume if no buffering capacity of the sewer network)
    data[ , worst_case_overflow := pmax(runoff + qdwf - network_storage_mult_rate, 0)]
    # Formula for Vienna more complicated. Ask Nina.


    # adding A and B terms to check results
    data[, A := runoff + qdwf - rate_constant_network_storage * network_storage]
    data[, B := runoff + qdwf - shift(network_flow, 1L)] # default is type 'lag', so 1L means one previous timestep


    ### Scenario in one step ### Implementation checked by Steffen, just changed from function to direct data.table assignment
    data[, scenario :=
             fcase(  ( B >= 0 ) & ( A >= B ) | ( B <= 0 ) & ( A >= 0 ), "a", # B<=0 here in appendix, means network overflow 0, but important distinction for tank overflow
                     ( B >= 0 ) & ( A <= B )  &  ( A >= B * exp(-rate_constant_network_storage)), "b", # due to mail: A>=Bexp, equal returns exactly 0, but scenario matters for the tank overflow calculation
                     ( B <= 0 ) & ( A >= B ) & ( A <= B * exp(-rate_constant_network_storage)), "c", # B <= 0 in appendix, again exactly 0 should still not be d for tank overflow calculations
                     default = "d"
    )]


    ### tau1 ###
    # used in tank overflow calculations
    data[, tau1 := fcase(
        scenario %in% c("a", "d"), 0,
        scenario %in% c("b", "c"), log(B/A) / rate_constant_network_storage
    )]


    ### Network overflow E'(t) ### Equation 5
    fun_network_overflow <- function(A,
                                     B,
                                     scenario,
                                     rate_constant_network_storage) {

        ratio <- B / A
        safe_ratio <- ifelse(ratio > 0, ratio, NA_real_) # to avoid warnings for log(B/A) when negative

        ratio_A_B <- A / B
        safe_ratio_A_B <- ifelse(ratio_A_B > 0, ratio_A_B, NA_real_)

        Eprime <- fcase(

            # scenario a:
            scenario == "a",
            A - B / k1 * (1 - exp(-k1)),

            # scenario b:
            ## ln(A/B) in the Excel files, I think B/A is correct, but left at A/B for now to be able to compare
            # scenario == "b",
            # A * (1 - (1 + log(safe_ratio)) / k1) +
            #     B / k1 * exp(-k1),
            scenario == "b",
            A * (1 - (1 + log(safe_ratio_A_B)) / k1) +
                B / k1 * exp(-k1),

            # scenario c:
            scenario == "c",
            A / k1 * (1 + log(safe_ratio)) -
                B / k1,

            # scenario d (no overflow)
            default = 0
        )

        return(Eprime)
    }

    data[, network_overflow := pmax(0, fun_network_overflow(A, B, scenario, rate_constant_network_storage))]







    ### virtual volume and tank volume ###
    fun_virtual_volume_and_tank_volume <- function(
        runoff,
        network_flow,
        tau1,
        k2,                   # rate_constant_tank_storage
        qdwf,
        k1,                   # rate_constant_network_storage
        W1,                   # network_storage
        k1W1,                 # network_storage_mult_rate
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
             "tank_volume_W") := fun_virtual_volume_and_tank_volume(runoff, network_flow, tau1, rate_constant_tank_storage, qdwf, rate_constant_network_storage,
                                                                  network_storage, network_storage_mult_rate, tank_volume_init, scenario)]


    ### tau2 ### equation 11 in appendix B
    data[, part1 := rate_constant_network_storage * network_storage - rate_constant_tank_storage * shift(tank_volume_W)]
    data[, part2 := rate_constant_network_storage * network_storage - rate_constant_tank_storage * tank_storage]
    data[, tau2 :=  fifelse(part2 != 0 & part1 > 0, pmax(0, pmin(1, log(part1/part2) / rate_constant_tank_storage)), 0)]

    data[, c("part1", "part2") := NULL] # not needed further


    ### Ea(t) ### Equation 12 in appendix B
    data[, Ea := (1 - tau2) * (network_storage_mult_rate - (rate_constant_tank_storage * tank_storage))]

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
                               fcase(tank_volume[t - 1] == tank_storage & virtual_volume_d[t] >= tank_storage, 0, # tank full from beginning and stays full -> overflow from time 0
                                     tank_volume[t - 1] == tank_storage,                                       (tank_volume[t - 1] - tank_storage) / (tank_volume[t - 1] - virtual_volume_d[t]), # the ratio
                                     tank_volume[t - 1] == virtual_volume_d[t],                                1, # if tau can't be determined (and Wt-1 < W2), tau2 is 1 (case where it is 0 covered by first expression)
                                     virtual_volume_d[t] >= tank_storage,                                      (tank_volume[t - 1] - tank_storage) / (tank_volume[t - 1] - virtual_volume_d[t]), # the ratio
                                     default = 1)
                         ))
        }
        return(v)
    }

    data[, t2_1 := fun_t2_1(tank_volume_W, virtual_volume_d, network_storage_mult_rate, rate_constant_tank_storage, tank_storage)]


    ### Ed(t) ### Equation 20 (?) in appendix B

    data[, Ed := fifelse(
        shift(data$tank_volume) < tank_storage,
        # case: tank not full at previous timestep
        (runoff + qdwf - rate_constant_tank_storage * tank_storage) * (1 - t2_1) +
            (runoff + qdwf - shift(data$network_flow)) / rate_constant_network_storage *
            (exp(-rate_constant_network_storage) - exp(-rate_constant_network_storage * t2_1)),

        # case: tank full at previous timestep
        (runoff + qdwf - rate_constant_tank_storage * tank_storage) * t2_1 +
            (t2_1 + qdwf - shift(data$network_flow)) / rate_constant_network_storage *
            (exp(-rate_constant_network_storage * t2_1) - 1)
    )]



    ### t2_2 ###
    data[ ,t2_2 := pmin(1, (shift(tank_volume_W) - tank_storage) / (shift(tank_volume_W) - virtual_volume_b)) * tau1]


    ### Eb ###
    data[, Eb := # maybe change this to a safe_ratio in log if warnings annoying
             (runoff + qdwf - rate_constant_tank_storage * tank_storage) * (tau1 - t2_2) + (runoff + qdwf - shift(network_flow)) / rate_constant_network_storage *
             (exp(-rate_constant_network_storage * tau1) - exp(-rate_constant_network_storage * t2_2)) +
             (network_storage * rate_constant_network_storage - rate_constant_tank_storage * tank_storage) *
             (1 - pmin(1, tau1 + pmax(0, log((network_storage * rate_constant_network_storage - virtual_volume_b_t1 * rate_constant_tank_storage) / # tau1 capped at 1 here
                                                 (network_storage * rate_constant_network_storage - rate_constant_tank_storage * tank_storage)) / rate_constant_tank_storage)))]


    ### t2_3 ### # implementation fits the Excel version and makes sense with the appendix
    data[, t2_3 := fcase(virtual_volume_c_t1 < tank_storage, tau1, # here the same as condition tau > tau1; if tau2''' == tau1, no overflow for that integral, Ec collapses to just the tau<=tau1 part
                         virtual_volume_c_t1 == tank_volume_W, 1, # if the virtual volume at tau1 the same as the tank volume at the end of the timestep, tau1 must have been 1 and tau2''' becomes 1 in its formula
                         rep(TRUE,.N), tau1 + (1 - tau1) * (virtual_volume_c_t1 - tank_storage) / (virtual_volume_c_t1 - tank_volume_W) # this is basically the else part
                         )]
    ### Ec ###
    data[, Ec := pmax(0, (runoff + qdwf - (rate_constant_tank_storage * tank_storage)) * (t2_3 - tau1) + ((runoff + qdwf - network_flow) / rate_constant_network_storage) *
                          (exp(-rate_constant_network_storage * t2_3) - exp(-rate_constant_network_storage * tau1)) +
                          ((network_storage * rate_constant_network_storage) - (rate_constant_tank_storage * tank_storage)) * (tau1 - pmin(tau1, tau2)))]




    ### tank_overflow ###
    data[, tank_overflow := pmax(0, fcase(scenario == "a", Ea,
                                          scenario == "b", Eb,
                                          scenario == "c", Ec,
                                          scenario == "d", Ed))]


    ### Duration ###
    data[, duration := fifelse(pmax(network_overflow, tank_overflow) > 0, 1, 0)] # if one (or both) of network and tank overflow, then this timestep is 1 and counts towards the duration

    ### Rain_event ### # == spill numbers in Excel; 1 = start; 2 = rain continues; 0 = no rain
    data[, rain_event := fifelse(
        shift(precipitation) == 0 & precipitation > 0, 1L,
        fifelse(
            shift(precipitation) > 0 & precipitation > 0, 2L,
            0L
        )
    )]

    ### Frequency overflow ### # not used further, not found in Excel, marks overflow events, so can be used later to calculate how many events there were
    data[, frequency_overflow := fifelse(duration == 1 & shift(duration, type = "lead") == 0, 1, 0)] # no , fill = 0 means NA in first row

    ### Total overflow ###
    data[, total_overflow := tank_overflow + network_overflow]

    ### CSO and Rain volumes per rain event
    # already gives total at event end, was cumulative and then total before; this is per RAIN event as in Barcelona
    # marking end of rain event to simplify the cso volume per event calculation (where rain_event==2 or 1 transitions to 0)
    data[, rain_end := fifelse(rain_event != 0 & shift(rain_event, type = "lead", fill = 0) == 0, 1L, 0L)]

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


    return(data)
}
