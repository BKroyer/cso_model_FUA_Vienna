# Script to visualise prec data, cso and event results, sensitivity analysis results, bias to validation data
# by Bettina Kroyer


# overview precipitation plots -------------------------------------------------

dim(prec_dt)

prec_dt[, date := as.Date(time)]
prec_dt[, year_month := format(time, "%Y-%m")]
prec_dt[, year := format(time, "%Y")]


# daily precipitation sum per gridcode
prec_daily <- prec_dt[
    , .(precip_mm_day = sum(precipitation_mm, na.rm = TRUE)),
    by = .(gridcode, date)
]


p1 <- ggplot(data= prec_daily, aes(x = date, y = precip_mm_day)) +
    geom_line(color = "steelblue2", linewidth = 0.6, alpha = 1) +
    geom_point(color = "darkblue", size = 2, alpha = 0.6) +
    theme_bw() +
    labs(y = "Precipitation [mm]", x = "") +
    scale_y_continuous(breaks = seq(0, 50, 5))

p1

pdf("prec_dt_plot_daily.pdf", width = 8, height = 5)
print(p1)
dev.off()



prec_monthly <- prec_dt[
    , .(precip_mm_month = sum(precipitation_mm, na.rm = TRUE)),
    by = .(gridcode, year_month)
]


p2 <- ggplot(data= prec_monthly, aes(x = year_month, y = precip_mm_month, group = 1)) +
    geom_point(color = "darkblue") +
    geom_line(color = "steelblue2") +
    theme_bw() +
    labs(y = "Precipitation [mm]", x = "")

p2



# annual precipitation for AoI over 2010 - 2024 --------------------------------

area_gridcodes <- data.table(gridcode = urb_3035$gridcode, area_gridcode = expanse(urb_3035, unit = "km"))
area_gridcodes[, weight := area_gridcode/sum(area_gridcode)]

setkey(area_gridcodes, gridcode)

annual_sum_dt <- rbindlist(
    lapply(2010:2024, function(year) {

        precip_temp <- readRDS(
            file.path(path_intermediate_res, paste0("precipitation_ts_settlements_", year, ".rds")))

        # result <- precip_temp[, .(annual_sum = sum(precipitation_mm * weight), by = gridcode)]
        result <- precip_temp[
            area_gridcodes,
            on = "gridcode"
        ][
            , .(annual_mm_weighted = sum(precipitation_mm * weight)),
        ]

        rm(precip_temp)
        gc()

        result[, year := year]
    })
)

prec_sum_over_time <- ggplot(data = annual_sum_dt, aes(x = year, y = annual_mm_weighted)) +
    theme_bw() +
    geom_line(color = "steelblue3", linewidth = 2) +
    geom_point(color = "darkblue", size = 3.5)+
    scale_x_continuous(breaks = seq(2010, 2024)) +
    labs(y = "Area-weighted annual precipitation in AoI [mm]", x = "")
prec_sum_over_time

pdf("annual_prec_sum_over_time.pdf", width = 7, height = 5)
print(prec_sum_over_time)
dev.off()



# results plot per validation region -------------------------------------------
library(gridExtra)
validation_plot <- function(validation_region){

    res_cso <- setDT(read.xlsx(path_out, sheet = "overflow_yearly"))
    cso_col <- c(res_cso[metric == "total [Mm3y]", model]) *10^6 * factor_enlarge_validation
    dwf_col <- c(res_cso[metric == "DWF volume [Mm3]", model]) *10^6 * factor_enlarge_validation

    prec_col <- c(res_cso[metric == "total annual precipitation [mm]", model])

    time_temp <- yr_begin:yr_end
    if (validation_region){
        val_dat <- val_value
    }

    used_params <- params[[1]][1, 2:7]
    param_text <- paste0(
        "Model parameters\n",
        "k0 = ", used_params$k0, "\n",
        "W0 = ", used_params$W0, "\n",
        "dn = ", used_params$dn, "\n",# (AUT/GER: 30)
        "dt = ", used_params$dt, "\n",
        "W1 = ", used_params$W1, "\n",
        "W2 = ", used_params$W2
    )

    dat_temp <- data.table(cso = cso_col, prec = prec_col, time = time_temp, dwf = dwf_col)



    # plotting settlement types next to each other
    dat_temp$settlement_type <- "WWTP catchments"

    res_cso <- setDT(read.xlsx("data_NOTREAD/intermediate_results/AUT_FUA_rep_2021_2022_2023_2024_Jul17.xlsx", sheet = "overflow_yearly"))
    cso_col <- c(res_cso[metric == "total [Mm3y]", model]) *10^6 * factor_enlarge_validation
    dwf_col <- c(res_cso[metric == "DWF volume [Mm3]", model]) *10^6 * factor_enlarge_validation
    dat_temp2 <- data.table(cso = cso_col, prec = prec_col, time = time_temp, dwf = dwf_col)
    dat_temp2$settlement_type <- "FUAs"

    res_cso <- setDT(read.xlsx("data_NOTREAD/intermediate_results/AUT_rep_2021_2022_2023_2024_Jul17.xlsx", sheet = "overflow_yearly"))
    cso_col <- c(res_cso[metric == "total [Mm3y]", model]) *10^6 * factor_enlarge_validation
    dwf_col <- c(res_cso[metric == "DWF volume [Mm3]", model]) *10^6 * factor_enlarge_validation
    dat_temp3 <- data.table(cso = cso_col, prec = prec_col, time = time_temp, dwf = dwf_col)
    dat_temp3$settlement_type <- "Smaller settlements"

    dat_temp <- rbind(dat_temp, dat_temp2, dat_temp3)
    ###





    if (validation_region){
        p_wo_text <- ggplot(data = dat_temp, aes(x = time, y = cso)) +
            theme_bw() +
            geom_col(fill = "darkolivegreen3", width = 0.5, alpha = 0.9)+#, linewidth = 2) +
            geom_point(aes(color = "Modelled CSO volume"), size = 4, shape = 21, fill = "grey30", stroke = 2) +
            geom_hline(aes(yintercept = val_dat, color = "Validation data: CSO volume"), linewidth = 1.5, linetype = "solid", alpha = 0.9) + #, color = "lightpink"
            geom_hline(aes(yintercept = mean(cso),  color = "Modelled mean annual CSO volume"), linewidth = 1.5, linetype = "dashed", alpha = 0.9) +
            geom_col(aes(y = dwf, color = "Modelled DWF content"), width = 0.48, fill = "brown4", alpha = 0.3, linewidth = 0.0000001) +

            scale_color_manual(values = c("Modelled CSO volume" = "darkolivegreen3",
                                          "Validation data: CSO volume" = "pink3",
                                          "Modelled mean annual CSO volume" = "darkolivegreen",
                                          "Modelled DWF content" = "brown4")) +


            ylab("CSO volume [m³ per year]") +
            xlab("") +
            labs(shape = "", color = "") +
            theme(legend.position = "top") +
            scale_x_continuous(breaks = seq(yr_begin, yr_end), limits = c(yr_begin-0.3, yr_end+0.3), expand = c(0,0))+
            scale_y_continuous(labels = scales::number_format(accuracy = 1))
    }else{
    #     p_wo_text <- ggplot(data = dat_temp, aes(x = time, y = cso)) +
    #         theme_bw() +
    #         geom_col(fill = "darkolivegreen3", width = 0.5, alpha = 0.9)+#, linewidth = 2) +
    #         geom_point(aes(color = "Modeled CSO volume"), size = 4, shape = 21, fill = "grey30", stroke = 2) +
    #         geom_hline(aes(yintercept = mean(cso),  color = "Modeled mean annual CSO volume"), linewidth = 1.5, linetype = "dashed", alpha = 0.9) +
    #         geom_col(aes(y = dwf, color = "Modeled DWF content"), width = 0.48, fill = "brown4", alpha = 0.3, linewidth = 0.0000001) +
    #
    #         scale_color_manual(values = c("Modeled CSO volume" = "darkolivegreen3",
    #                                       "Modeled mean annual CSO volume" = "darkolivegreen",
    #                                       "Modeled DWF content" = "brown4")) +
    #
    #
    #         ylab("CSO volume [m³ per year]") +
    #         xlab("") +
    #         labs(shape = "", color = "") +
    #         theme(legend.position = "top") +
    #         scale_x_continuous(breaks = seq(yr_begin, yr_end), limits = c(yr_begin-0.3, yr_end+0.3), expand = c(0,0))+
    #         scale_y_continuous(labels = scales::number_format(accuracy = 1))
    # }

    # settlements types
    p_wo_text <- ggplot(data = dat_temp, aes(x = time, y = cso/10^6, fill = settlement_type)) +
            theme_bw() +
            geom_col(position = "dodge", width = 0.7, alpha = 0.9)+#, linewidth = 2) + fill = "darkolivegreen3",
            geom_point(aes(shape = "Modelled CSO volume", color = settlement_type), fill = "grey30", stroke = 2, size = 4,
                       position = position_dodge(width = 0.7)) +
            geom_col(aes(y = dwf/10^6, color = "Modelled DWF content"), width = 0.68, alpha = 0.3, linewidth = 0.6, position = "dodge") +

            scale_fill_manual(values = c("WWTP catchments" = "#A6D854", #"darkolivegreen3",
                                     "FUAs" = "#FFC20E",
                                     "Smaller settlements" = "lightpink")) +

            scale_color_manual(values = c("Modelled DWF content" = "brown4"),
                               guide = guide_legend(override.aes = list(fill = "white"))) +

            scale_shape_manual(values = c("Modelled CSO volume" = 21),
                               guide = guide_legend(override.aes = list(fill = "grey30", color = "grey60"))) +
            # scale_size_manual(values = c("WWTP catchments" = 4,
            #                              "FUAs" = 4,
            #                              "Smaller settlements" = 4)) +


            ylab("CSO volume [Mm³ per year]") +
            xlab("") +
            labs(color = "", fill = "", shape = "") +
            theme(legend.position = "top") +
            scale_x_continuous(breaks = seq(yr_begin, yr_end), limits = c(yr_begin-0.5, yr_end+0.5), expand = c(0,0))+
            scale_y_continuous(breaks = seq(0,200,25), labels = scales::number_format(accuracy = 1), minor_breaks = F)
    }



    validation_code_text <- validation_code
    if (!validation_region){
        validation_code_text <- ""
    }

    validation_annotation <- ggplot() +
        annotate("text", x = 0.5, y = 0, label = validation_code_text, size = 12, fontface = "bold") +
        theme_void() +
        theme(plot.margin = margin(b = 0.5))


    param_plot <- ggplot() +
        annotate("text", x = 0, y = 0.145, label = param_text, hjust = 0, vjust = 0, size = 3.2) +
        theme_void() +
        xlim(0, 1) + ylim(0, 1)

    empty_plot <- ggplot() +
        annotate("text", x = 0, y = 0.25, label = "", hjust = 0, vjust = 1, size = 3.2) +
        theme_void() +
        xlim(0, 1) + ylim(0, 1)


    prec_add <- ggplot(data = dat_temp, aes(x = time, group = 1)) +
        geom_ribbon(aes(ymin = 0, ymax = prec), fill = "lightblue3", color = "lightblue4", alpha = 0.8, linewidth = 1) +
        theme_bw() +
        #scale_x_continuous(breaks = paste0(seq(yr_begin, yr_end), "-01"), labels = seq(yr_begin, yr_end))
        scale_x_continuous(breaks = seq(yr_begin, yr_end), limits = c(yr_begin-0.3, yr_end+0.3), expand = c(0,0)) +
        scale_y_continuous(breaks = seq(0,1500,200))+#, limits = c(0,1000)) +
        labs(x = "", y = "Precipitation [mm]")

    pdf(file.path(path_intermediate_res, paste0(area_of_interest_name, "_resultsplot.pdf")), width = 9.2, height = 6.5)


    full_plot <-grid.arrange(grobs = list(p_wo_text, param_plot, validation_annotation, prec_add, empty_plot),
                             heights = c(0.25, 0.5, 1.25, 1.3), widths = c(2,1,0.5),
                             layout_matrix = rbind(c(1,1,NA),
                                                   c(1,1,3),
                                                   c(1,1,2),
                                                   c(4,4,5))
    )
    dev.off()

}

validation_plot(validation_region = validation_region)




# bias plot --------------------------------------------------------------------
# plot showing all validation regions results as offset from validation value in per cent (middle line: 1: perfect match)
file_list <- list.files(path_intermediate_res, pattern = paste0(datum, "\\.xlsx$"), full.names = TRUE)
val_files <- file_list[grepl(paste0("^[A-Z]_", datum, "\\.xlsx$"), basename(file_list))]

val_dt <- NULL
for (val_file in val_files){
    val_res <- setDT(read.xlsx(val_file, sheet = 3))
    gridcode <- letter2 <- sub("_.*$", "", basename(val_file))

    cso_val <- val_res[metric == "total [Mm3y]", c("year", "model")]
    val_value <- as.numeric(val_data[Code == gridcode, Value])

    val_temp <- data.table(gridcode = gridcode, year = cso_val$year, cso_val = cso_val$model * 10^6, val_value = val_value)
    val_dt <- rbind(val_dt, val_temp)
}

#val_dt <- val_dt[year != "2024"]
# val_dt <- val_dt[, .("cso_val" = mean(cso_val),
#            val_value = unique(val_value)),
#        by = gridcode]

# # just the means compare (one dot per gridcode)
# for (val_file in val_files){
#     val_res <- setDT(read.xlsx(val_file, sheet = 4))
#     gridcode <- letter2 <- sub("_.*$", "", basename(val_file))
#
#     cso_val <- as.numeric(val_res[metric == "total [Mm3y]", "annual_means_scaled"] * 10^6)
#     val_value <- as.numeric(val_data[Code == gridcode, Value])
#
#     val_temp <- data.table(gridcode = gridcode, cso_val = cso_val, val_value = val_value)
#     val_dt <- rbind(val_dt, val_temp)
# }

val_dt[, bias := (cso_val - val_value) / val_value * 100]

bias_plot <- ggplot(val_dt, aes(x = gridcode, y = bias, color = year)) +#, color = year
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    geom_segment(aes(x = gridcode, xend = gridcode, y = 0, yend = bias),
                 color = "grey50") +
    geom_point(size = 3) + #, alpha = 0.7
    scale_y_continuous(breaks = seq(floor(min(val_dt$bias)/20)*20,
                                    ceiling(max(val_dt$bias)/20)*20,
                                    by = 20),
                       labels = function(x) paste0(x, "%")) +
    labs(y = "Bias to validation value (%)", x = "Gridcode", color = "") + #, color = ""
    theme_bw()

pdf("bias_plot_extracted_CS_laterTP_scalingchanged.pdf", width = 8, height = 5)
#pdf("bias_plot_best_reasonable_wo2024.pdf", width = 8, height = 5)
print(bias_plot)
dev.off()



# events plots -----------------------------------------------------------------
res_event <- setDT(read.xlsx(path_out, sheet = "event_metrics_yearly"))
res_nr_events <- rbind(
    data.table(year = res_event$year,
               nr_of_events = res_event$nr_of_rain_events,
               event_duration = as.numeric(res_event$mean_rain_duration_per_event),
               mean_volume_per_event_mm = as.numeric(res_event$mean_rain_volume_per_event_mm),
               event_type = "rain event"),

    data.table(year = res_event$year,
               nr_of_events = res_event$nr_of_overflow_events,
               event_duration = as.numeric(res_event$mean_overflow_duration_per_event),
               mean_volume_per_event_mm = as.numeric(res_event$mean_overflow_volume_per_event_mm),
               event_type = "overflow event"))


scaling_duration <- max(res_nr_events$nr_of_events)/max(res_nr_events$mean_volume_per_event_mm)
p_event <- ggplot(data = res_nr_events, aes(x = year)) +
    theme_bw() +
    geom_col(aes(y = nr_of_events, fill = event_type), position = "dodge", width = 0.7) +
    #geom_col(aes(y = nr_of_overflow_events, color = "annual mean number of overflow events"), fill = "pink2", position = "dodge") +
    theme(legend.position = "top") +
    labs(color = "", fill = "", y = "Modeled annual number of events", x = "") +
    scale_fill_manual(values = c(
        "overflow event" = "pink2",
        "rain event" = "lightblue3"
    )) +

    geom_point(aes(y = mean_volume_per_event_mm*scaling_duration, color = event_type, group = event_type), size = 5) +
    geom_line(aes(y = mean_volume_per_event_mm*scaling_duration, color = event_type, group = event_type), linewidth = 1) +
    scale_color_manual(values = c(
        "overflow event" = "pink3",
        "rain event" = "lightblue4"
    )) +
    scale_y_continuous(breaks = seq(0,400, 20),
                        sec.axis = sec_axis(~./scaling_duration, name = "Mean volume per event [mm]", breaks = seq(0,30,2))) +
    theme(
        #axis.title.y = element_text(color = temperatureColor, size=13),
        axis.title.y.left = element_text(color = "grey40")
    )

p_event

pdf(file.path(path_intermediate_res, paste0("Austria_default_paramsdn30_later_", "_eventsplot.pdf")), width = 8, height = 5)
print(p_event)
dev.off()




# sensitivity analysis plots ---------------------------------------------------
file_list <- list.files(file.path(path_intermediate_res, "G_sensitivity_param_plots"), pattern = paste0(datum, "\\.xlsx$"), full.names = TRUE)

sens_files <- file_list[grepl(paste0(validation_code,"_sensitivity"), basename(file_list))]

sens_files <- sens_files[grepl("_prec_", sens_files)]

sens_dt <- NULL
for (sens_file in sens_files){
    sens_res <- setDT(read.xlsx(sens_file, sheet = 4))
    gridcode <- letter2 <- sub("_.*$", "", basename(sens_file))
    param_nam_split <- strsplit(gsub(paste0("_", datum, ".xlsx"), "", sens_file),"_")
    param_val <- as.numeric(param_nam_split[[1]][[length(param_nam_split[[1]])]])
    param_nam <- param_nam_split[[1]][[length(param_nam_split[[1]]) - 1]]

    cso_val <- as.numeric(sens_res[metric == "total [Mm3y]", c("annual_means_scaled")])

    sens_temp <- data.table(param = param_nam, gridcode = gridcode, param_val = param_val, cso_val = cso_val * 10^6)
    sens_dt <- rbind(sens_dt, sens_temp)
}

p_sens <- ggplot(data = sens_dt, aes(x = param_val, y = cso_val)) +
    theme_bw() +
    geom_line() +
    geom_point(size = 2.5) +
    labs(x = param_nam, y = "Modeled CSO volume [m³], Application period") +
    #scale_y_continuous(breaks = seq(100000, 250000, 10000), limits = c(100000, 250000)) +
    scale_x_continuous(breaks = varying_param)+
    labs(x = "Precipitation change factor ")
p_sens

saveRDS(sens_dt, file.path(path_intermediate_res, "E_sensitivity_param_plots", paste0(validation_code ,"_", param_nam,"_sensitivity_plot.rds")))


# arrange plot of all 6 params
combined_dt <- NULL
#for (pp in c("W0", "k0", "W1", "W2", "dn", "dt")){
for (pp in c("dt", "dn", "W2", "W1", "k0", "W0")){
    #p <- setDT(readRDS(file.path(path_intermediate_res, "G_sensitivity_param_plots", paste0("G_", pp,"_sensitivity_plot.rds"))))
    p <- setDT(readRDS(file.path(path_intermediate_res, "E_sensitivity_param_plots", paste0("E_", pp,"_sensitivity_plot.rds"))))

    combined_dt <- rbind(combined_dt, p)
}


break_list <- list(
    dt = seq(2, 20, 1), #dt
    dn = seq(3, 30, 1), #dn
    W2 = seq(0.2, 9.8, 0.8), #W2
    W1 = seq(0.6, 5, 0.4), #W1
    k0 = seq(0.1, 2.1, 0.2), # k0
    W0 = seq(0.2, 5, 0.4) # W0

)

scale_x_custom <- function(...) {
    scale_x_continuous(..., breaks = function(x) {
        # attempt to infer current panel by looking at x range
        # choose the break set whose range overlaps x
        # sel <- sapply(break_list, function(b) {
        #     (min(x) >= min(b) - 2 && max(x) <= max(b) + 2) ||
        #         (min(x) >= min(b) - 1*(max(b)-min(b)) && max(x) <= max(b) + 1*(max(b)-min(b)))
        # })
        # if (any(sel)) return(break_list[[which(sel)[1]]])
        # fallback: pretty breaks for that panel range
        pretty(x, n = 10)
    })
}

p_sens <- ggplot(data = combined_dt, aes(x = param_val, y = cso_val)) +
    theme_bw() +
    geom_line() +
    geom_point(size = 2.5) +
    labs(x = param_nam, y = "Modeled CSO volume [m³], Application period") +
    facet_wrap(~param, scales = "free_x", ncol = 2) +
    #scale_y_continuous(breaks = seq(200000, 300000, 10000), limits = c(200000, 300000)) +
    scale_x_custom() +
    labs(x  = "")

p_sens

pdf(file.path(path_intermediate_res, "G_sensitivity_param_plots", "G_param_plot_prec.pdf"), width = 6, height = 5)

print(p_sens)
dev.off()
