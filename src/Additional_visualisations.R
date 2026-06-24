# Script to visualise prec data, cso and event results, sensitivity analysis results, bias to validation data
# by Bettina Kroyer


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







# annual precipitation for AoI over 2010 - 2024

# area_aoi <- sum(expanse(aoi_3035, unit = "km")) #km²
# area_austria <- 83883.87 #km²

area_gridcodes <- data.table(gridcode = urb_3035$gridcode, area_gridcode = expanse(urb_3035, unit = "km"))
area_gridcodes[, weight := area_gridcode/sum(area_gridcode)]

#library(sf)
#area_gridcodes <- data.table(
#    gridcode = urb_3035$gridcode,
#    area_km2  = as.numeric(st_area(urb_3035)) / 1e6
#)
#area_gridcodes[, weight := area_km2 / sum(area_km2)]

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

    if (validation_region){
        p_wo_text <- ggplot(data = dat_temp, aes(x = time, y = cso)) +
            theme_bw() +
            geom_col(fill = "darkolivegreen3", width = 0.5, alpha = 0.9)+#, linewidth = 2) +
            geom_point(aes(color = "Modeled CSO volume"), size = 4, shape = 21, fill = "grey30", stroke = 2) +
            geom_hline(aes(yintercept = val_dat, color = "Validation data: CSO volume"), linewidth = 1.5, linetype = "solid", alpha = 0.9) + #, color = "lightpink"
            geom_hline(aes(yintercept = mean(cso),  color = "Modeled mean annual CSO volume"), linewidth = 1.5, linetype = "dashed", alpha = 0.9) +
            geom_col(aes(y = dwf, color = "Modeled DWF content"), width = 0.48, fill = "brown4", alpha = 0.3, linewidth = 0.0000001) +

            scale_color_manual(values = c("Modeled CSO volume" = "darkolivegreen3",
                                          "Validation data: CSO volume" = "pink3",
                                          "Modeled mean annual CSO volume" = "darkolivegreen",
                                          "Modeled DWF content" = "brown4")) +


            ylab("CSO volume [m³ per year]") +
            xlab("") +
            labs(shape = "", color = "") +
            theme(legend.position = "top") +
            scale_x_continuous(breaks = seq(yr_begin, yr_end), limits = c(yr_begin-0.3, yr_end+0.3), expand = c(0,0))+
            scale_y_continuous(labels = scales::number_format(accuracy = 1))
    }else{
        p_wo_text <- ggplot(data = dat_temp, aes(x = time, y = cso)) +
            theme_bw() +
            geom_col(fill = "darkolivegreen3", width = 0.5, alpha = 0.9)+#, linewidth = 2) +
            geom_point(aes(color = "Modeled CSO volume"), size = 4, shape = 21, fill = "grey30", stroke = 2) +
            #geom_hline(aes(yintercept = val_dat, color = "Validation data: CSO volume"), linewidth = 1.5, linetype = "solid", alpha = 0.9) + #, color = "lightpink"
            geom_hline(aes(yintercept = mean(cso),  color = "Modeled mean annual CSO volume"), linewidth = 1.5, linetype = "dashed", alpha = 0.9) +
            geom_col(aes(y = dwf, color = "Modeled DWF content"), width = 0.48, fill = "brown4", alpha = 0.3, linewidth = 0.0000001) +

            scale_color_manual(values = c("Modeled CSO volume" = "darkolivegreen3",
                                          #"Validation data: CSO volume" = "pink3",
                                          "Modeled mean annual CSO volume" = "darkolivegreen",
                                          "Modeled DWF content" = "brown4")) +


            ylab("CSO volume [m³ per year]") +
            xlab("") +
            labs(shape = "", color = "") +
            theme(legend.position = "top") +
            scale_x_continuous(breaks = seq(yr_begin, yr_end), limits = c(yr_begin-0.3, yr_end+0.3), expand = c(0,0))+
            scale_y_continuous(labels = scales::number_format(accuracy = 1))
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
    #pdf(file.path(path_intermediate_res, paste0("Traisen_reduced_default_params", "_resultsplot.pdf")), width = 9.2, height = 6.5)
    #full_plot <- grid.arrange(p_wo_text, param_plot, prec_add, empty_plot, nrow = 2, heights = c(2, 1.3), widths = c(2,0.3))


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
#validation_plot(validation_region = TRUE)





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


#library(scales)

#val_dt$gridcode <- factor(val_dt$gridcode, levels = val_dt$gridcode)

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


# bias plot for the Entsorgungsgebiete
val_data <- setDT(read.xlsx("data/Validation_data.xlsx"))
val_data <- val_data[, .(NAME, Value, Code)]
setnames(val_data, "gridcode", "NAME", skip_absent = T)


res_data <- setDT(read.xlsx("data_NOTREAD/intermediate_results/Entsorgungsgebiete_Eurostat_manualCS_HEF_manualpop_2021_2022_2023_2024_Jun24.xlsx", sheet = "results_per_gridcode"))
#res_data2 <- setDT(read.xlsx("data_NOTREAD/intermediate_results/Entsorgungsgebiete_EUROSTAT_extractedCS_2021_2022_2023_2024_Jun24.xlsx", sheet = "results_per_gridcode"))

#res_data[gridcode == "EMREG_BE_Abwasserverband Region Hohenems"]

setnames(res_data, "gridcode", "NAME")
res_data <- res_data[ , .(NAME, year, total_overflow_Mm3y)]
res_data$NAME[res_data$NAME == "ebswien kläranlage & tierservice"] <- "Wien Kanal"
# combine ARA Pulkau and ARA Schrattenthal for the validation data
temp <- res_data[NAME == "ARA Pulkau" | NAME == "ARA Schrattenthal",
         .(total_overflow_Mm3y = sum(total_overflow_Mm3y)), by = year]
temp$NAME <- "Gemeindeabwasserverband- Pulkau-Schrattenthal-Pillersdorf"
res_data <- rbind(res_data, temp)
res_data <- res_data[year < 2024]
res_data <- res_data[, .(total_overflow_Mm3y = mean(total_overflow_Mm3y)), by = NAME]

plot_data <- merge(val_data, res_data, by = "NAME")
plot_data[, bias := (total_overflow_Mm3y * 10^6 - Value) / Value * 100]

bias_plot <- ggplot(plot_data, aes(x = Code, y = bias)) +#, color = year
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    geom_segment(aes(x = Code, xend = Code, y = 0, yend = bias),
                 color = "grey50") +
    geom_point(size = 3) + #, alpha = 0.7
    scale_y_continuous(limits = c(-63.5,63.5), breaks = seq(-60, 60, #seq(floor(min(plot_data$bias)/20)*20,
                                    #ceiling(max(plot_data$bias)/20)*20,
                                    by = 10),
                       labels = function(x) paste0(x, "%")) +
    labs(y = "Bias to validation value (%)", x = "Gridcode") + #, color = ""
    theme_bw()# +
    #theme(axis.text.x = element_text(angle = 90, vjust = 0.7))
#bias_plot

#pdf("bias_Entsorgung_EUROSTAT_wo2024_extracted_CS.pdf", width = 9, height = 10)
pdf("bias_Entsorgung_EUROSTAT_wo2024_manual_CS_MEAN_woHEF_manualpop_q125.pdf", width = 8, height = 6)
print(bias_plot)
dev.off()




# check regions with few inhabitants

austria_res_temp <- setDT(read.xlsx(file.path(path_intermediate_res, "Austria_default_dn302010_2011_2012_2013_2014_2015_2016_Mar24.xlsx"), sheet = 2))
austria_prec_add <- setDT(read.xlsx(file.path(path_intermediate_res, "Austria_default_dn302010_2011_2012_2013_2014_2015_2016_Mar24.xlsx"), sheet = 1))
austria_res_temp <- merge.data.table(austria_res_temp, austria_prec_add[, c("gridcode", "mean_annual_prec")], by = "gridcode")


# ggplot(data = austria_res_temp, aes(x = population, y = total_overflow_Mm3y)) +
#     scale_x_log10() +
#     geom_point() +
#     theme_bw()
#
# ggplot(data = austria_res_temp, aes(x = mean_annual_prec/population, y = total_overflow_Mm3y)) +
#     geom_point() +
#     theme_bw()


# events plots
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




# sensitivity analysis plots
#file_list <- list.files(file.path(path_intermediate_res, "G_sensitivity_param_plots"), pattern = paste0(datum, "\\.xlsx$"), full.names = TRUE)
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

#saveRDS(sens_dt, file.path(path_intermediate_res, "G_sensitivity_param_plots", paste0(validation_code ,"_", param_nam,"_sensitivity_plot.rds")))
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

#pdf(file.path(path_intermediate_res, "G_sensitivity_param_plots", "G_param_plot.pdf"), width = 9, height = 12)
pdf(file.path(path_intermediate_res, "G_sensitivity_param_plots", "G_param_plot_prec.pdf"), width = 6, height = 5)

print(p_sens)
dev.off()



# k1_fun <- function(qdwf, dn, W1){
#     return(qdwf*dn/W1)
# }
#
#
# k1_c <- c()
# for (dn_temp in seq(3,30,1)){
#     k1_c <- c(dn_c, k1_fun(0.0684449, dn_temp, 5))
# }

qdwf <- 0.0684449

qdwf*5/5


qdwf_pop <- (manual_pop/1.4251) * 0.21 /8/1000

qdwf_pop*30/5

dn3 <- readRDS(file.path(path_intermediate_res, paste0("data_dn", 3)))
dn4 <- readRDS(file.path(path_intermediate_res, paste0("E_data_dn", 4)))
dn5 <- readRDS(file.path(path_intermediate_res, paste0("E_data_dn", 5)))
dn6 <- readRDS(file.path(path_intermediate_res, paste0("E_data_dn", 6)))
dn8 <- readRDS(file.path(path_intermediate_res, paste0("E_data_dn", 8)))
dn11 <- readRDS(file.path(path_intermediate_res, paste0("E_data_dn", 11)))


plot_G_dwf <- readRDS("C:\\Users\\simulation\\bkroyer\\git_clone\\cso-modell-upper-danube\\data_NOTREAD\\intermediate_results\\G_DWF per capita_sensitivity_plot.rds")


dt12 <- readRDS(file.path(path_intermediate_res, paste0("data_dt", 12)))
dt14 <- readRDS(file.path(path_intermediate_res, paste0("data_dt", 14)))
dt15 <- readRDS(file.path(path_intermediate_res, paste0("data_dt", 15)))
dt16 <- readRDS(file.path(path_intermediate_res, paste0("data_dt", 16)))
