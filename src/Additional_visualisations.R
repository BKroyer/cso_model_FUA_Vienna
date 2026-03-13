# Script to visualise prec data
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




# quick Eisenstadt test

# change to general plot function in the validation_functions script and add the prec as transparent shape

area_gridcodes <- data.table(gridcode = gridcode_to_process, area_gridcode = expanse(urb_3035[urb_3035$gridcode %in% gridcode_to_process], unit = "km"))
area_gridcodes[, weight := area_gridcode/sum(area_gridcode)]

setkey(area_gridcodes, gridcode)

prec_monthly <- prec_dt[
    , .(precip_mm_month = sum(precipitation_mm, na.rm = TRUE)),
    by = .(gridcode, year_month)
]

prec_monthly_weighted_sum <- prec_monthly[
    area_gridcodes,
    on = "gridcode"
][
    , .(monthly_mm_weighted = sum(precip_mm_month * weight)),
    by = year_month
]

prec_yearly<- prec_dt[
    , .(precip_mm_year = sum(precipitation_mm, na.rm = TRUE)),
    by = .(gridcode, year)
]

prec_yearly_weighted_sum <- prec_yearly[
    area_gridcodes,
    on = "gridcode"
][
    , .(yearly_mm_weighted = sum(precip_mm_year * weight)),
    by = year
]

cso_col <- c(0.2932692, 0.2064973, 0.2117195, 0.1853364, 0.3506581, 0.1484669, 0.2234508) *10^6

time_temp <- c(2010:2016)
val_dat <- 221655
used_params <- params[1, 2:7]
param_text <- paste0(
    "Model parameters\n",
    "k0 = ", used_params$k0, "\n",
    "W0 = ", used_params$W0, "\n",
    "dn = ", used_params$dn, "\n",
    "dt = ", used_params$dt, "\n",
    "W1 = ", used_params$W1, "\n",
    "W2 = ", used_params$W2
)

dat_temp <- data.table(cso = cso_col, time = time_temp)

p_eisen<- ggplot(data = dat_temp, aes(x = time, y = cso)) +
    theme_bw() +
    # geom_line(color = "darkolivegreen3", linewidth = 2) +
    geom_point(aes(color = "Modeled CSO volume [m³ per year]"), size = 4, shape = 21, fill = "grey30", stroke = 2) +
    geom_hline(aes(yintercept = val_dat, color = "Validation data: CSO volume [m³ per year]"), linewidth = 1.5, linetype = "solid", alpha = 0.9) + #, color = "lightpink"
    geom_hline(aes(yintercept = mean(cso),  color = "Modeled mean annual CSO volume over 2010 to 2016"), linewidth = 1.5, linetype = "dashed", alpha = 0.9) +
    scale_color_manual(values = c("Modeled CSO volume [m³ per year]" = "darkolivegreen3", "Validation data: CSO volume [m³ per year]" = "pink3", "Modeled mean annual CSO volume over 2010 to 2016" = "darkolivegreen")) +
    ylab("CSO volume [m³ per year]") +
    xlab("") +
    labs(shape = "", color = "") +
    theme(legend.position = "top") +
    scale_x_continuous(breaks = seq(yr_begin, yr_end))


param_plot <- ggplot() +
    annotate("text", x = 0, y = 0.25, label = param_text, hjust = 0, vjust = 1, size = 3.2) +
    theme_void() +
    xlim(0, 1) + ylim(0, 1)

p_eisen + param_plot + plot_layout(widths = c(4, 1))

# add precipitation as plot below with the same x axis
#prec_add <- ggplot(data = prec_monthly, aes(x = year_month, y = precip_mm_month, group = 1)) +
prec_add <- ggplot(data = prec_yearly, aes(x = year, y = precip_mm_year, group = 1)) +
    geom_line() +
    theme_bw()# +
    #scale_x_continuous(breaks = paste0(seq(yr_begin, yr_end), "-01"), labels = seq(yr_begin, yr_end))
    #scale_x_continuous(breaks = seq(yr_begin, yr_end))
prec_add
