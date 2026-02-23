# Script to visualise prec data
# by Bettina Kroyer


dim(prec_dt)

prec_dt[, date := as.Date(time)]
prec_dt[, year_month := format(time, "%Y-%m")]


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

pdf("prec_dt_plot_monthly.pdf", width = 8, height = 5)
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
