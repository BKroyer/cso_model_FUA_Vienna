# data[, A := runoff + qdwf - k1 * W1]
# data[, B := runoff + qdwf - shift(network_flow, 1L)]

# Realistic values for A: 0 to circa 50 ? (if bigger, design massively misjudged)
# Realistic values for B: circa -50 to 50, but > 0 for case b, so again 0 to 50

A_vals <- seq(0.01, 50.001, 1)
B_vals <- seq(0.01, 50.001, 1)
k1_vals <- 1

network_overflow_with_B_A <- function(A, B, k1 = k1_vals){
    return( A * (1 - (1 + log(B/A)) / k1) + B / k1 * exp(-k1))
}

network_overflow_with_A_B <- function(A, B, k1 = k1_vals){
    return( A * (1 - (1 + log(A/B)) / k1) + B / k1 * exp(-k1))
}



data_temp <- CJ(A = A_vals, B = B_vals)
data_temp[, ratio := A/B]
data_temp[, overflow_B_A := network_overflow_with_B_A(A, B)]
data_temp[, overflow_A_B := network_overflow_with_A_B(A, B)]
data_temp[, diff := overflow_B_A - overflow_A_B]
data_temp[, old_to_new := overflow_A_B/overflow_B_A]

data_temp[, pct_diff := 100 * (overflow_A_B - overflow_B_A) / overflow_A_B]
# Avoid Inf/NaN if overflow_A_B ~ 0
data_temp[overflow_A_B == 0, pct_diff := NA_real_]

data_unique <- data.table(ratio = unique(data_temp$ratio))
#data_unique[, overflow_old_new := net]


p1 <- ggplot(data_temp, aes(x = A, y = B, fill = diff)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
    labs(title = "Difference between log(B/A) and log(A/B) in network overflow",
         fill = "B_A - A_B") +
    theme_bw()
p1


data_temp[, ratio := A/B]

data_ratio <- data_temp[, .(
    mean_diff = mean(diff, na.rm = TRUE),
    median_diff = median(diff, na.rm = TRUE),
    max_diff = max(diff, na.rm = TRUE),
    pct_diff_mean = mean(pct_diff, na.rm = TRUE)
), by = ratio]

# Plot mean difference vs ratio
ggplot(data_ratio, aes(x = ratio, y = mean_diff)) +
    geom_line(color = "red") +
    labs(title = "Mean absolute difference vs A/B ratio", x = "A/B", y = "Mean diff (B_A - A_B)") +
    theme_minimal()

# plot of ratio old results to correct ones
p3 <- ggplot(data_temp, aes(x = A, y = B, fill = old_to_new)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
    labs(title = "Ratio in network overflow using log(A/B) vs. log(B/A)",
         fill = "A_B/B_A") +
    theme_bw()
p3


data_temp[, ratio_bin := cut(ratio, breaks = seq(0, 50, 0.5))]

ggplot(data_temp, aes(x = ratio_bin, y = pct_diff)) +
    geom_boxplot(outlier.size = 0.5) +
    labs(title = "Percentage difference distribution by A/B ratio",
         x = "A/B ratio", y = "% difference") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))


data_temp[, rel_diff := diff / A]  # or /B depending on reference

ggplot(data_temp, aes(x = ratio, y = k1_vals[1], fill = rel_diff)) +
    geom_tile() +
    scale_fill_gradient2(low="blue", mid="white", high="red", midpoint=0) +
    labs(title = "Relative difference as function of A/B ratio", x="A/B", y="k1", fill="Relative diff") +
    theme_minimal()
