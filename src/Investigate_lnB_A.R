# Investiagting the difference the typo ln(A/B) instead of ln(B/A) can make

# Realistic values for A: 0 to circa 50 ? (if bigger, design massively misjudged)
# Realistic values for B: circa -50 to 50, but > 0 for case b, so again 0 to 50

B_fun <- function(R,Q){
    return(R + Q)
}


A_fun <- function(R, Q, dn){
    return(R + Q * (1-dn))
}


k1_fun <- function(dn, Q, W1){
    return((dn*Q)/W1)
}


logterm <- function(A,B,k1){
    return(log(B/A)*(A/k1))
}


R_vec <- seq(0, 50, 0.5)    # mm
Q_vec <- seq(0, 1, 0.05)    # mm
dn_vec <- seq(0, 35, 1)     # -
W1_vec <- seq(0.1, 10, 0.1) # mm

max_val <- -Inf
max_params <- NULL

results <- list()
for (R in R_vec) {
    for (Q in Q_vec) {
        B <- B_fun(R, Q)
        # precompute matrices for dn and W1
        # compute A for all dn
        A_dn <- R + Q * (1 - dn_vec)    # vector length length(dn_vec)
        valid_dn_idx <- which(A_dn > 0 & B >= A_dn)  # A>0 and B>=A
        if (length(valid_dn_idx) == 0) next

        # loop over valid dn only
        for (i in valid_dn_idx) {
            dn <- dn_vec[i]
            A <- A_dn[i]
            # k1 varies with W1; vectorize over W1
            k1_vec <- k1_fun(dn, Q, W1_vec)
            valid_k1_idx <- which(k1_vec > 0)
            if (length(valid_k1_idx) == 0) next
            k1_v <- k1_vec[valid_k1_idx]
            # compute logterm vectorized
            v <- logterm(A, B, k1_v)
            # filter finite values
            v[!is.finite(v)] <- NA
            if (all(is.na(v))) next
            vmax <- max(v, na.rm = TRUE)
            if (vmax > max_val) {
                max_val <- vmax
                W1_best <- W1_vec[valid_k1_idx][which.max(v)]
                max_params <- list(R=R, Q=Q, dn=dn, W1=W1_best, A=A, B=B,
                                    k1=k1_fun(dn,Q,W1_best))
            }
        }
        # store max per (R,Q) for plotting
        # for speed compute maximal v across dn and W1 (approx)
        # brute force small loops:
        max_v_RQ <- -Inf
        for (dn in dn_vec) {
            A <- R + Q * (1 - dn)
            if (!(A > 0 && B >= A)) next
            for (W1 in W1_vec) {
                k1 <- k1_fun(dn, Q, W1)
                if (!(k1 > 0)) next
                v <- logterm(A, B, k1)
                if (!is.finite(v)) next
                if (v > max_v_RQ) max_v_RQ <- v
            }
        }
        results[[length(results)+1]] <- data.table(R=R, Q=Q, dn=dn, A=A, B=B, W1=W1,vmax=max_v_RQ)
    }
}

dt <- rbindlist(results)
# remove -Inf rows
dt <- dt[is.finite(vmax)]

# Plot heatmap of vmax over R and Q
p <- ggplot(dt, aes(x=R, y=Q, fill=vmax)) +
    geom_raster(interpolate=FALSE) +
    scale_fill_viridis_c(option="magma", na.value="white") +
    labs(fill="max logterm", x="R (mm)", y="Q (mm)") +
    theme_bw()

# mark best point
if (!is.null(max_params)) {
    p <- p + geom_point(aes(x=max_params$R, y=max_params$Q), color="cyan", size=3)
}

print(p)
max_val
max_params
