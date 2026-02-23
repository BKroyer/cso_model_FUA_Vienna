#include <Rcpp.h>
using namespace Rcpp;




// [[Rcpp::export]]
NumericVector network_flow_cpp(NumericVector runoff, double qdwf, double k1) {
    int n = runoff.size();
    NumericVector v(n);
    double alpha = exp(-k1);
    v[0] = runoff[0] + qdwf;
    for(int t = 1; t < n; t++) {
        v[t] = (runoff[t] + qdwf) * (1 - alpha) + v[t-1] * alpha;
    }
    return v;
}





// [[Rcpp::export]]
NumericVector surface_storage_cpp(NumericVector precip, double W0, double k0) {
    int n = precip.size();
    NumericVector v(n);
    v[0] = std::min(precip[0], W0);
    for(int t = 1; t < n; t++) {
        if(precip[t] > 0) {
            v[t] = std::min(W0, precip[t] + v[t-1]);
        } else {
            v[t] = std::min(W0, v[t-1] * exp(-k0));
        }
    }
    return v;
}





// [[Rcpp::export]]
NumericVector network_overflow_rcpp(NumericVector A,
                                    NumericVector B,
                                    CharacterVector scenario,
                                    double k1,
                                    bool ln_A_B,
                                    double e_k1) {
    int n = A.size();
    NumericVector E(n);

    for (int i = 0; i < n; i++) {
        double a = A[i];
        double b = B[i];
        double ratio = b / a;
        double ratio_A_B = a / b;
        double safe_ratio_A_B = (ratio_A_B > 0) ? ratio_A_B : NA_REAL;

        double Eprime_b;
        if (!ln_A_B) {
            Eprime_b = a * (1 - (1 + log(ratio)) / k1) + b / k1 * e_k1;
        } else {
            Eprime_b = a * (1 - (1 + log(safe_ratio_A_B)) / k1) + b / k1 * e_k1;
        }

        if (scenario[i] == "a") {
            E[i] = a - b / k1 * (1 - e_k1);
        } else if (scenario[i] == "b") {
            E[i] = Eprime_b;
        } else if (scenario[i] == "c") {
            E[i] = a / k1 * (1 + log(ratio)) - b / k1;
        } else {
            E[i] = 0.0; // scenario "d" or default
        }

        // make sure non-negative
        if (E[i] < 0.0) E[i] = 0.0;
    }

    return E;
}






// [[Rcpp::export]]
List virtual_volume_and_tank_volume_cpp(
        NumericVector runoff,
        NumericVector network_flow,
        NumericVector tau1,
        double k2,
        double qdwf,
        double k1,
        double W1,
        double k1W1,
        double W2,
        IntegerVector scenario   // a=1, b=2, c=3, d=4
) {
    int n = runoff.size();

    NumericVector va(n), vd(n), vb_t1(n), vc_t1(n), vb(n), vc(n), tv(n);


    const double e_k1 = std::exp(-k1); // already computed in R, but fast here, else take as function argument
    const double e_k2 = std::exp(-k2);
    const double k2_k1 = k2 - k1;

    tv[0] = network_flow[0] / k2;

    for (int t = 1; t < n; ++t) {

        double r = runoff[t] + qdwf;
        double nf_prev = network_flow[t - 1];
        double tau1_temp = tau1[t];

        // scenario a
        va[t] = k1W1 / k2 * (1.0 - e_k2) + tv[t - 1] * e_k2;

        // scenario d
        vd[t] =
            (r / k2) * (1.0 - e_k2)
            - ((r - nf_prev) / k2_k1) * (e_k1 - e_k2)
            + tv[t - 1] * e_k2;

        // scenario b: 0 to tau1
        double e_k2_tau = std::exp(-k2 * tau1_temp);
        double e_k1_tau = std::exp(-k1 * tau1_temp);

        vb_t1[t] =
            (r / k2) * (1.0 - e_k2_tau)
            - ((r - nf_prev) / k2_k1) * (e_k1_tau - e_k2_tau)
            + tv[t - 1] * e_k2_tau;

        // scenario c: 0 to tau1 (Excel does: tau1 = 1 always)
        vc_t1[t] = k1W1 / k2 * (1.0 - e_k2) + tv[t - 1] * e_k2;


        // scenario b: tau1 to 1
        vb[t] =
            vb_t1[t] * std::exp(-k2 * (1.0 - tau1_temp))
            + k1W1 / k2 * (1.0 - std::exp(-k2 * (1.0 - tau1_temp)));

        //  scenario c: tau1 to 1
        vc[t] =
            (r / k2) * (1.0 - std::exp(-k2 * (1.0 - tau1_temp)))
            + ((r - nf_prev) / k2_k1) // minus in appendix but then e_k1 term minus e_k2 term
            * (std::exp(-k2 * (1.0 - tau1_temp)) - std::exp(-k1 * (1.0 - tau1_temp)))
            + vc_t1[t] * std::exp(-k2 * (1.0 - tau1_temp));

        // attribute to scenario
        double tv_new;
        if (scenario[t] == 1)      tv_new = va[t];
        else if (scenario[t] == 2) tv_new = vb[t];
        else if (scenario[t] == 3) tv_new = vc[t];
        else                       tv_new = vd[t];

        tv[t] = std::min(tv_new, W2);
    }

    return List::create(
        _["virtual_volume_a"]     = va,
        _["virtual_volume_d"]     = vd,
        _["virtual_volume_b_t1"]  = vb_t1,
        _["virtual_volume_c_t1"]  = vc_t1,
        _["virtual_volume_b"]     = vb,
        _["virtual_volume_c"]     = vc,
        _["tank_volume_W"]        = tv
    );
}




// [[Rcpp::export]]
NumericVector t2_1_cpp(NumericVector tank_volume,
                       NumericVector virtual_volume_d,
                       double W2) {

    int n = tank_volume.size();
    NumericVector v(n, NA_REAL);

    for (int t = 1; t < n; ++t) {

        double W_prev = tank_volume[t - 1];
        double Vd     = virtual_volume_d[t];

        double tau2;

        // tank full from beginning and stays full -> overflow from time 0
        if (W_prev == W2 && Vd >= W2) {
            tau2 = 0.0;

        // tank full previously -> the ratio from appendix B
        } else if (W_prev == W2) {
            tau2 = (W_prev - W2) / (W_prev - Vd);

        // degenerate case -> if tau can't be determined (and Wt-1 < W2), tau2 is 1 (case where it is 0 covered by first expression)
        } else if (W_prev == Vd) {
            tau2 = 1.0;

        // virtual volume exceeds tank -> the ratio from appendix B
        } else if (Vd >= W2) {
            tau2 = (W_prev - W2) / (W_prev - Vd);

        // default
        } else {
            tau2 = 1.0;
        }

        // restrict tau2 to [0, 1]
        if (tau2 < 0.0) tau2 = 0.0;
        if (tau2 > 1.0) tau2 = 1.0;

        v[t] = tau2;
    }

    return v;
}