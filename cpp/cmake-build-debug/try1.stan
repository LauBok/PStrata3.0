functions {
    real identity_func(real x) {
        return x;
    }
}

data {
    int<lower=0> N;
    int<lower=0> PS;
    int<lower=0> PG;
    int<lower=0, upper=1> Z[N];
    int<lower=0, upper=3> D[N];
    real Y[N];
    matrix[N, PS] XS;
    matrix[N, PG] XG;
}

transformed data {
    int S[8];
    S[1] = 1;
    S[2] = 2;
    S[3] = 2;
    S[4] = 3;
    S[5] = 3;
    S[6] = 4;
    S[7] = 5;
    S[8] = 6;
}

parameters {
    matrix[5, PS] beta_S;
    matrix[8, PG] beta_G;
    real<lower=0> sigma[8];
}

model {
    beta_S[:, 1] ~ normal(0.000000, 1.000000);
    beta_G[:, 1] ~ normal(0.000000, 1.000000);
    to_vector(beta_S[:, 2:PS]) ~ normal(0.000000, 1.000000);
    to_vector(beta_G[:, 2:PG]) ~ normal(0.000000, 1.000000);
    sigma ~ inv_gamma(0.100000, 0.100000);
    for (n in 1:N) {
        int length;
        real log_prob[6];
        log_prob[1] = 0;
        for (s in 2:6) {
            log_prob[s] = XS[n] * beta_S[s - 1]';
        }
        if (Z[n] == 0 && D[n] == 0)
            length = 3;
        else if (Z[n] == 0 && D[n] == 1)
            length = 2;
        else if (Z[n] == 0 && D[n] == 3)
            length = 1;
        else if (Z[n] == 1 && D[n] == 0)
            length = 1;
        else if (Z[n] == 1 && D[n] == 1)
            length = 2;
        else if (Z[n] == 1 && D[n] == 3)
            length = 3;
        {
            real log_l[length];
            if (Z[n] == 0 && D[n] == 0) {
                // strata: 0 1 2
                log_l[1] = log_prob[1] + normal_lpdf(Y[n] | identity_func(XG[n] * beta_G[1]'), sigma[1]);
                log_l[2] = log_prob[2] + normal_lpdf(Y[n] | identity_func(XG[n] * beta_G[2]'), sigma[2]);
                log_l[3] = log_prob[3] + normal_lpdf(Y[n] | identity_func(XG[n] * beta_G[4]'), sigma[4]);
            }
            else if (Z[n] == 0 && D[n] == 1) {
                // strata: 3 4
                log_l[1] = log_prob[4] + normal_lpdf(Y[n] | identity_func(XG[n] * beta_G[6]'), sigma[6]);
                log_l[2] = log_prob[5] + normal_lpdf(Y[n] | identity_func(XG[n] * beta_G[7]'), sigma[7]);
            }
            else if (Z[n] == 0 && D[n] == 3) {
                // strata: 5
                log_l[1] = log_prob[6] + normal_lpdf(Y[n] | identity_func(XG[n] * beta_G[8]'), sigma[8]);
            }
            else if (Z[n] == 1 && D[n] == 0) {
                // strata: 0
                log_l[1] = log_prob[1] + normal_lpdf(Y[n] | identity_func(XG[n] * beta_G[1]'), sigma[1]);
            }
            else if (Z[n] == 1 && D[n] == 1) {
                // strata: 1 3
                log_l[1] = log_prob[2] + normal_lpdf(Y[n] | identity_func(XG[n] * beta_G[3]'), sigma[3]);
                log_l[2] = log_prob[4] + normal_lpdf(Y[n] | identity_func(XG[n] * beta_G[6]'), sigma[6]);
            }
            else if (Z[n] == 1 && D[n] == 3) {
                // strata: 2 4 5
                log_l[1] = log_prob[3] + normal_lpdf(Y[n] | identity_func(XG[n] * beta_G[5]'), sigma[5]);
                log_l[2] = log_prob[5] + normal_lpdf(Y[n] | identity_func(XG[n] * beta_G[7]'), sigma[7]);
                log_l[3] = log_prob[6] + normal_lpdf(Y[n] | identity_func(XG[n] * beta_G[8]'), sigma[8]);
            }
            target += log_sum_exp(log_l) - log_sum_exp(log_prob);
        }
    }
}

generated quantities {
    vector[8] mean_effect;
    {
        matrix[N, 8] expected_mean = XG * beta_G';
        matrix[N, 6] log_prob;
        vector[6] denom;
        vector[8] numer;
        log_prob[:, 1] = 0 * log_prob[:, 1];
        log_prob[:, 2:6] = XS * beta_S';
        for (n in 1:N) {
            log_prob[n] -= log_sum_exp(log_prob[n]);
        }
        for (s in 1:6) denom[s] = mean(exp(log_prob[:, s]));
        for (g in 1:8) {
            numer[g] = mean(expected_mean[:, g] .* exp(log_prob[:, S[g]]));
            mean_effect[g] = numer[g] / denom[S[g]];
        }
    }
}

