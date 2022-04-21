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
    int<lower=0, upper=1> D[N];
    int<lower=1> NR;
    int<lower=1, upper=NR> W[N];
    real Y[N];
    matrix[N, PS] XS;
    matrix[N, PG] XG;
}

transformed data {
    int S[3];
    S[1] = 1;
    S[2] = 2;
    S[3] = 2;
}

parameters {
    matrix[1, PS] beta_S;
    matrix[3, PG] beta_G;
    real<lower=0> sigma[3];
    real rnd_intr[NR];
}

model {
    for (n in 1:N) {
        int length;
        real log_prob[2];
        log_prob[1] = 0;
        for (s in 2:2) {
            log_prob[s] = rnd_intr[W[n]] + XS[n] * beta_S[s - 1]';
        }
        if (Z[n] == 0 && D[n] == 0)
            length = 2;
        else if (Z[n] == 1 && D[n] == 0)
            length = 1;
        else if (Z[n] == 1 && D[n] == 1)
            length = 1;
        {
            real log_l[length];
            if (Z[n] == 0 && D[n] == 0) {
                // strata: 0 1
                log_l[1] = log_prob[1] + normal_lpdf(Y[n] | identity_func(XG[n] * beta_G[1]'), sigma[1]);
                log_l[2] = log_prob[2] + normal_lpdf(Y[n] | identity_func(XG[n] * beta_G[2]'), sigma[2]);
            }
            else if (Z[n] == 1 && D[n] == 0) {
                // strata: 0
                log_l[1] = log_prob[1] + normal_lpdf(Y[n] | identity_func(XG[n] * beta_G[1]'), sigma[1]);
            }
            else if (Z[n] == 1 && D[n] == 1) {
                // strata: 1
                log_l[1] = log_prob[2] + normal_lpdf(Y[n] | identity_func(XG[n] * beta_G[3]'), sigma[3]);
            }
            target += log_sum_exp(log_l) - log_sum_exp(log_prob);
        }
    }
}

generated quantities {
    vector[3] mean_effect;
    {
        matrix[2, PS] beta_S_full = rep_matrix(0, 2, PS);
        vector[2] denom;
        vector[3] numer;
        matrix[N, 2] log_prob;
        matrix[N, 3] expected_mean = XG * beta_G';
        beta_S_full[2:2, :] = beta_S;
        log_prob = XS * beta_S_full';
        for (n in 1:N) {
            log_prob[n] -= log_sum_exp(log_prob[n]);
        }
        for (s in 1:2) {
            denom[s] = mean(exp(log_prob[:, s]));
        }
        for (g in 1:3) {
            numer[g] = mean(expected_mean[:, g] .* exp(log_prob[:, S[g]]));
            mean_effect[g] = numer[g] / denom[S[g]];
        }
    }
}
