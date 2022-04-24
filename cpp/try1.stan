
data {
    int<lower=0> N;
    int<lower=0> PS;
    int<lower=0> PG;
    int<lower=0, upper=0> Z[N];
    int<lower=0, upper=0> D[N];
    
    matrix[N, PS] XS;
    matrix[N, PG] XG;
}

transformed data {
    int S[1];
}

parameters {
    matrix[0, PS] beta_S;
    matrix[1, PG] beta_G;
}

model {
    for (n in 1:N) {
        int length;
        real log_prob[1];
        log_prob[1] = 0;
        for (s in 2:1) {
            log_prob[s] = XS[n] * beta_S[s - 1];
        }
        {
            real log_l[length];
            target += log_sum_exp(log_l) - log_sum_exp(log_prob);
        }
    }
}

generated quantities {
    vector[1] mean_effect;
    {
        matrix[N, 1] expected_mean = XG * beta_G';
        matrix[N, 1] log_prob = XS * beta_S';
        vector[1] denom;
        vector[1] numer;
        for (n in 1:N) {
            log_prob[n] -= log_sum_exp(log_prob[n]);
        }
        for (s in 1:1) denom[s] = mean(exp(log_prob[:, s]));
        for (g in 1:1) {
            numer[g] = mean(expected_mean[:, g] .* exp(log_prob[:, S[g]]));
            mean_effect[g] = numer[g] / denom[S[g]];
        }
    }
}

