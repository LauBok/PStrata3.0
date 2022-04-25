Y.formula <- formula(Y + D1 + D2 ~ x1 + x2)
Y.family <- binomial(link = "logit")
S <- c(0, 1, 3)
ER <- c(0, 3)

write.txt(Y.formula = Y.formula, Y.family = Y.family, S = S, ER = ER,
          filename = "test/new/1_test_p1s3c3.txt")
