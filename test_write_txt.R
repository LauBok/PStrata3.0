Y.formula <- formula(Y + D1 + D2 ~ x1 + x2)
Y.family <- gaussian(link = "identity")
S <- c(0, 1, 3, 5, 15)
ER <- c(0, 5, 15)

write.txt(Y.formula = Y.formula, Y.family = Y.family, S = S, ER = ER,
          filename = "test/new/1_test_p2s5c0.txt")
