Y.formula <- formula(y + d1 + d2 ~ x1 + x2)
Y.family <- gaussian(link = "identity")
S <- c(0, 1, 3, 5, 7, 15)
ER <- c(0, 5, 7, 15)

write.txt(Y.formula = Y.formula, Y.family = Y.family, S = S, ER = ER,
          filename = "test.txt")
