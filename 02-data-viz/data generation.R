## Tucci data

tucci = data.frame(b = seq(-3.0, 3.0, length.out = 300),
                   d = rep(-0.05, n = 300))
tucci$c =  1/(1+exp(tucci$d-tucci$b))              

plot(tucci$b, tucci$c)

## Mau data

mau_tx = data.frame(num_patients = c(0, 17, 27, 7, 2, 0, 0, 1, 7, 22, 23, 2),
                 noms = c(2,3,4,5,6,7, 2,3,4,5,6,7),
                 tx = c("pre", "pre", "pre", "pre", "pre", "pre", "post","post","post","post","post","post"))

write_csv(mau_tx, "mau_tx.csv")

mau_age = data.frame(age = c(c(seq(55, 60, length.out = 10)), 
                             c(seq(61, 65, length.out = 11)),
                             c(seq(66, 70, length.out = 11)),
                             c(seq(71, 75, length.out = 17)),
                             c(seq(76, 80, length.out = 7)),
                             c(seq(81, 85, length.out = 5)),
                             c(seq(86, 91, length.out = 5))
                             ))
mau_age$age_range = ifelse(mau_age$age < 61, "55-60",
                           ifelse(mau_age$age >= 61 & mau_age$age <= 65, "61-65", 
                                  ifelse(mau_age$age >= 66 & mau_age$age <= 70, "66-70",
                                         ifelse(mau_age$age >= 71 & mau_age$age <= 75, "71-75",
                                                ifelse(mau_age$age >= 76 & mau_age$age <= 80, "76-80",
                                                       ifelse(mau_age$age >= 81 & mau_age$age <= 85, "81-85","86-91"))))))

write_csv(mau_age, "mau_age.csv")                     
