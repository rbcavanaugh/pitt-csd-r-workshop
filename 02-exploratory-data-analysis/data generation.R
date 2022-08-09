tucci = data.frame(b = seq(-3.0, 3.0, length.out = 300),
                   d = rep(-0.05, n = 300))
tucci$c =  1/(1+exp(tucci$d-tucci$b))              

plot(tucci$b, tucci$c)

write.csv(tucci, )
