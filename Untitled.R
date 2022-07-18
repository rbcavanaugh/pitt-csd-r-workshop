a <- list(c(1,2,3), c(4,5,6))
b <- list(c("a", 'b', 'c'), c('d', 'e', 'f'))
test <- append(list(a), list(b))
# turn the list of 2 lists with 2 vectors within each list into 
# a list of 4 elements. 
test_unlisted = test2 = unlist(test, recursive = 2)
# give them fake names if necessary
names(test2) = paste0("x", seq(1, length(test2), 1))
# turn them into columns
df = dplyr::bind_cols(test2)
df


i = 1
length_of_lists = 3
list_out = list()
for(i in 1:length(test)){
  
  names(test[[i]]) = paste0("x", seq(1, length(test[[i]]), 1))
  list_out = t(bind_rows(test[i]) )
 
}

data.frame(V1 = c(1,4), V2 = c(2,5), V3 = c(3,6), V4 = c("a", "d"), V5 = c("b", "e"), V6 = c("c", "f"))
