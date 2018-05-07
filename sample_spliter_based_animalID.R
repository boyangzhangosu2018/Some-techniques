##sample spliter
lb= 19
ub= 23
n=219
ans<<-c()
while (length(ans) < lb) {
  a=sample(1:n,size = 1)
  b=as.numeric(final_data[a,]$ANIMID)
  sample_number=which(final_data$ANIMID == b)
  unique_check = length(which(ans %in% sample_number))
  length_check = length(ans)+length(sample_number)
  if(unique_check == 0 && length_check <= ub){
    ans <<- c(ans,sample_number)
  }
}
ans
length(unique(ans))
test_data <- final_data[ans,]
head(test_data)
train_data <- final_data[-ans,]
head(train_data)
##Factor analysis
?        