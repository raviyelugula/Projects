str(data.work)
sapply(sapply(data.work,length(unique)),length)
count.fields(unique(data.work$STORE_NUM))

for(i in 1:25)
print(c(nrow(unique(data.work[i])),names(data.work[i])))


trimws(names(data.work[i]))
