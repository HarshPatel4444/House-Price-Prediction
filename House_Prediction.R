# First load the data
data = read.csv("F:/R/HOUSE_PRICE/House price prediction-Dataset.csv")
View(data)

#Handle the null value
na.fail(data)
ncol(data)

#####    MODE FUNCTION  #####

mode_man= function(v){
  uniq=unique(v)
  uniq[which.max(tabulate(match(v,uniq)))]
}

#Replace Null value to meaningfull value like mean value or mode or median.
for(i in 1:ncol(data)){
  if((class(data[,i])== "numeric") | (class(data[,i])=="integer")){
    if ( length(table(data[,i])) ==2 ){
      data[is.na(data[,i]),i] = mode_man(data[,i])
    }
    data[is.na(data[,i]),i] = mean(data[,i],na.rm = TRUE)  
  }
  else{
    if((class(data[,i])== "character") | (class(data[,i])=="factor")){
      data[is.na(data[,i]),i] = mode_man(data[,i])
    }
  } 
}

#now check Null is remain in data or not. 
na.fail(data)
View(data)

# now time to feature selection for this we use FSelector library.
library(FSelector)

# gain ratio find the which feature is most important for our model.
gain = gain.ratio(SalePrice ~ . , data = data)
View(gain)

# Now subset the important column.
gain = subset(gain , attr_importance > 0.20)
important_column = row.names(gain)
important_column

# make formula for our model
formula= as.formula(paste("SalePrice ~",paste(names(data[,important_column]),collapse = "+")))
formula

# finally make our model and after we can predict house price.
model = lm(f,data = data)
summary(model)







