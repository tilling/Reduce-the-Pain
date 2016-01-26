require(lubridate)

id = read.csv('ID.csv', as.is=TRUE)
id <- within(id, DOB <- mdy(DOB))

#Test on a single data set first;
#when it works, you can build a "loop"
#(really lapply) for it
# visit1 = read.csv('Data Set 1.csv', as.is=TRUE)

#get_visit does that for variable i:
#read in "Data Set [i].csv", convert the
#date to a date object, and assign 
#to visit(i) in the global environment.
get_visit <- function(i) {
  filename = sprintf("Data Set %d.csv", i)
  visit = read.csv(filename, as.is=TRUE)
  visit <- within(visit, date <- mdy(date))
  return(visit)
}


#Use lapply and Reduce. Who needs loops and
#extra made-up variables?
#Make a list of the four visit data frames:
visit_list = lapply(FUN=get_visit, X=as.list(1:4))
#Stack them in long format:
all_visits <- Reduce(rbind, visit_list)

#How to do a transformation inside:
get_age <- function(DOB, date) {
  as.numeric(date - DOB) / 365.25
} 

#plyr version
require(plyr)

#Join ID to visits the plyr way
df_long <- join(id, all_visits, by="name")
df_long <- within(df_long, age <- get_age(DOB, date))


#split * apply * return the plyr way

est_growth <- function(subdf) {
  mod <- lm(height ~ age, data=subdf)
  return(coef(mod)[2])
}

growth_rates1 <- ddply(.data=df_long, .variables='name', .fun=est_growth)


#Make plot showing growth of each child:
#Plot data points
with(df_long, plot(age, height, type='p'))
#Function to show age-height lines for any data frame
showline <- function(df) with(df, lines(age, height))
#Break into sub-frames and show age-height lines
d_ply(.data=df_long, .variables='name', .fun=showline)

detach(package:plyr)


#dplyr format
require(dplyr)

df_long <- left_join(id, all_visits, by="name")
df_long <- within(df_long, age <- get_age(DOB, date))

#First: you have to group things, forming
#a new grouped_df.

df_by_name <- group_by(df_long, name)

#The internal function has to be defined
#differently: it's a function of the
#two variables instead of the subdf.

est_growth <- function(x, y) {
  mod <- lm(y ~ x)
  return(coef(mod)[2])
}
growth_rates2 <- summarize(df_by_name, rate = est_growth(age, height))

