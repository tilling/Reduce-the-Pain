require(lubridate)

id = read.csv('ID.csv', as.is=TRUE)
id <- within(id, DOB <- mdy(DOB))

#Test on a single data set first;
#when it works, you can build a "loop"
#(really lapply) for it
# visit1 = read.csv('Data Set 1.csv', as.is=TRUE)

#get_visit does that for variable i:
#read in Data Set i.csv, convert the
#date to a date object, and assign 
#to visit(i) in the global environment.
get_visit <- function(i) {
  filename = sprintf("Data Set %d.csv", i)
  visit = read.csv(filename, as.is=TRUE)
  visit <- within(visit, date <- mdy(date))
  return(visit)
#   outname = sprintf("visit%d", i)
#   assign(outname, visit, env=.GlobalEnv)
}

#for (i in 1:4) get_visit(i)
#Instead of that, we use lapply
#and Reduce. Who needs loops and
#extra made-up variables?
visit_list = lapply(FUN=get_visit, X=as.list(1:4))
all_visits <- Reduce(rbind, visit_list)

df_long <- left_join(id, all_visits, by="name")

#How to do a transformation inside:
get_age <- function(DOB, date) {
  as.numeric(date - DOB) / 365.25
}

df_long <- within(df_long, age <- get_age(DOB, date))


#split * apply * return

#plyr format
require(plyr)
est_growth <- function(subdf) {
  mod <- lm(height ~ age, data=subdf)
  return(coef(mod)[2])
}

growth_rates1 <- ddply(.data=df_long, .variables='name', .fun=est_growth)

showline <- function(df) {
  lines(df$age, df$height)
}

plot(df_long$age, df_long$height, type='p',
     xlab='age', ylab='height')
d_ply(.data=df_long, .variables='name', .fun=showline)


detach(package:plyr)


#dplyr format
require(dplyr)

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

#summarize(df_by_name, zz = lines(age, height))


#Try plotting the different lines
# plot.new()
# plot.window(xlim=range(df_long$age),
#             ylim=range(df_long$height))
# 
# 
# #You can do better than this, but it's good
# #for showing the idea
# # df_wide <- join_all(dfs=list(id, visit1, visit2, visit3, visit4),
# #                     by='name')
# # 
# # df_wide <- summarize(group_by(df_wide, name), 
# #                      age1)
# 
# visit2 = read.csv('Data Set 2.csv', as.is=TRUE)
# visit3 = read.csv('Data Set 3.csv', as.is=TRUE)
# visit4 = read.csv('Data Set 4.csv', as.is=TRUE)
# df_wide <- join_all(dfs=list(id, visit1, visit2, visit3, visit4),
#                     by='name')
# df_wide <- within(df_wide,
#                   )



# id <- within(id, DOB <- mdy(DOB))
# visit1 <- within(visit1, date <- mdy(date))
# df1 <- left_join(id, visit1, by='name')
# df1a <- within(df1, age <- date-DOB)

#Use one big summarize at the end?

#visit2 = read.csv('Data Set 2.csv', as.is=TRUE)
