name <- c("Jacob", "Mason", "Ethan", "Noah",
           "William", "Sophia", "Emma",
           "Isabella", "Olivia")
sex <- c("M", "M", "M", "M", "M", "F", "F",
         "F", "F")
n <- length(name)

#dob_from_start <- floor(runif(n, min=0, max=4*365))
#dob <- mdy("1/1/2002") + ddays(dob_from_start)
#rates <- rnorm(n, mean=2, sd=0.3)

#Making the id set
dob_string = sprintf("%s/%s/%s", month(dob),
                     day(dob), 
                     substr(year(dob), 3, 4))
id <- data.frame(name=name, sex=sex, DOB=dob_string)


#That should be it for the permanent
#variables. Now for the data.

make_df <- function(dates, heights, year) {
  newyear <- mdy(sprintf("01/01/%d", year))
  newdaynum <- floor(runif(n, min=1, max=366))
  newdates <- newyear + ddays(newdaynum)
  delta_days = newdates - dates #as integer
  delta_years = as.numeric(delta_days) / 365.25
  newheights = heights + rates * delta_years + 
    rnorm(n, sd=0.2)
  #newheights = round(newheights)
  df <- data.frame(name=name, date=newdates, 
                   height=newheights)
  return(df)
}

data_sets = vector('list', length=4)
data_sets[[1]] <- make_df(dob, 34, 2009)
for (i in 2:4) {
  old_df = data_sets[[i-1]]
  data_sets[[i]] <- make_df(old_df$date,
                            old_df$height,
                            2008+i)
}

save_data_set <- function(i) {
  #i is the df number, that's it
  df <- data_sets[[i]]
  df$height = round(df$height)
  m <- month(df$date)
  d <- day(df$date)
  y <- year(df$date)
  df$date <- sprintf("%d/%d/%d", m, d, y)
  #df
  filename = sprintf("Data Set %d.csv", i)
  
  #Sort by date of visit
  ord <- order(data_sets[[i]]$date)
  df <- df[ord, ]
  write.csv(df, file=filename, row.names=FALSE)
}

for (i in 1:4) {save_data_set(i)}

