#0#
firstName<- "Yanchi"
lastName <- "Li"
print(
  paste(firstName,
        lastName
  )
)

studentID<- "1505113"
print(studentID)

#1#
library(foreign)
df.ex <-read.dta("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")

#2#
install.packages("dplyr")
library(dplyr)
df.ex.yearmonth <- dplyr::filter(df.ex,year == 2013 & month == 12)
dim(df.ex.yearmonth)
df.ex.yearsummer <- dplyr::filter(df.ex,year == 2013 & month == 7 |month == 8 |month== 9)
dim(df.ex.yearsummer)

#3#
df.ex.3a <- dplyr::arrange(df.ex,year,month)

#4#
df.ex.4a <- dplyr::select(df.ex,year,age)
df.ex.4b <- dplyr::select(df.ex,year,age,starts_with("i"))
df.ex.state<-distinct(select(df.ex,state))

#5#
stndz <- function(x){(x - mean(x, na.rm = T))  /  sd(x, na.rm = T)}
nrmlz <- function(x){(x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))}

df.ex.5a <- dplyr::mutate(df.ex, rw.stndz = stndz(rw), rw_nrmlz = nrmlz(rw)) %>%
  select(rw.stndz, rw_nrmlz)

df.ex.5b <- df.ex %>% group_by(year,month) %>% 
  mutate(rw.stndz = stndz(rw), rw_nrmlz = nrmlz(rw),count = n()) %>%
  select(rw.stndz, rw_nrmlz,count)

#6#
df.ex.6 <- df.ex %>% 
  dplyr::group_by(year,month,state)%>%
  dplyr::summarise(
   rw_quantile = quantile(rw, .25, na.rm = T),
   rw_mean = mean(rw, na.rm = T),
   rw_median = median(rw, na.rm = T),
   rw_max = max(rw, na.rm = T),
   count = n())

first <- first(df.ex.6,"rw_mean")
print(first)

