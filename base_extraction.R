rm(list = ls())
require(XML)
require(tidyverse)
require(stringr)

input.dir <- "./combin_input"
files.v <- dir(path=input.dir, pattern=".*xml")



i <- 2
files.v[i]

# read xml structure from file to .R object
doc.object <- xmlTreeParse(file.path(input.dir, files.v[i]), useInternalNodes=TRUE)

# extract all <word> elements and children into XmlNodeList object
word.nodes <- getNodeSet(doc.object, "//word")


word.list <- xmlApply(word.nodes, xmlToList)




y <- map(word.list, names) %>%
  unlist() %>%
  unique()

y 



y <- y[-which(y == "insertion_id" | y == "artificial") ]


system.time(
for (i in seq_along(y)) {
  
  if ( i == 1) {
    
    z <- word.list %>% map_chr(y[i]) %>%
      data.frame(check.names = FALSE, stringsAsFactors = FALSE)
    
  } else {
    
    z <- word.list %>% map_chr(y[i]) %>%
      cbind(z,  ., stringsAsFactors = FALSE)
    
  }

}
)


word.list[108]

colnames(z) <- y

################### 

# initialize data frame for results
w <- matrix(nrow = length(word.list)) %>%
  as.data.frame()

# extract data from word list and put it in data frame with data for each word on separate row

w <- word.list %>% map_chr(y[1]) %>%  # extract each element in word.list and convert it to a character vector
  cbind(z,  ., stringsAsFactors = FALSE) # insert each vector as column in data frame.


w <-  w[, - which(colnames(w) == ".")]


######################## 

###################### Create all combinations of chosen variables

xxx <- vector("list", length(c(17, 16, 18:39, 13:15)))  # create list with number of elements equal to number of chosen variables

i <- 1
system.time(
for (i in 1:length(y[c(17, 16, 18:39, 13:15)])) {
  xxx[[i]] <-  combn(y[c(17, 16, 18:39, 13:15 )], i, simplify = TRUE) # combn() creates all combinations
}
)

# loop to create vector of names for list of new variables
i <- 1
nomina.v <- NULL
for (i in 1:length(y[c(17, 16, 18:39, 13:15)])) {
  nomina.v <- paste("27", "Choose",  i, collapse = " ") %>%
    append(nomina.v, .)
}

names(xxx) <- nomina.v  # assign names to list

#########################################

check1 <- lengths(xxx)
sum(check1)

# loop for deleting impossible combinations from those generated above
x2 <- vector("list", 27) # initialize list with number of elements equal to number of original varaibles used in comination.

system.time(
for (i in seq_along(xxx)) {
 combs.v <- xxx[[i]]
 
 
 bad_c <- NULL
 
 bad_c <- apply(combs.v, 2, function(x) ("self-morph-person"  %in% x & "self-morph-gender" %in% x))
 combs.v <- subset(combs.v, select = which(bad_c == FALSE))
 
 bad_c <- apply(combs.v, 2, function(x) ("self-morph-person"  %in% x & "self-morph-case" %in% x)) 
 combs.v <- subset(combs.v, select = which(bad_c == FALSE))
 
 bad_c <- apply(combs.v, 2, function(x) ("self-morph-person"  %in% x & "self-morph-degree" %in% x))
 combs.v <- subset(combs.v, select = which(bad_c == FALSE))
 
 bad_c <- apply(combs.v, 2, function(x) ("parent-morph-person"  %in% x & "parent-morph-gender" %in% x)) 
 combs.v <- subset(combs.v, select = which(bad_c == FALSE))
 
 bad_c <- apply(combs.v, 2, function(x) ("parent-morph-person"  %in% x & "parent-morph-case" %in% x)) 
 combs.v <- subset(combs.v, select = which(bad_c == FALSE))
 
 bad_c <- apply(combs.v, 2, function(x) ("parent-morph-person"  %in% x & "parent-morph-degree" %in% x)) 
 combs.v <- subset(combs.v, select = which(bad_c == FALSE))
  
 x2[[i]] <- combs.v
 
}
)
###########################################

combs.v <- xxx[[2]]

bad_c <- NULL

bad_c <- apply(combs.v, 2, function(x) ("self-morph-person"  %in% x & "self-morph-gender" %in% x))
combs.v <- subset(combs.v, select = which(bad_c == FALSE))

bad_c <- apply(combs.v, 2, function(x) ("self-morph-person"  %in% x & "self-morph-case" %in% x)) 
combs.v <- subset(combs.v, select = which(bad_c == FALSE))

bad_c <- apply(combs.v, 2, function(x) ("self-morph-person"  %in% x & "self-morph-degree" %in% x))
combs.v <- subset(combs.v, select = which(bad_c == FALSE))

bad_c <- apply(combs.v, 2, function(x) ("parent-morph-person"  %in% x & "parent-morph-gender" %in% x)) 
combs.v <- subset(combs.v, select = which(bad_c == FALSE))

bad_c <- apply(combs.v, 2, function(x) ("parent-morph-person"  %in% x & "parent-morph-case" %in% x)) 
combs.v <- subset(combs.v, select = which(bad_c == FALSE))

bad_c <- apply(combs.v, 2, function(x) ("parent-morph-person"  %in% x & "parent-morph-degree" %in% x)) 
combs.v <- subset(combs.v, select = which(bad_c == FALSE))

###########################

names(x2) <- nomina.v
check2 <- lengths(x2)

sum(check1) -
sum(check2)


#################################################
# use matrix of combinations of varaible names to create new variables

z[1, zzz[,1]]


zzz.l <- as.list(zzz)

paste0(z[1, zzz[,1]], collapse = "-")

i <- 1
j <- 1
v.list <- list()

zzz[, 59]
i <- 1




for (j in seq_len(ncol(z))) {
  vv <- NULL
  for (i in seq_len(nrow(z))) {
    vv <-  append(vv, paste0(z[i, zzz[, j]], collapse = "-")) %>%
      unlist()
  }
  v.list[[j]] <- vv
} %>% system.time()

zzz[,2]
########################################
########################## populate list object with character vectors of new variables

i <- 1
j <- 1
seq_along(culled.output.list)



v.list <- vector("list",sum (lengths(culled.output.list)))
counter <- 1
var.names <- NULL
for (i in 1:2) {
  
  var.holder <- culled.output.list[[i]]
 # v.list <- vector("list", ncol(var.holder))
 
  
  
  
    for (j in seq_len(ncol(var.holder))) {
      compound.vars <- apply(z, 1, function(x) paste0(x[var.holder[, j]], collapse = "-")) %>%
        as.character()
      
      var.names  <- paste0(var.holder[, j], collapse = "*") %>%
        append(var.names, .)
      
      v.list[[counter]] <- compound.vars
       names(v.list[[counter]]) <- var.names
      counter <- counter + 1
      
    }
  
}


i <- 1


system.time(
for (j in seq_along(x2)) {
  
  zzz <- x2[[j]]
  v.list <- vector("list", ncol(zzz))
  nomina.v <- NULL
  
  t <- system.time(
  for (i in seq_len(ncol(zzz))) {
    sss <- apply(z, 1, function(x) paste0(x[zzz[, i]], collapse = "-")) %>%
      as.character()
    
    nomina.v  <- paste0(zzz[, i], collapse = "*") %>%
      append(nomina.v, .)
    
    v.list[[i]] <- sss
    names(v.list) <- nomina.v
    
  }
  )
  
  print(t)
  
  temp.l<- list()
  temp.l[[1]] <- do.call(cbind, v.list)
  a <- temp.l[[1]]
  colnames(a) <- var.names
  dim (a)
  
  View(a[1:5, 1:20])
  
  write.csv(temp.l[[1]], file = paste0("new_variables", "-", j, ".csv"))
  
}
)

################################ redesign loop to drop na and null before processing??

v.list[[3]][1:10]
nomina.v[3]

v.list2 <- v.list[1:10]

seq_along(v.list)

test <- lapply(v.list, function(x) lapply(x, f_NA)) # insert logical NA for any variable containing the string "NA".
test2 <- lapply(v.list[1:5], function(x) lapply(x, f_NA2))

na.holder <- lapply(test2, function(x) which(is.na(x)))

test.df <- do.call(f_bind, test2) 

test.df <- 

  f_bind <- function(x) {
    cbind.data.frame(unlist(x), stringsAsFactors = FALSE)
  }  
  

lengths(v.list) %>%
  sum()

lengths(test2) %>% 
  sum()

lengths(na.holder) %>%
  sum()

16861203 - 14822187

which(is.na(na.test[[1]]))

which(is.na(test2[, 1]))

is.na(test[[1]])
test2 <- vector("list", 3)
test2[[3]] <- test[[3]]

na.test3 <- cbind.data.frame(unlist(test[[1]]), unlist(test[[2]]), stringsAsFactors = FALSE) # This one works!!

a2 <- do.call(cbind.data.frame, v.list)

colnames(na.test3) <- nomina.v[1:2]
na.test3 <- cbind(token_id, na.test3)


##################### This code block works to handle na values and return a long form data table
seq_len(ncol(zzz))
nomina.v <- NULL
for (i in seq_len(ncol(zzz)))  {
  nomina.v  <- paste0(zzz[, i], collapse = "*") %>%
    append(nomina.v, .)
  
}

f_bind <- function(x) {
  cbind.data.frame(unlist(x), stringsAsFactors = FALSE)
}


test.2 <- lapply(temp.l, f_bind)
dim(test.1[[1]])
temp.df <- test.2[[1]]

test.1[[1]][1:10, 1:10]

na.test.df <- do.call(cbind.data.frame, na.test3)
colnames(na.test.df) <- nomina.v
na.test.df <- cbind.data.frame(token_id, na.test.df, stringsAsFactors = FALSE)

long.na.test <- gather(na.test.df, variable_name, variable_value, -(token_id)) %>%
  drop_na()


holder <- which(str_detect(na.test.df, "null") == TRUE)
which(str_detect(na.test.df[, 38], "null") == TRUE)

f_NullRows <- function(x) { # use holder as input
   which(str_detect(na.test.df[, x], "null") == TRUE)
}

null.row.numbs <- lapply(holder, f_NullRows)

for (i in seq_along(holder))  {
indices <- c(holder[i], null.row.numbs[[i]])
  
}

i <- 1
j <- 1

for (j in seq_along(holder)) {
  for (i in seq_along(null.row.numbs[[j]])) {
    
    na.test.df[null.row.numbs[[j]][i], holder[i] ] <- NA
    
  }
  
}

seq_along(null.row.numbs[[j]])

paste(holder[i], null.row.numbs[[i]])
c(holder[i], null.row.numbs[[i]])


na.test.df[null.collect[[1]], 38]

rep_holder <- rep(holder[1], length(null.row.numbs[[1]]))

na.test.df[null.row.numbs[[1]], rep_holder[1]] <- NA




for (j in 3:length(holder)) { # this one worked !!!
  rep_holder <- rep(holder[j], length(null.row.numbs[[j]]))
  na.test.df[null.row.numbs[[j]], holder[j]] <- NA
  
}

###################################################


na.test3[[1]][1:10,1]
View(na.test3[[1]])
colnames(na.test3[[1]])


long.na.test <- gather(na.test3, variable_name, variable_value, -(token_id)) %>%
  drop_na()

colnames(na.test3) <- nomina.v[1:2]

na.test3 <- do.call(cbind.data.frame, test2) %>%
  cbind(token_id, .)

token_id <- seq_along(test[[1]]) %>%
  paste0("token-", .)

test.df <- do.call(cbind, test) %>%
  cbind(token_id, .) %>%
  as.data.frame()

test.tib <- as_tibble(test.df)

short.test.tib <- gather(test.tib, variable_name, variable_value, -(token_id)) %>%
  drop_na()

which(short.test.tib == "NA")

short.test.tib[[1]]

test.m <- matrix(c)

##########################################
####################################### Identify values containing "NA"

v.list[[1]][1]
length(v.list[[1]])
str(v.list)
v.list$`self-depdist*self-relation*self-morph-pos*self-morph-person*self-morph-number`[2]

which(str_detect(v.list[[1]], "NA") == TRUE) %>%
  length()

f_NA_count <- function(x) {
  which(str_detect(x, "NA") == TRUE | str_detect(x, "null") == TRUE ) %>%
    length()
  
}



na.count <- NULL
na.count <- append(na.count, unlist(lapply(v.list, f_NA_count)))
na.count[[1]]
lapply(na.count, sum)

which(str_detect(v.list[[20000]], "NULL") == TRUE)
v.list[[1]][1:50]
v.names <- names(v.list)
parents <- which(str_detect(v.names, "parent-morph-person") == TRUE)

names(v.list[10])
v.list[[10]][1:10]

str_detect(v.list[[10]], "null") %>%
  which(. == TRUE) %>%
  length()

sum(na.count[unique(names(na.count))])

unique(names(na.count)) %>%
  length()
 
198599820 / sum(lengths(v.list))

sum(lengths(v.list)) - 198599820
 
na.count[1:3]
sum(na.count) /
sum(lengths(v.list))

f_NA <- function(x) {
  if (str_detect(x, "NA") == TRUE) { # Note: str_detect() requires the stringr package.
    NA
  } else {
    x
  }
}


f_NA2 <- function(x) {
  if (str_detect(x, "NA") == TRUE | str_detect(x, "null") == TRUE) { # Note: str_detect() requires the stringr package.
    NA
  } else {
    x
  }
}

is.na(test.v)
test.v <- lapply(v.list[[1]], f_NA) %>%
  unlist()



f_NULL <- function(x) {
  if (str_detect(x, "null") == TRUE) { # Note: str_detect() requires the stringr package.
    NA
  } else {
   x
  }
}







f_NULL_count <- function(x) {
  which(str_detect(x, "null") == TRUE ) %>%
    length()
}

apply(na.test.df, 2, f_NULL_count) %>%
  sum()

result.df <- lapply(na.test.df, f_NULL)

short.test <- lapply(v.list[[3]], f_NA)

which(str_detect(na.test.df, "null") == TRUE)

na_if(v.list[[1]], str_detect(v.list[[1]], "NA") == TRUE)

na.v <- str_detect(v.list[[1]], "NA") %>%
  which(. == TRUE)
na_if(v.list[[1]], na.v)

s <- 1:10
s[5] <- NA
is.na(s)
na_if(1:5, 5:1)

###############################
############## play with long form tibble

token_id <- seq_len(nrow(z)) %>%
  paste0("token-", .)

z_id <- cbind(token_id, z)
z.tib <- as_tibble(z_id)
z.tib <- z.tib[, c(1, 8:62)]


z.long.tib <- gather(z.tib, variable_name, variable_value, -token_id)


filter(z.long.tib, token_id == "token-10")

filter(z.long.tib, variable_name == "DepDist")


zzz <- x2[[j]]
v.list <- vector("list", ncol(zzz))
nomina.v <- NULL


for (i in seq_len(ncol(zzz))) {
  sss <- apply(z, 1, function(x) paste0(x[zzz[, i]], collapse = "-")) %>%
    as.character()
  
  nomina.v  <- paste0(zzz[, i], collapse = "*") %>%
    append(nomina.v, .)
  
  v.list[[i]] <- sss
  names(v.list) <- nomina.v
  
}



temp.l<- list()
temp.l[[1]] <- do.call(cbind, v.list)

write.csv(temp.l[[1]], file = paste0("new_variables", "-", j, ".csv"))


###################################
############################## play with dropping na values



token_id <- seq_along(v.list[[10]]) %>%
  paste0("token-", .)

temp.v <- v.list[[10]] %>%
  unlist()


temp.df <- cbind(token_id, temp.v) %>%
  as.data.frame() %>%
  drop_na()

null.v <- lapply(v.list[[10]], f_NULL) %>%
  unlist()

na.v <- lapply(v.list[[10]], f_NA) %>%
  unlist()

colnames(short.temp.df)[2] <-  names(v.list[10]) 
 


index.both.v <- union(which(is.na(na.v)), which(is.na(null.v)))

short.temp.df <- temp.df[-index.both.v,]


which(is.na(null.v)) %>%
  length()

null_na.v <- l
out[[4]]

####################more play 



length(na.test.df[[1]])
seq_along(na.test.df)
########################

nomina.v <- NULL
for (i in seq_len(ncol(zzz))) {
  nomina.v  <- paste0(zzz[, i], collapse = "*") %>%
    append(nomina.v, .)
  
}

length(nomina.v)
nomina.v

names(v.list) <- nomina.v

v.list[[1]] <- as.character(v.list[[1]])
v.list[[1]][1:10]

zzz3 <- do.call(cbind, v.list)
dim(zzz3)
View(zzz3[1:10, 1:10])

########################


names(v.list[[1]])

seq_len(ncol(zzz))
ncol(zzz)
sss <- NULL
names(v.list) <- letters

sss <- apply(z, 1, function(x) paste0(x[zzz[, 1]], collapse = "-"))


vv <- NULL
system.time(
for (i in seq_len(nrow(z))) {
  vv <-  append(vv, paste0(z[i, zzz[, j]], collapse = "-")) %>%
    unlist()
}
)

f1 <- function(x) apply(z, 1, function(x) paste0(x[zzz[, 1]], collapse = "-")) 

sss <- do.call(f1, zzz.l)


seq_len(ncol(zzz))


fff <- apply(zzz, 2, `[`)

sss <- z[, apply(zzz, 2, `[`)]

sss[1:3, 1:3]

fff <- NULL

fff <- apply(z, 1, function(x) paste0(x[zzz[,1]], collapse = "-"))
fff[1]


vv <- NULL

for (i in seq_len(nrow(z))) {
 vv <-  append(vv, paste0(z[i, zzz[, 1]], collapse = "-")) %>%
   unlist()
}


vv[1:10]



length(which(bad_c == TRUE)) / length(bad_c)

zzz[, 1:10]
bad.names <- names(bad_c[which(bad_c == TRUE)])

names(bad_c[which(bad_c == FALSE)]) %>%
  unique()

zzz2 <- subset(zzz, select = which(bad_c == FALSE))

which(bad.names  %in% zzz)

zzz[, bad.names[1]]
v <-  c(bad.names)

for (i in seq_along(bad.names)) {
  x <- paste0("zzz", bad.names[i])
  zzz$x
  x
}

quote(x)
expr(x)
expression(x)

kkk <- map(word.list, `[`, y)

xxx <-  combn(y[c(11, 5, 16:25)], i)

comb.l <- map(y[c(11, 5, 16:25)], seq_along(y[c(11, 5, 16:25)]) )

tz <- as_tibble(z)
tz
rm(i)
zzz <- table(z$relation, z$Planarity)

zzz
row.names(zzz)

which(is.na(z) == TRUE)


#convert wide format table to matrix object
final.m <- apply(zzz, 2, as.numeric)

row.names(final.m) <- row.names(zzz)

final.df <- as.data.frame(final.m)

ratio <-  round(final.df$NonPlanar / (final.df$NonPlanar + final.df$Planar), digits = 4)
final.df  <-   cbind(final.df, ratio)

ratio

final.df   <- final.df[order(final.df$ratio, decreasing = TRUE),]

write.csv(final.df, file = "plnarityByRelation.csv")


rm(z)

############################################

colnames(y) <- "relation"

dword.list[[1]]

a <- lapply(word.list, names) #extract names of variable categories (these are the names of children of word elements.)


b <- unlist(a, recursive = TRUE, use.names = TRUE) # change a from list to character vector
var_types <-  sort(unique(b)) # alphabetize b


# Remove extraneous names from vector b
var_types <- var_types[which(! var_types == ".attrs")]
var_types <- var_types[which(! var_types == "text")]





holder.list <- vector("list", length(var_types)) # make list of length equal to number of variable categories

names(holder.list) <- var_types # add name of variable type to each element in list




# function to extract values of each variable type; it checks whether type of variable exists for each word.

f1 <- function(x) { # var_types as input 
  
  if (x %in% names(word.list[[k]]))  {
   a <-   word.list[[k]][[x]]["text"]
  } else {
    a <-    "NA"
  }
  
}


# create objects to hold results
result <- NULL
citations <- NULL

ptm <- proc.time()

for (k in 1:length(word.list)) { # loop applies function f1 to each word in word.list
  
 result <- append(result, sapply(var_types, f1)) 
 
 
 citations <- append(citations, word.list[[k]]$.attrs["cite"]) # word and sentence number collected for each word in word.list
  
}

result <- tolower(result) # lower case

temp <- unlist(result) # change contents of result to named character vector

z <- matrix(temp, ncol=length(var_types),byrow = TRUE) # create matrix containing results; rows are words, columns are variable types.
colnames(z) <- var_types # names for columns
row.names(z) <- citations # names for rows

proc.time() - ptm

write.csv(z, file = "intermed_sev-2.csv")



#############################

dim(z)


sent_name <-  unique(z[, 16])


sent.freq.table <- vector("list", length(unique(z[, 16])))



n <- 1

for (n in 1:length(sent_name)) {
  row_index <- (which(z[,16] == sent_name[n]))
  sent.vars <- z[row_index,1:15]
  
  sent.freq.table[[n]] <- table(sent.vars)/ length(row_index)
 
  
}



freqs.l <- mapply(data.frame, ID=seq_along(sent.freq.table),
                  sent.freq.table, SIMPLIFY=FALSE, MoreArgs=list(stringsAsFactors=FALSE))


freqs.df <- do.call(rbind, freqs.l)


#convert from long form table to wide format
tab.result <- xtabs(Freq ~ ID+sent.vars, data=freqs.df)


#convert wide format table to matrix object
final.m <- apply(tab.result, 2, as.numeric)
row.names(final.m) <- unique(z[, 16])

write.csv(final.m, file = "output_wint_no_ellips.csv")

output.m <- read.csv(file = "output_wint_no_ellips.csv",check.names = FALSE, row.names = 1, stringsAsFactors = FALSE)

combined.m <- read.csv(file = "combined_sent_data.csv",check.names = FALSE, stringsAsFactors = FALSE)

plot(log(combined.m[,2]), log(combined.m[,3]))


cor.test(log(combined.m[,2]), log(combined.m[,3]))

dim(output.m)

plot(output.m[,1], output.m[,2])


plot(output.m[,4], output.m[,5])


output.m[,1]

cor.test(log(output.m[,1]), log(output.m[,2]))


cor.test(log(output.m[,1]), log(output.m[,3]))

cor.test(output.m[,4], output.m[,5])
cor.test(output.m[,4], log2(output.m[,5]))
plot(output.m[,4], log2(output.m[,5]))

dim(final.m)

10^-12
log(.1)
2.718 ^ -2.30

z.corr <- cor(log(output.m[,1]), output.m[,4:1962] )
write.csv(z.corr, file = "wint_cor_matrix_no_ellips.csv")

which(z[, "Projectivity"] == "NonProjective") %>%
  length()
