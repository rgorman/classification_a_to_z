
seq_along(test2)
length(test2[[1]])
test2[[1]]

test3.df <- as.data.frame(matrix(nrow=length(v.list[[1]])))
test3 <- lapply(v.list, function(x) lapply(x, f_NA2))

i <- 1
system.time(
for (i in seq_along(test3)) {
  test3.df <- cbind.data.frame(test3.df, unlist(test3[[i]]), stringsAsFactors = FALSE)
  
}
)

test3.df <- test3.df[, 2:ncol(test3.df)]


names(test3.df) <- nomina.v

token_id <- seq_along(test3[[1]]) %>%
  paste0("token-", .)

test3.df <- cbind.data.frame(token_id, test3.df, stringsAsFactors = FALSE)

system.time(
long.test3.df <- gather(test3.df, variable_name, variable_value, -(token_id)) %>%
  drop_na()
)

names(long.test3.df)

test.tab <- table(test3.df$`self-depdist*self-relation*self-morph-pos`)
test.tab <- table(test3.df[[2:20]])

seq_along(test3.df)
length(test3.df)

tab.list <- vector("list", length(test3.df))
for (i in seq_along(test3.df)) {
  tab.list[[i]] <- table(test3.df[[i]])
}

names(tab.list)[1] <-"Token_ID"
names(tab.list)[2:length(tab.list)] <- nomina.v
length(tab.list)



#converting an R list into a Data Matrix
tb  <- as.data.frame(tab.list[[2]], stingsAsFactors = FALSE)

tab.list2 <- lapply(tab.list, data.frame)

freqs.df <- do.call(rbind, tab.list2[2:length(tab.list2)])

object.size(freqs.df)

which(freqs.df$Freq >= 5) %>%
  length()

freqs.df2 <- freqs.df[which(freqs.df$Freq >= 5) ,]

choose(8, 1:8) %>%
  sum()
  
###################### Create all combinations of chosen variables

self_attrs <- c(17, 16, 18:39, 3:15, 9:10)
parent_attrs <- 27:39
g1parent_attrs <- 40:52
g2parent_attrs  <- 53:55
g3parent_attrs <- 56:58
g4parent_attrs <- 59:61

group_list <- list() # create a list object to hold vectors identifying attributes to extract

group1.v <- c(17, 16, 18, 28, 27, 31, 41, 40, 44, 54, 53, 55) # indices (from vector "y") for creation of sWord level variables.



group2.v <- c(17, 16, 18:39, 13:15, 9:10) # indices of morphology values, etc.  for first 2 generations (target word and parent)



group3.v <- c(28, 27, 29:39, 41, 40, 42:52) # indices for parent and grandparent values.



group_1.list <- list() # make empty list object to store result of loop
nomina.v <- NULL # make empty vector to store names


for (i in seq_along(group1.v)) { # iterate for each item in group1.v
  group_1.list[[i]] <- combn(y[group1.v], i) # make all possible combinations of variables
  nomina.v <- paste(length(group1.v), "Choose",  i, collapse = " ") %>% # create names for elements in list
    append(nomina.v, .)
}

names(group_1.list) <- nomina.v  # assign names to list elements


group_2.list <- list() # make empty list object to store result of loop
nomina.v <- NULL # make empty vector to store names




for (i in 1:5) { # iterate selected number of times
  group_2.list[[i]] <- combn(y[group2.v], i) # make all possible combinations of variables
  nomina.v <- paste(length(group2.v), "Choose",  i, collapse = " ") %>% # create names for elements in list
    append(nomina.v, .)
}

names(group_3.list) <- nomina.v  # assign names to list elements


group_3.list <- list() # make empty list object to store result of loop
nomina.v <- NULL # make empty vector to store names

for (i in 1:5) { # iterate selected number of times
  group_3.list[[i]] <- combn(y[group3.v], i) # make all possible combinations of variables
  nomina.v <- paste(length(group3.v), "Choose",  i, collapse = " ") %>% # create names for elements in list
    append(nomina.v, .)
}

names(group_3.list) <- nomina.v  # assign names to list elements






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
  nomina.v <- paste("24", "Choose",  i, collapse = " ") %>%
    append(nomina.v, .)
}

names(xxx) <- nomina.v  # assign names to list

#########################################
choose(12, 1:12) %>%
  sum()

#########################################
choose(26, 1:26) %>%
  sum()

choose(29, 1:15)
paste("24", "Choose",  1:5, collapse = " ")
