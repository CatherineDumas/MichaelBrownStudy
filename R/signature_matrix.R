# make a 'signature x petition' matrix

# run this command in the terminal first, with your account!:
# ssh -L 8888:projpet.rit.albany.edu:3306 youraccount@projpet.rit.albany.edu -N


# put your username and password here:
user = "root"
password = "root"

# the list of petitions-- Michael Brown
petitions = as.vector(read.csv("MBS_Episode_1.txt", header = F,
    stringsAsFactors = F)[,1 ])
# Darren Wilson petitions
petitions2 = as.vector(read.csv("MBS_Episode_2.txt", header = F,
    stringsAsFactors = F)[,1 ])
# last petitions
petitions3 = "54e318e6ee140f5853000000"
petitions = c(petitions, petitions2, petitions3)


# querying the database
library(RMySQL)

con = dbConnect(MySQL(), dbname = "wtp", user = user,
    password = password)

query = paste0("select * from wtp_data_signatures where petition_id in ('",
    paste(petitions, collapse = "', '"), "')")
signatures = dbGetQuery(con, query)

dbDisconnect(con)

# double checking
pettab = table(signatures$petition_id)



# cleaning the data

# remove signatures with no zip code
signatures2 = signatures[grepl("^\\d{5}$", signatures$zip) &
                           grepl("^[a-zA-Z]{2}$", signatures$name), ]

# combine the name and zip code
signatures2$signer = paste(toupper(signatures2$name),
    signatures2$zip, sep = ", ")

# get the total number of petitions signed by each unique signer
totals = table(signatures2$signer)


# write individual petition files and remove duplicated signatures
setwd("../data") # move to the data folder
signatures2$pet_id = match(signatures2$petition_id, petitions)
# don't need this
## for (petition in petitions) {
##   signatures3 = signatures2[signatures2$petition_id == petition, ]
##   tab3 = table(signatures3$signer)
##   signatures4 =
##     signatures3[signatures3$signer %in% names(tab3)[tab3 == 1], ]
##   write.csv(signatures4[, c("pet_id", "signer")],
##             file = paste0(petition, ".csv"), row.names = F)
## }


# write file in market basket csv format
library(reshape2)
file_name = "combined.basket"
# convert from long to wide/matrix format
sig_mat = dcast(signatures2, signer ~ pet_id)
# get rid of the known duplicates-- check every row for an entry > 1
sig_mat2 = sig_mat[apply(sig_mat, 1,
    function(x) !any(as.integer(x[-1]) > 1)), ]
pet_ids = names(sig_mat)[-1]
# don't want to add to an existing file
if (file.exists(file_name)) file.remove(file_name)
for (n in 1:nrow(sig_mat2)) {
  # list of petitions where the matrix entry is not zero
  signed = paste(pet_ids[as.logical(sig_mat2[n, -1])],
      collapse = ",")
  txt = paste0(n, " ", signed, "\n")
  cat(txt, file = file_name, append = T)
}



# putting together the sparse matrix

# number of petitions
petitions = unique(signatures$petition_id)
npet = length(petitions)

# number of signers (as far as I can tell)
signers = unique(signatures$signer)
nsign = length(signers)

library(Matrix)

# make a sparse matrix, which won't waste computer space storing a
# zillion zeroes
m = sparseMatrix(
    # the row numbers of each space to fill in
    i = match(signatures$signer, signers),
    # the column numbers
    j = match(signatures$petition_id, petitions),
    # dimensions equal to the number of signers and number of
    # petitions
    dims = c(nsign, npet),
    # signers are the rows, petitions are the columns
    dimnames = list(signers, petitions))

# It would be a lot better to use Matrix Market format. But for now
# the old-fashioned wasteful format. The matrix wants to be boolean after
# conversion, but coerce it back to numeric by adding zero. Sneaky
# trick!
write.csv(as.matrix(m) + 0, "matrix.csv")



# g02.nx file

write_nx = function(sig_mat, n) {
  file_name = paste0("combined_g0", n, ".nx")
  # don't want to add to an existing file
  if (file.exists(file_name)) file.remove(file_name)

  # get rid of signers that can't possibly have two petitions in common,
  # make things so much faster
  ## sig_mat2 = sig_mat[rowSums(sig_mat[, -1]) >= 2, ]
  ## sig_mat3 = sig_mat[rowSums(sig_mat[, -1]) >= 3, ]
  ## sig_mat4 = sig_mat[rowSums(sig_mat[, -1]) >= 4, ]
  ## sig_mat5 = sig_mat[rowSums(sig_mat[, -1]) >= 5, ]
  ## sig_mat6 = sig_mat[rowSums(sig_mat[, -1]) >= 6, ]
  sig_mat_n = sig_mat[rowSums(sig_mat[, -1]) >= n, ]
  print(paste0(nrow(sig_mat_n, " rows")))

  x = 1
  for (n1 in 1:nrow(sig_mat_n)) {
    # looking at all the rows after the current row, so I don't
    # double-count the edges
    for (n2 in (1:nrow(sig_mat_n))[1:nrow(sig_mat_n) > x]) {
      # do the signers have at least two petitions in common?
      if (sum(sig_mat_n[n1, -1] > 0 & sig_mat_n[n2, -1] > 0) >= n) {
        txt =
          paste0(sig_mat_n$signer[n1], " ",
                 sig_mat_n$signer[n2], "\n")
        cat(txt, file = file_name, append = T)
      }
    }
    x = x + 1
    if (x %% 100 == 0) print(x)
  }
}
