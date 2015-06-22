# make a 'signature x petition' matrix

# run this command in the terminal first, with your account!:
# ssh -L 8888:projpet.rit.albany.edu:3306 youraccount@projpet.rit.albany.edu -N


# put your username and password here:
user = "username"
password = "password"

# the list of petitions
petitions = read.csv("MBS_Episode_1.txt", stringsAsFactors = F)



# querying the database
library(RMySQL)

con = dbConnect(MySQL(), dbname = "wtp", user = user,
    password = password, host = "127.0.0.1", port = 8888)

query = paste0("select * from signature where petition_id in ('",
    paste(petitions[, 1], collapse = "', '"), "')")
signatures = dbGetQuery(con, query)

dbDisconnect(con)



# cleaning the data

# remove signatures with no zip code
signatures = signatures[signatures$zip != "", ]

# combine the name and zip code
signatures$signer = paste(signatures$name, signatures$zip, sep = ", ")

# get the total number of petitions signed by each unique signer
totals = table(signatures$signer)

# Some of these numbers are totally bogus, with supposedly 150
# petitions signed. I'm removing those.
real_totals = totals[totals < 100]

# the non-bogus signatures
signatures = signatures[signatures$signer %in% names(real_totals), ]




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
