# balanced_latin.R
# author: Jessica Tin
# updated: Jul 5, 2018
#
# Defines a function, balanced_latin(n), which generates an n-by-n Latin square
# whose elements are balanced such that the number of times each digit precedes
# every other digit is equal (or one-off from equal, for odd values of n) to the
# number of times that digit follows every digit, rowwise.
# e.g. In a 4x4 Latin square, the number of times 1 precedes 2 across all rows =
#       the number of times 2 precedes 1 across all rows.
#      In a 7x7 Latin square, the number of times 1 precedes 2 across all rows =
#       the number of times 2 precedes 1 across all rows +/- 1.
#      etc.


#### LOAD PACKAGES ####
if(!require("magic")) install.packages("magic", dependencies = TRUE)
if(!require("dplyr")) install.packages("dplyr", dependencies = TRUE)
library("magic") # for latin square functions
library("dplyr") # for data frame manipulation

#### DEFINE FUNCTION ####
## Function:  balanced_latin(n)
## Arguments: n, an integer; the dimension of the desired Latin square
## Returns:   an n-by-n integer array; a Latin square, balanced as described above
balanced_latin <- function(n) {
    l <- latin(n)
    #print(l)
    
    # create a data frame to keep track of balance
    balance <- filter(as.data.frame(t(rbind(rep(1:n, each = n),
                                            rep(1:n, times = n)))),
                      V1 != V2)
    balance$count <- 0
    
    for(r in 1:n) { # for each row of the Latin square
        r_string <- paste(l[r,], collapse = "") # represent the row as a string
        # e.g. "1234"
        for(x in 1:(n-1)) { # starting characters, e.g. for "1234", 1,2,3
            for(y in (x+1):n) { # ending characters, e.g. for "1234", 2,3,4
                # for each pair of numbers, going across each row, add 1 to the
                # corresponding count column in the data frame
                # e.g. for the row "1234", add 1 to data frame count columns for
                #  1,2; 1,3; 1,4; 2,3; 2,4; and 3,4
                balance$count[balance$V1 == substr(r_string, x, x) &
                                  balance$V2 == substr(r_string, y, y)] <- balance$count[balance$V1 == substr(r_string, x, x) &
                                                                                             balance$V2 == substr(r_string, y, y)] + 1
            }
        }
    }
    #print(cbind(arrange(balance, V1, V2), arrange(balance, V2, V1)))
    balanced <- ifelse(n %% 2 > 0,
                       # if n is an odd number, check that the corresponding counts
                       # (e.g. counts of 1,2 and counts of 2,1) are within +/- 1
                       # of each other
                       all(abs(arrange(balance, V1, V2)$count - arrange(balance, V2, V1)$count) <= 1),
                       
                       # else, if n is even, check that the corresponding counts
                       # (e.g. counts of 1,2 and counts of 2,1) are equal to each
                       # other
                       all(arrange(balance, V1, V2)$count == arrange(balance, V2, V1)$count))
    
    # if the first latin square wasn't balanced, keep generating new latin squares
    # until balanced
    while(!balanced) {
        l <- another_latin(l)
        #print(l)
        balance$count <- 0
        
        for(r in 1:n) { # for each row of the Latin square
            r_string <- paste(l[r,], collapse = "") # represent the row as a string
            # e.g. "1234"
            for(x in 1:(n-1)) { # starting characters, e.g. for "1234", 1,2,3
                for(y in (x+1):n) { # ending characters, e.g. for "1234", 2,3,4
                    # for each pair of numbers, going across each row, add 1 to the
                    # corresponding count column in the data frame
                    # e.g. for the row "1234", add 1 to data frame count columns for
                    #  1,2; 1,3; 1,4; 2,3; 2,4; and 3,4
                    balance$count[balance$V1 == substr(r_string, x, x) &
                                      balance$V2 == substr(r_string, y, y)] <- balance$count[balance$V1 == substr(r_string, x, x) &
                                                                                                 balance$V2 == substr(r_string, y, y)] + 1
                }
            }
        }
        #print(cbind(arrange(balance, V1, V2), arrange(balance, V2, V1)))
        balanced <- ifelse(n %% 2 > 0,
                           # if n is an odd number, check that the corresponding counts
                           # (e.g. counts of 1,2 and counts of 2,1) are within +/- 1
                           # of each other
                           all(abs(arrange(balance, V1, V2)$count - arrange(balance, V2, V1)$count) <= 1),
                           
                           # else, if n is even, check that the corresponding counts
                           # (e.g. counts of 1,2 and counts of 2,1) are equal to each
                           # other
                           all(arrange(balance, V1, V2)$count == arrange(balance, V2, V1)$count))
    }
    return(l)
}
