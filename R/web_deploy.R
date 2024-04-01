source("R/Functions_ipsrPubList.R")
library(rmarkdown)

prev_fn <- read.table("latest_publist")

write.table(prev_fn, "previous_publist",
            row.names = FALSE, col.names = FALSE, quote = FALSE)

# Generate the latest publication list
new_fn <- getIPSRpublist(out_dir = "publist_archive")

# Save the latest publication list file name
write.table(new_fn, "latest_publist",
            row.names = FALSE, col.names = FALSE, quote = FALSE)

render_site()

