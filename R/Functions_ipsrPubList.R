library(htmltools)
library(rentrez)
library(reticulate)
library(jsonlite)

################################################################################
# Generate a formatted list of the update on the publication list
pubDiff <- function(x, y){
    if(as.Date(y$ExportDate[1]) > as.Date(x$ExportDate[1])){
        n <- y
        o <- x
    } else {
        n <- x
        o <- y
    }
    return(n[!n$doi %in% o$doi, ])
}

checkBcunknumber <- function(new, bn){
    new_title <- sapply(new$title, function(x){
        return(substr(tolower(gsub("[^a-zA-Z]", "", x)), 1, 50))
    })
    pub_title <- sapply(bn$Title, function(x){
        return(substr(tolower(gsub("[^a-zA-Z]", "", x)), 1, 50))
    })
    hit <- match(new_title, pub_title)
    return(new[is.na(hit), ])
}

makePubList <- function(x, out_fn){
    if(nrow(x) == 0){
        publist <- "No publication"
    } else {
        lowers <- c("in", "of", "for", "and", "on", "or")

        x <- tapply(seq_len(nrow(x)), x$doi, function(i){
            if(length(i) == 1){
                return(x[i, ])
            }
            check <- grepl("Early Access", x$doctype[i])
            if(any(check)){
                return(x[i, ][check, ])
            } else {
                return(x[i, ])
            }
        })
        x <- do.call("rbind", x)

        publist <- apply(x, 1, function(y){
            ti <- tolower(strsplit(y["title"], "\ ")[[1]])
            ti <- sapply(ti, function(z){
                if(z %in% lowers){
                    return(z)
                } else {
                    return(stringr::str_to_title(z))
                }
            })
            ti <- paste(ti, collapse = " ")
            ti <- gsub(" +", " ", ti)
            tmp <- data.frame(journal = y["journal"],
                              issue = y["issue"],
                              vol = y["volume"],
                              page = y["pages"],
                              year = paste0("(", y["year"], ")"),
                              article_no = y["article_no"])
            if(y["doctype"] == "Early Access"){
                out <- paste(tmp$journal, "[Online first]", tmp$year, sep = " ")
            } else {
                tmp[tmp == ""] <- NA
                tmp <- subset(tmp, select = !is.na(tmp))
                if(is.null(tmp$page) & !is.null(tmp$article_no)){
                    artno <- sub("ARTN ?| ?\\[pii\\]| ?\\[doi\\]", "", tmp$article_no)
                    if(!is.null(tmp$issue)){
                        tmp$issue <- paste0("(", tmp$issue, ")")
                    }
                    out <- paste0(tmp$journal,
                                  ", ",
                                  paste(c(paste0(tmp$vol, tmp$issue), artno),
                                        collapse = ":"),
                                  " ", tmp$year)

                } else {
                    if(!is.null(tmp$issue)){
                        tmp$issue <- paste0("(", tmp$issue, ")")
                    }
                    out <- paste0(tmp$journal,
                                  ", ",
                                  paste(c(paste0(tmp$vol, tmp$issue), tmp$page), collapse = ":"),
                                  " ", tmp$year)
                }
            }
            if(!grepl("\\.$", ti)){
                ti <- paste0(ti, ".")
            }
            out <- c(y["authors"], ti,
                     out, paste0("Doi.org/", y["doi"]), "")
            return(out)
        })
    }

    write.table(as.vector(publist), out_fn,
                quote = FALSE, row.names = FALSE, col.names = FALSE)
}

makeHTMLpub <- function(x){
  if(nrow(x) == 0){
    publist <- "No publication"

  } else {

    x <- tapply(seq_len(nrow(x)), x$doi, function(i){
      if(length(i) == 1){
        return(x[i, ])
      }

      check <- grepl("Early Access", x$doctype[i])
      if(any(check)){
        return(x[i, ][!check, ])

      } else {
        return(x[i, ])
      }
    })

    out <- tagList(lapply(X = x, FUN = fmtHTMLentry))
  }

  return(out)
}

fmtHTMLentry <- function(y){
  lowers <- c("in", "of", "for", "and", "on", "or")
  # ti <- tolower(strsplit(y$title, "\ ")[[1]])
  # ti <- sapply(ti, function(z){
  #   if(z %in% lowers){
  #     return(z)
  #   } else {
  #     return(stringr::str_to_title(z))
  #   }
  # })
  # ti <- paste(ti, collapse = " ")
  ti <- y$title
  ti <- gsub(" +", " ", ti)
  tmp <- data.frame(journal = y$journal,
                    issue = y$issue,
                    vol = y$volume,
                    page = y$pages,
                    year = paste0("(", y$year, ")"),
                    article_no = y$article_no)
  if("Early Access" %in% y$doctype){
    out <- paste(tmp$journal, "[Online first]", tmp$year, sep = " ")

  } else {
    tmp[tmp == ""] <- NA
    tmp <- subset(tmp, select = !is.na(tmp))
    if(is.null(tmp$page) & !is.null(tmp$article_no)){
      artno <- sub("ARTN ?| ?\\[pii\\]| ?\\[doi\\]", "", tmp$article_no)
      if(!is.null(tmp$issue)){
        tmp$issue <- paste0("(", tmp$issue, ")")
      }
      out <- paste0(tmp$journal,
                    ", ",
                    paste(c(paste0(tmp$vol, tmp$issue), artno),
                          collapse = ":"),
                    " ", tmp$year)

    } else {
      if(!is.null(tmp$issue)){
        tmp$issue <- paste0("(", tmp$issue, ")")
      }
      out <- paste0(tmp$journal,
                    ", ",
                    paste(c(paste0(tmp$vol, tmp$issue), tmp$page), collapse = ":"),
                    " ", tmp$year)
    }
  }
  if(!grepl("\\.$", ti)){
    ti <- paste0(ti, ".")
  }
  doi <- paste0("Doi.org/", y$doi)

  out <- tags$div(tags$br(y["authors"]),
                  tags$br(ti),
                  tags$br(out),
                  tags$a(doi, href = paste0("http://", tolower(doi))),
                  tags$br(),
                  tags$br())
  return(out)
}

################################################################################
# Get publication list
getIPSRpublist <- function(env_fn = "getIPSRpublist.env", out_dir){
    env_df <- read.csv(env_fn, header = FALSE, row.names = 1)
    today <- Sys.Date()
    out_fn <- paste0(out_dir, "/", env_df["out_fn", 1], "_", today, ".csv")

    pubmed <- getIPSRpublistPUBMED()
    # wos <- getIPSRpublistWOS(env_fn = env_fn)
    # names(wos)[names(wos) == "source_title"] <- "journal"
    # names(wos)[names(wos) == "published_biblio_date"] <- "date"
    # names(wos)[names(wos) == "published_biblio_year"] <- "year"
    # tmp <- which(!pubmed$doi %in% wos$doi)
    # pubmed <- pubmed[tmp, ]
    pubmed$year <- sub(" .*", "", pubmed$pub_date)
    pubmed$date <- sub("[0-9]* ", "", pubmed$pub_date)
    pubmed$date <- sub("20[0-9][0-9]|19[0-9][0-9]", "", pubmed$date)
    pubmed <- subset(pubmed, select = -pub_date)
    # wos <- subset(wos, select = names(pubmed))
    # df <- rbind(cbind(wos, database = "WOS"), cbind(pubmed, database = "PubMed"))
    df <- cbind(pubmed, database = "PubMed")
    df <- cbind(df, ExportDate = today)
    df <- df[order(as.numeric(df$year), decreasing = TRUE), ]
    write.csv(df, out_fn, row.names = FALSE)
    return(out_fn)
}

################################################################################
# Get publication list from Pubmed
getIPSRpublistPUBMED <- function(){
    ent <- getEntre()
    med <- sepRecords(ent)
    mat <- lapply(med, med2mat)
    df <- mat2df(mat)
    return(df)
}

getEntre <- function(){
    ent <- entrez_search("pubmed",
                         '"Institute of Plant Science and Resources"[AFFL]')
    if(ent$count > ent$retmax){
        ent <- entrez_search("pubmed",
                             '"Institute of Plant Science and Resources"[AFFL]',
                             retmax = ent$count)
    }

    hits <- NULL
    job <- as.numeric(cut(1:ent$count, breaks = as.integer(ent$count/50, 1)))
    job[is.na(job)] <- max(job) + 1
    for(i in unique(job)){
        if(is.null(hits)){
            hits <- entrez_fetch(db = "pubmed",
                                 id = ent$ids[job == i],
                                 rettype = "medline")
        } else {
            hits <- paste(hits, entrez_fetch(db = "pubmed",
                                             id = ent$ids[job == i],
                                             rettype = "medline"), sep = "\n")
        }
    }
    hits <- strsplit(hits, "\n")[[1]]
    return(hits)
}

sepRecords <- function(x){
    pmid <- grep("^PMID", x)
    index <- as.numeric(cut(1:length(x), breaks = pmid,
                            include.lowest = FALSE, right = FALSE))
    index[is.na(index)] <- max(index, na.rm = TRUE) + 1
    x <- tapply(x, index, list)
}

med2mat <- function(x){
    one <- NULL
    for(i in 1:length(x)){
        tmp <- x[i]
        tmp <- gsub("\ +", " ", tmp)
        if(grepl("(^[A-Z]+\ *\\-\ *)", tmp)){
            one <- c(one, tmp)
        } else {
            one[length(one)] <- paste0(one[length(one)], tmp)
        }
    }
    mat <- strsplit(sub("(^[A-Z]+\ *\\-\ *)", "\\1#SPLIT", one), "#SPLIT")
    mat <- do.call("rbind", mat)
    mat[, 1] <- sub("\\-", "", gsub("\ ", "", mat[, 1]))
    return(mat)
}

mat2df <- function(x){
    df <- lapply(x, function(y){
        ath <- y[y[, 1] %in% "FAU", 2]
        ath <- sapply(ath, function(name){
            name <- strsplit(name, ", ")[[1]]
            last <- strsplit(name[2], " ")[[1]]
            last <- paste(paste0(substr(last, 1, 1), "."), collapse = "")
            return(paste(name[1], last, sep = ", "))
        })
        ath <- paste(ath, collapse = ", ")
        title <- y[y[, 1] %in% "TI", 2]
        journal <- y[y[, 1] %in% "JT", 2]
        issue <- y[y[, 1] %in% "IP", 2]
        vol <- y[y[, 1] %in% "VI", 2]
        pages <- y[y[, 1] %in% "PG", 2]
        article_no <- y[y[, 1] %in% "LID", 2][1]
        pub_date <- y[y[, 1] %in% "DP", 2]
        doctype <- y[y[, 1] %in% "PT", 2][1]
        doi <- sub(" \\[doi\\]", "", y[grep("\\[doi\\]", y[, 2]), 2][1])
        if(length(issue) == 0){issue <- NA}
        if(length(vol) == 0){vol <- NA}
        if(length(pages) == 0){pages <- NA}
        if(length(pub_date) == 0){pub_date <- NA}
        if(length(doctype) == 0){doctype <- NA}
        if(length(doi) == 0){doi <- NA}
        return(data.frame(authors = ath, title = title, journal = journal,
                          issue = issue, volume = vol, pages = pages,
                          article_no = article_no,
                          pub_date = pub_date, doi = doi, doctype = doctype))
    })
    df <- do.call("rbind", df)
    return(df)
}

################################################################################
# Get publication list from Web of Science
getIPSRpublistWOS <- function(env_fn = "getIPSRpublist.env", limit = 50){
    env_df <- read.csv(env_fn, header = FALSE, row.names = 1)
    options(reticulate.conda_binary = env_df["conda_binary", 1])
    use_condaenv(env_df["conda_env", 1])
    source_python(env_df["source_python", 1])
    key <- as.character(read.table(env_df["key_fn", 1]))
    query <- env_df["query", 1]

    df <- NULL
    page <- 1
    while(TRUE){
        wos <- getWOS(key = key,
                      query = query,
                      limit = as.integer(limit),
                      page = as.integer(page))
        wos <- lapply(wos, .wosParser)
    }
    for(skip in seq(1, wos$query_result$records_found, 100)){
        wos <- getWOS(key, query, as.integer(100), as.integer(skip))
        df <- c(df, lapply(wos$data, function(x){
            title <- x$title$title
            doctype <- x$doctype$doctype
            doi <- x$other$identifier_doi
            artno <- x$other$identifier_article_no
            if(is.null(title)) {title <- NA}
            if(is.null(doctype)) {doctype <- NA}
            if(is.null(doi)) {doi <- NA}
            if(is.null(artno)) {artno <- NA}
            src <- x$source
            src <- sapply(src, function(y){
                if(is.null(y)){ y <- NA}
                return(y)
            })
            ath <- x$author$authors
            ath <- sapply(ath, function(name){
                name <- strsplit(name, ", ")[[1]]
                last <- strsplit(name[2], " ")[[1]]
                last <- paste(paste0(substr(last, 1, 1), "."), collapse = "")
                return(paste(name[1], last, sep = ", "))
            })
            ath <- paste(ath, collapse = ", ")
            return(data.frame(title = title,
                              doctype = doctype,
                              t(src),
                              authors = ath,
                              doi = doi,
                              article_no = artno))
        }))
    }
    df <- do.call("rbind", df)
    return(df)
}

.wosParser <- function(wos){
    wos <- as.character(wos)
    wos <- gsub("()", "\"\"", wos, fixed = TRUE)
    wos <- gsub("DynamicSchema(", "", wos, fixed = TRUE)
    wos <- gsub(",)", "", wos, fixed = TRUE)
    wos <- gsub("(", "", wos, fixed = TRUE)
    wos <- gsub(")", "", wos, fixed = TRUE)
    wos <- gsub(",,", ",", wos, fixed = TRUE)
    wos <- gsub("\\s\\s", "\\s", wos)
    wos <- sub("^\\{", "[{", wos)
    wos <- sub("\\}$", "}]", wos)
    wos <- gsub("'", '"', wos, fixed = TRUE)
    wos <- gsub("Decimal", "", wos, fixed = TRUE)
    wos <- gsub("{\"authors\": ", "{\"authors\": [", wos, fixed = TRUE)
    wos <- gsub("}}, \"links\"", "}]}, \"links\"", wos, fixed = TRUE)
    out <- fromJSON(txt = wos)
    return(out)
}

################################################################################
# Get publication list
getAUpublist <- function(au, dp){
    today <- Sys.Date()
    ent <- .getAUentre(au = au, dp = dp)
    if(is.null(ent)){
        return(NULL)
    }
    med <- sepAUrecords(x = ent)
    mat <- lapply(med, med2mat)
    df <- au_mat2df(x = mat, au = au)
    if(is.null(df)){
        return(NULL)
    }
    df$year <- sub(" .*", "", df$pub_date)
    df$date <- sub("[0-9]* ", "", df$pub_date)
    df$date <- sub("20[0-9][0-9]|19[0-9][0-9]", "", df$date)
    df <- subset(df, select = -pub_date)
    if(nrow(df) == 0){
        return(NULL)
    }
    df <- cbind(df, database = "PubMed")
    df <- cbind(df, ExportDate = today)
    df <- df[order(as.numeric(df$year), decreasing = TRUE), ]
    return(df)
}

.getAUentre <- function(au, dp){
    query <- paste0(paste0(au, "[FAU]"), " AND ", paste(dp, "[dp]"))
    ent <- entrez_search("pubmed", query)
    if(ent$count == 0){return(NULL)}
    if(ent$count > ent$retmax){
        ent <- entrez_search("pubmed", query, retmax = ent$count)
    }

    hits <- NULL
    if(ent$count <= 50){
        job <- rep(1, ent$count)
    } else {
        job <- as.numeric(cut(1:ent$count, breaks = ceiling(ent$count/50)))
    }

    job[is.na(job)] <- max(job) + 1
    for(i in unique(job)){
        if(is.null(hits)){
            hits <- entrez_fetch(db = "pubmed",
                                 id = ent$ids[job == i],
                                 rettype = "medline")
        } else {
            hits <- paste(hits, entrez_fetch(db = "pubmed",
                                             id = ent$ids[job == i],
                                             rettype = "medline"), sep = "\n")
        }
    }
    hits <- strsplit(hits, "\n")[[1]]
    return(hits)
}

sepAUrecords <- function(x){
    x <- x[x != ""]
    pmid <- grep("^PMID", x)
    if(length(pmid) == 1){
        x <- list(x)
    } else {
        index <- as.numeric(cut(1:length(x), breaks = pmid,
                                include.lowest = FALSE, right = FALSE))
        index[is.na(index)] <- max(index, na.rm = TRUE) + 1
        x <- tapply(x, index, list)
    }
    return(x)
}

med2mat <- function(x){
    one <- NULL
    for(i in 1:length(x)){
        tmp <- x[i]
        tmp <- gsub("\ +", " ", tmp)
        if(grepl("(^[A-Z]+\ *\\-\ *)", tmp)){
            one <- c(one, tmp)
        } else {
            one[length(one)] <- paste0(one[length(one)], tmp)
        }
    }
    mat <- strsplit(sub("(^[A-Z]+\ *\\-\ *)", "\\1#SPLIT", one), "#SPLIT")
    mat <- do.call("rbind", mat)
    mat[, 1] <- sub("\\-", "", gsub("\ ", "", mat[, 1]))
    return(mat)
}

au_mat2df <- function(x, au){
    df <- lapply(x, function(y){
        ath <- y[y[, 1] %in% "FAU", 2]
        ath <- sapply(ath, function(name){
            name <- strsplit(name, ", ")[[1]]
            last <- strsplit(name[2], " ")[[1]]
            last <- paste(paste0(substr(last, 1, 1), "."), collapse = "")
            return(paste(name[1], last, sep = ", "))
        })
        ath <- paste(ath, collapse = ", ")
        hit_ath_index <- match(tolower(au), tolower(y[, 2]))
        if(is.na(hit_ath_index)){
            return(NULL)
        }
        fau_i <- which(y[, 1] %in% "FAU")
        ath_rank <- which(fau_i == hit_ath_index)
        if(!ath_rank %in% c(1, length(fau_i))){
            return(NULL)
        }
        end_fau_i <- fau_i[ath_rank + 1] - 1
        if(is.na(end_fau_i)){
            end_fau_i <- nrow(y)
        }
        hit_ath <- y[hit_ath_index, 2]
        subset_y <- y[hit_ath_index:end_fau_i, ]
        add <- subset_y[subset_y[, 1] %in% "AD", 2]
        hit_add <- paste(add, collapse = "; ")
        title <- y[y[, 1] %in% "TI", 2]
        journal <- y[y[, 1] %in% "JT", 2]
        issue <- y[y[, 1] %in% "IP", 2]
        vol <- y[y[, 1] %in% "VI", 2]
        pages <- y[y[, 1] %in% "PG", 2]
        article_no <- y[y[, 1] %in% "LID", 2]
        article_no <- grep("\\[doi\\]", article_no, invert = TRUE, value = TRUE)
        if(length(article_no) == 0){
            article_no <- NA
        }
        pub_date <- y[y[, 1] %in% "DP", 2]
        doctype <- y[y[, 1] %in% "PT", 2][1]
        doi <- sub(" \\[doi\\]", "", y[grep("\\[doi\\]", y[, 2]), 2][1])
        if(length(hit_ath) == 0){hit_ath <- NA}
        if(length(hit_add) == 0){hit_add <- NA}
        if(length(ath) == 0){ath <- NA}
        if(length(title) == 0){title <- NA}
        if(length(journal) == 0){journal <- NA}
        if(length(issue) == 0){issue <- NA}
        if(length(vol) == 0){vol <- NA}
        if(length(pages) == 0){pages <- NA}
        if(length(pub_date) == 0){pub_date <- NA}
        if(length(doctype) == 0){doctype <- NA}
        if(length(doi) == 0){doi <- NA}
        return(data.frame(query = hit_ath,
                          address = hit_add,
                          authors = ath,
                          title = title, journal = journal,
                          issue = issue, volume = vol, pages = pages,
                          article_no = article_no,
                          pub_date = pub_date, doi = doi, doctype = doctype))
    })
    df <- do.call("rbind", df)
    return(df)
}
