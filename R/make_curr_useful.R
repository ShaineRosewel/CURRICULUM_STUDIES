make_curr_useful <-
  function(curr = "elem") {
    library(magrittr)
    
    if (curr=="elem"){
      processed_curr<-readxl::read_xlsx("C:/Users/shain/Desktop/CURRICULUM_STUDIES/input/K12_math.xlsx", 
                                        skip=3, n_max=1102, col_names = TRUE)
    } else if (curr=="jhs") {
      processed_curr<-readxl::read_xlsx("C:/Users/shain/Desktop/CURRICULUM_STUDIES/input/K12_math.xlsx", 
                                        skip=1107, col_names = TRUE)
    } else if (curr=="k") {
      processed_curr<-readxl::read_xlsx("C:/Users/shain/Desktop/CURRICULUM_STUDIES/input/K12K_math.xlsx", skip=7, 
                                        n_max=70)
    }
    
    processed_curr_wo_na <- processed_curr[ , colSums(is.na(processed_curr)) != 
                                              nrow(processed_curr)]
  
    colnames(processed_curr_wo_na) <- gsub(pattern = "\r\n",
                                           replacement = " ",
                                           x = colnames(processed_curr_wo_na))

    weird_index <- grep(pattern="\\.\\.\\.[0-9]+", 
                        x = colnames(processed_curr_wo_na))
    weird_colname <- colnames(processed_curr_wo_na)[weird_index]
    
    slight_fix <-
      apply(processed_curr_wo_na[, weird_index], 2, function(x){
        #which.max returns index ng may highest frequency, names gives back the name!
        name <- names(which.max(table(x)))
        name <- gsub(pattern = "\r\n",
                     replacement = " ",
                     x = name)
        return(name)
      })
    
    x<-names(table(slight_fix))

    oo <- sapply(x, 
                 function(x) {
                   k <-append(
                     processed_curr_wo_na[ ,
                                           which(colnames(
                                             processed_curr_wo_na)==x)],
                     as.list(
                       processed_curr_wo_na[ ,names(which(slight_fix==x))]))
                   k1=dplyr::coalesce(!!!k)
                 })
    
    
    a<- setdiff(colnames(processed_curr_wo_na)
                [-(grep(pattern = "\\.\\.\\.[0-9]+" , 
                        x = colnames(processed_curr_wo_na)))],
                colnames(oo))
    b<-which(colnames(processed_curr_wo_na)%in% a)
    fin<-cbind(processed_curr_wo_na[,b], oo)
    
    if (curr %in% c("elem", "jhs")) {
      col_order <- union(colnames(
        processed_curr_wo_na)[-(grep(pattern = "\\.\\.\\.[0-9]+" , x = 
                                       colnames(processed_curr_wo_na)))], 
        colnames(oo))
      fin2 <- fin[, col_order]
    } else if (curr == "k") {
      colnames(fin)[c(1,2,4,6)] <- c("CONTENT STANDARDS", 
                                     "PERFORMANCE STANDARDS", 
                                     "LEARNING COMPETENCY", 
                                     "CONTENT")
      fin2 <- fin[, c(6, 1, 2, 4, 3, 5)]
    }
    
    fin3 <- dplyr::anti_join(fin2, fin2[which(duplicated(fin2)),])
    
    #===========================================================================
    
    c_1<-na.omit(gsub(pattern = "( *)|(\r\n)", x=fin3$CODE, replacement=""))
    
    omitted_na <- na.omit(fin3$`LEARNING COMPETENCY`)
    l =ifelse(grepl("^[0-9]", omitted_na), 
              paste0("starthere", omitted_na), omitted_na) 
    l = paste(l, collapse=" ") %>% gsub(pattern="\r\n", replacement=" ")
    
    lc <- strsplit(l, split = "starthere")[[1]][-1]
    
    a1 <- cbind(c_1, lc)
    colnames(a1) <- c("CODE", "LEARNING COMPETENCY")
    
    if(curr %in% c("elem", "jhs")){
      a2 <- tibble::as.tibble(a1) %>% 
        transform(SUBJECT=substr(CODE, 1, 1),
                  GRADE = stringr::str_extract(CODE, pattern="[0-9]+"),
                  CONTENT = stringr::str_extract(CODE, pattern="([A-Z]+){2}"),
                  QUARTER = stringr::str_extract(CODE, 
                                                 pattern="([IV]+)|((0+){2})")
      )
      
      a3=paste(a2$SUBJECT, a2$GRADE, a2$CONTENT, sep="") 
      a4=paste(a2$QUARTER, sep="") 
      PSID=paste(a3, a4, sep="-")
      a5 <- cbind(a2, PSID)
      
      b0<-fin3 %>% # prep for content, perf stand
        dplyr::filter(!is.na(
          `PERFORMANCE STANDARDS`) | !is.na(`CONTENT STANDARDS`)) %>%
        dplyr::select(c(CODE, `CONTENT STANDARDS`, `PERFORMANCE STANDARDS`))%>% 
        tidyr::fill(CODE)
      
      b1<-na.omit(gsub(pattern = "( *)|(\r\n)", x=b0$CODE, replacement=""))
      
      b <- b0 %>% 
        dplyr::mutate(PSID=stringr::str_extract_all(
          b1, pattern="[A-Z][0-9]+[A-Z]+-[I,V]+"))
      b$PSID <- unlist(b$PSID)
      
      csp<-  # content performance standard
        apply(b[, 2:3], 2, function(x){
          aggregate(x,
                    list(factor(b$PSID)),
                    paste, collapse=" ")
        })
      
      csp<-cbind(csp[[1]], csp[[2]][,2])
      colnames(csp) <- c("PSID", "CS", "PS")
      csp$PS <- gsub(pattern = "is able to", x = csp$PS, replacement = "")
      
      yy<-dplyr::left_join(a5, csp, by="PSID")
      yy$CS[duplicated(yy$CS)] <- NA
      yy$PS[duplicated(yy$PS)] <- NA
      
      yy[, c("CONTENT", "QUARTER")] <- lapply(yy[, c("CONTENT", "QUARTER")], 
                                              factor)
      levels(yy$QUARTER) <- c("1", "2", "3", "4")
      
    } else if (curr=="k") {
      a2 <- tibble::as.tibble(a1) %>% 
        transform(SUBJECT=substr(CODE, 1, 1),
                  GRADE = stringr::str_extract(CODE, pattern="K"),
                  CONTENT = c(rep("Logic (L)", 13), 
                              rep("Number and Number Sense (NNS)", 22), 
                              rep("Measurement (ME)", 10), 
                              rep("Geometry (G)", 5), 
                              rep("Statistics and Probability (SP)", 4)),
                  QUARTER = stringr::str_extract(CODE, pattern="00")
      )
      
      yy <- dplyr::left_join(a2, fin3[, 1:3], "CONTENT")
      colnames(yy)[7:8] <- c("CS", "PS")
      yy$PS[duplicated(yy$PS)] <- NA
      yy$CS[duplicated(yy$CS)] <- NA
      
      yy$CONTENT <- factor(yy$CONTENT)
      levels(yy$CONTENT)<-c("GE", "LG", "ME", "NS", "SP")
      yy$GRADE <- 0
      yy$QUARTER <- factor(0)
      #a7$CONTENT <- as.character(a7$CONTENT)
    }
    
    a8 <- yy %>% tidyr::separate(col = LEARNING.COMPETENCY, 
                                 into = c("NUMBER", "LC"), 
                                 sep = "\\.", 
                                 extra = "merge")
    
    a8$LC <- stringr::str_trim(a8$LC)
    
    a8$NUMBER <- formatC(as.numeric(a8$NUMBER), flag="0", width=3,format = "d")
    a8$GRADE <- formatC(as.numeric(a8$GRADE), flag="0", width=2, format = "d")
    a8$GRADE <- factor(a8$GRADE)
    a8$SUBJECT <- factor(a8$SUBJECT)
    
    if (curr=="k") {
      a8$NUMBER <- 1:54
      a8$NUMBER <- formatC(as.numeric(a8$NUMBER), 
                           flag="0", 
                           width=3,
                           format = "d")
    } 
    
    OURCODE = paste0(a8$SUBJECT, a8$GRADE, a8$CONTENT, a8$QUARTER, a8$NUMBER)
    a9 <- cbind(CODE=a8[,"CODE"], OURCODE, a8[, c("CS", "PS", "LC")])
    
    if(curr=="jhs"){
      a9$OURCODE[117:121] <- c(paste("M08SP", 4053:4057, sep=""))
    }
    return(a9)
    }

# sample usage -----------------------------------------------------------------
K <- make_curr_useful("k")
E <- make_curr_useful("elem")
J <- make_curr_useful("jhs")

full <- rbind(K, E, J)