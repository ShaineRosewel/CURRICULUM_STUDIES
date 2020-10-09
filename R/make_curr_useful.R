make_curr_useful <-
  function(curr = "elem", this = "whole") {
    library(magrittr)
    
    # get relevant data
    if (curr=="elem"){
      processed_curr<-readxl::read_xlsx("input/K12_math.xlsx", 
                                        skip=3, n_max=1102, col_names = TRUE)
    } else if (curr=="jhs") {
      processed_curr<-readxl::read_xlsx("input/K12_math.xlsx", 
                                        skip=1107, col_names = TRUE)
    } else if (curr=="k") {
      processed_curr<-readxl::read_xlsx("input/K12K_math.xlsx", skip=7, 
                                        n_max=70)
    } else {
      print("wala sa choices yan - k, elem, jhs lang")
    }
    
    # removes columns with all NA
    processed_curr_wo_na <- processed_curr[ , colSums(is.na(processed_curr)) != 
                                              nrow(processed_curr)]
    
    # index and names of all distorted columns
    weird_index <- grep(pattern="\\.\\.\\.[0-9]+", 
                        x = colnames(processed_curr_wo_na))
    weird_colname <- colnames(processed_curr_wo_na)[weird_index]
    
    # which.max returns index ng may highest frequency, names gives back name!
    slight_fix <- apply(processed_curr_wo_na[, weird_index], 2, function(x){
      names(which.max(table(x)))})
    x<-names(table(slight_fix))
    
    # oo <- foreach::foreach(i=x, .combine=cbind) %do% {
    #   k <-append(
    #     processed_curr_wo_na[ , which(colnames(processed_curr_wo_na)==i)],
    #              as.list(processed_curr_wo_na[,names(which(slight_fix==i))]))
    #   k1=dplyr::coalesce(!!!k) #coalesce a list of vectors
    # }
    
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
    
    
    #colnames(oo) <- x
    a<- setdiff(colnames(processed_curr_wo_na)
                [-(grep(pattern = "...[0-9]+" , 
                        x = colnames(processed_curr_wo_na)))],
                colnames(oo))
    b<-which(colnames(processed_curr_wo_na)%in% a)
    fin<-cbind(processed_curr_wo_na[,b], oo)
    
    if (curr %in% c("elem", "jhs")) {
      col_order <- union(colnames(
        processed_curr_wo_na)[-(grep(pattern = "...[0-9]+" , x = 
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
    
    o=gsub("(NA)|(\r\n)|( +)", x=paste(fin3$CODE, collapse = "_"), 
           replacement = "")
    l=paste(fin3$`LEARNING COMPETENCY`, collapse = "_")
    
    if(curr=="elem"){
      c_1 = stringr::str_extract_all(
        o, "[A-Z][0-9]+[A-Z]+-[I,V]+[a-z]-[0-9]+(\\.[0-9]+)?")[[1]]
    } else if (curr=="jhs") {
      c_1 = stringr::str_extract_all(
        o, "[A-Z][0-9]+[A-Z]+-[I,V]+[a-z]-([0-9]|[a-z])")[[1]]
    } else if (curr=="k") {
      c_1 = stringr::str_extract_all(
        o, "MK[A-Z]+(-|_)00-[0-9]+")[[1]]
    } else {
      print('mali supplied mo')
    }
    
    o1<-gsub("(NA)|(\r\n)", x=l, replacement = "")
    o2<-gsub("(e\\.g\\.)|([1-9]+[0-9]*\\.[0-9]+)", x=o1, replacement = "-") 
    o3<-gsub("10\\. +10\\.", x=o2, replacement = "10\\.")
    o4<-regmatches(o3,gregexpr(
      text = o3, pattern = "[1-9]+[0-9]*\\.( |[a-zA-Z]).+?(_|\\.) *[a-z]*"))
    lc = sapply(o4, function(.){gsub(pattern = "_+$", x = ., replacement = "")})
    
    a1 <- cbind(c_1, lc)
    colnames(a1) <- c("CODE", "LEARNING COMPETENCY")
    
    
    if(curr %in% c("elem", "jhs")){
      a2 <- tibble::as.tibble(a1) %>% 
        transform(SUBJECT=substr(CODE, 1, 1),
                  GRADE = stringr::str_extract(CODE, pattern="[0-9]+"),
                  CONTENT = stringr::str_extract(CODE, pattern="([A-Z]+){2}"),
                  QUARTER = stringr::str_extract(CODE, 
                                                 pattern="([IV]+)|((0+){2})")#,
                  #what1 = stringr::str_extract(CODE, pattern="[a-z]+"),
                  #what2 = 
                  #stringr::str_extract(CODE, pattern="[0-9]+(\\.[0-9])?$")
      )
      a3=paste(a2$SUBJECT, a2$GRADE, a2$CONTENT, sep="") 
      a4=paste(a2$QUARTER, sep="") #, elem2$what1, sep="") 
      PSID=paste(a3, a4, sep="-")
      a5 <- cbind(a2, PSID)
  
      # cleaning performance and content standards =============================
      
      b<-fin3 %>% 
        dplyr::filter(!is.na(
          `PERFORMANCE STANDARDS`) | !is.na(`CONTENT STANDARDS`)) %>%
        dplyr::select(c(CODE, `CONTENT STANDARDS`, `PERFORMANCE STANDARDS`))%>% 
        tidyr::fill(CODE)
      
      b1<-paste(b$CODE, collapse="")
      b1<-gsub(b1, pattern=" ", replacement="")
      
      if(curr == "elem"){
        b$CODE <- stringr::str_extract_all(
          b1, pattern="[A-Z][0-9]+[A-Z]+-[I,V]+[a-z]-[0-9]+(\\.[0-9]+)?")[[1]]
        b <- b %>% 
          dplyr::mutate(PSID=stringr::str_extract_all(
            b1, pattern="[A-Z][0-9]+[A-Z]+-[I,V]+")[[1]])
      } else if (curr=="jhs") {
        b$CODE <- stringr::str_extract_all(
          b1, pattern="[A-Z][0-9]+[A-Z]+-[I,V]+[a-z]-([0-9]|[a-z])")[[1]]
        b <- b %>% 
          dplyr::mutate(PSID=stringr::str_extract_all(
            b1, pattern="[A-Z][0-9]+[A-Z]+-[I,V]+")[[1]])
      } else {
        print('mali supplied mo')
      }
      
      # CS ---------------------------------------------------------------------
      
      cs <- b %>% dplyr::group_by(PSID) %>% 
        dplyr::summarise(CS = gsub("(NA)|(\r\n)", 
                                   x = paste(`CONTENT STANDARDS`,
                                           collapse = " "),
                                   replacement = " ") %>%
                           strsplit(split = 
                                      "([1-9]+[0-9]*\\.)? *demonstrates") %>%
                           unlist()) %>%
        dplyr::ungroup() %>%
        dplyr::filter(CS!="")
      
      # PS ---------------------------------------------------------------------
      
      ps <- b %>% dplyr::group_by(PSID) %>% 
        dplyr::summarise(PS = gsub("(NA)|(\r\n)",
                                   x=paste(`PERFORMANCE STANDARDS`, 
                                           collapse = " "),
                                   replacement = " ") %>%
                           strsplit(split = 
                                      "([1-9]+[0-9]*\\.)? *is able to") %>%
                           unlist()) %>%
        dplyr::ungroup() %>%
        dplyr::filter(PS!="")
      
      # ------------------------------------------------------------------------
      
      cs_1 <- cs %>% dplyr::group_by(PSID) %>% 
        dplyr::summarise(CS=paste(CS, collapse="_"))
      ps_1 <- ps %>% dplyr::group_by(PSID) %>% 
        dplyr::summarise(PS=paste(PS, collapse="_"))
      
      a6 <- dplyr::left_join(a5, cs_1, "PSID")
      a6$CS[duplicated(a6$CS)] <- NA
      
      a7 <- dplyr::left_join(a6, ps_1, "PSID")
      a7$PS[duplicated(a7$PS)] <- NA
      a7$CONTENT <- factor(a7$CONTENT)
      a7$QUARTER <- factor(a7$QUARTER)
      levels(a7$QUARTER) <- c("1", "2", "3", "4")
      
    } else if (curr=="k") {
      a2 <- tibble::as.tibble(a1) %>% 
        transform(SUBJECT=substr(CODE, 1, 1),
                  GRADE = stringr::str_extract(CODE, pattern="K"),
                  CONTENT = c(rep("Logic (L)", 13), 
                              rep("Number and Number Sense (NNS)", 22), 
                              rep("Measurement (ME)", 10), 
                              rep("Geometry (G)", 5), 
                              rep("Statistics and Probability (SP)", 4)),
                  #unknown = stringr::str_extract(
                  #CODE, pattern="(?<=MK).*?(?=-|_)"),
                  QUARTER = stringr::str_extract(CODE, pattern="00")#,
                  #what1 = stringr::str_extract(CODE, pattern="[0-9]+$")
      )
      
      a7 <- dplyr::left_join(a2, fin3[, 1:3], "CONTENT")
      colnames(a7)[7:8] <- c("CS", "PS")
      a7$PS[duplicated(a7$PS)] <- NA
      a7$CS[duplicated(a7$CS)] <- NA
      
      a7$CONTENT <- factor(a7$CONTENT)
      levels(a7$CONTENT)<-c("GE", "LG", "ME", "NS", "SP")
      a7$GRADE <- 0
      a7$QUARTER <- factor(0)
      #a7$CONTENT <- as.character(a7$CONTENT)
    }
    
    a8 <- a7 %>% tidyr::separate(col = LEARNING.COMPETENCY, 
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
    
    # output====================================================================
    
    if (this=="whole"){
      return(fin3)
    } else if (this=="prt") {
      return(a8)
    } else if (this=="spread") {
      return(a9)
    } else {
      print("walang ganyang supply choice")
    }
  }

# sample usage -----------------------------------------------------------------
K <- make_curr_useful("k", "spread")
E <- make_curr_useful("elem", "spread")
J <- make_curr_useful("jhs", "spread")


full <- rbind(K, E, J)