# Couples testing/treatment cleaning functions

phia_ind_clean <- function(x, y = NULL, survey = NULL) {
  phia <- haven::read_dta(x)
  if(!is.null(y)) {
    phia_roster <- haven::read_dta(y)
    phia_roster <- phia_roster %>%
      select(personid, partnerclusterid,
             husid, partid1, partid2, partid3)
    phia <- phia %>%
      left_join(phia_roster)
  }
  lookup <- c(hivtestm = "hivtslstm", hivtesty = "hivtslsty")
  phia <- phia %>%
    rename(any_of(lookup))
  # removing ethnicgrp since not in all surveys (available in MPHIA 2015-16)
  phia <- phia %>%
    select(gender, age, personid, partnerclusterid, 
           # schooling variable weird for Mozambique
           # schlhi,
           # religion not in CIPHIA 2017
           #religion, 
           hivtestm, hivtesty, hivtstever, surveystmonth, surveystyear,
           husid, partid1, partid2, partid3, 
           #hivrtpg, hivrslr,hivtstpostbirth, 
           #hivtprg, 
           #hivttlb, 
           #hivtsprgm, hivtsprgy,
           schlat, #parthivtest1,
           hivtstrslt) %>%
    mutate(hivtslstm_clean = ifelse(hivtestm %in% c(-8,-9), NA, hivtestm),
           hivtslsty_clean = ifelse(hivtesty %in% c(-8,-9), NA, hivtesty),
           hiv_test_date = case_when(!is.na(hivtslstm_clean) &
                                       !is.na(hivtslsty_clean) &
                                       nchar(hivtslstm_clean)==1 ~
                                       paste0(hivtslsty_clean,
                                              "-0",hivtslstm_clean,"-15"),
                                     !is.na(hivtslstm_clean) &
                                       !is.na(hivtslsty_clean) &
                                       nchar(hivtslstm_clean)==2 ~
                                       paste0(hivtslsty_clean,
                                              "-",hivtslstm_clean,"-15"),
                                     is.na(hivtslstm_clean) &
                                       !is.na(hivtslsty_clean) ~
                                       paste0(hivtslsty_clean,
                                              "-07-01"),
                                     TRUE ~ NA_character_),
           # hivtslstm_prg_clean = ifelse(hivtsprgm %in% c(-8,-9), NA, hivtsprgm),
           # hivtslsty_prg_clean = ifelse(hivtsprgy %in% c(-8,-9), NA, hivtsprgy),
           # hiv_test_date_prg = case_when(!is.na(hivtslstm_prg_clean) &
           #                                 !is.na(hivtslsty_prg_clean) &
           #                                 nchar(hivtslstm_prg_clean)==1 ~
           #                                 paste0(hivtslsty_prg_clean,
           #                                        "-0",hivtslstm_prg_clean,"-15"),
           #                               !is.na(hivtslstm_prg_clean) &
           #                                 !is.na(hivtslsty_prg_clean) &
           #                                 nchar(hivtslstm_prg_clean)==2 ~
           #                                 paste0(hivtslsty_prg_clean,
           #                                        "-",hivtslstm_prg_clean,"-15"),
           #                               is.na(hivtslstm_prg_clean) &
           #                                 !is.na(hivtslsty_prg_clean) ~
           #                                 paste0(hivtslsty_prg_clean,
           #                                        "-07-01"),
           #                               TRUE ~ NA_character_),
           hivtstever = factor(ifelse(hivtstever %in% c(-8,-9),NA,hivtstever),
                               levels = c(1,2),
                               labels = c("Yes","No")),
           # ethnicgrp = factor(ifelse(ethnicgrp %in% c(-8,-9),NA,ethnicgrp),
           #                    levels = c(1,2,3,4,5,6,7,8,96),
           #                    labels = c("Nkhonde","Tumbuka","Tonga","Yao",
           #                               "Chewa","Sena","Lomwe","Ngoni","Other")),
           # education = case_when(schlat == 2 ~ 0,
           #                       !schlhi %in% c(-8,-9) ~ schlhi,
           #                       TRUE ~ NA_integer_),
           # education = factor(education,
           #                    levels = c(0,1,2,3),
           #                    labels = c("None","Primary","Secondary","Higher")),
           interview_date = case_when(!is.na(surveystyear) &
                                        !is.na(surveystmonth) &
                                        nchar(surveystmonth)==1 ~
                                        paste0(surveystyear,
                                               "-0",surveystmonth,"-15"),
                                      !is.na(surveystmonth) &
                                        !is.na(surveystyear) &
                                        nchar(surveystmonth)==2 ~
                                        paste0(surveystyear,
                                               "-",surveystmonth,"-15"),
                                      TRUE ~ NA_character_),
           test_past = (as.Date(interview_date) - as.Date(hiv_test_date))/365,
           # test_past_prg = (as.Date(interview_date) - as.Date(hiv_test_date_prg))/365,
           hiv_testing = case_when(hivtstever=="No" ~ "Never",
                                   # !is.na(hivrslr) & hivrslr==1 ~ "Positive",
                                   # !is.na(hivrtpg) & hivrtpg==1 ~ "Positive",
                                   !is.na(hivtstrslt) & hivtstrslt==1 ~ "Positive",
                                   !is.na(test_past) & test_past<1 ~ "Past yr",
                                   !is.na(test_past) & test_past>=1 & test_past<2 ~ "1-2 yrs",
                                   !is.na(test_past) & test_past>=2 ~ "2+ yrs",
                                   # is.na(test_past) & !is.na(test_past_prg) &
                                   #   test_past_prg < 1 ~ "Past yr",
                                   # is.na(test_past) & !is.na(test_past_prg) &
                                   #   test_past_prg >= 1 & test_past_prg < 2 ~ "1-2 yrs",
                                   # is.na(test_past) & !is.na(test_past_prg) &
                                   #   test_past_prg >=2 ~ "2+ yrs",
                                   TRUE ~ NA_character_),
           survey = survey)
  return(phia)
}

phia_bio_clean <- function(x) {
  phia_bio <- haven::read_dta(x)
  ###### ONLY KEEPING PEOPLE FOR WHOM HIV STATUS IS NOT MISSING
  phia_bio <- phia_bio %>%
    select(personid, hivstatusfinal, aware, arvstatus, vls, btwt0) %>%
    filter(hivstatusfinal !=99)
  return(phia_bio)
}

phia_join_ind_bio <- function(phia, phia_bio) {
  phia <- phia %>%
    right_join(phia_bio)
  
  phia <- phia %>%
    mutate(fullstatus = case_when(vls==1 ~ "LHIV, VS",
                                  arvstatus==1 ~ "LHIV, on ART, not VS",
                                  aware==1 ~ "LHIV, aware of status, not on ART",
                                  hivstatusfinal == 1 ~ "LHIV, unaware of status",
                                  hiv_testing=="Past yr" ~ "HIV-, tested past year",
                                  hiv_testing=="Never" ~ "HIV-, never tested",
                                  hiv_testing %in% c("1-2 yrs","2+ yrs") ~ "HIV-, tested 1+ yrs ago",
                                  TRUE ~ NA_character_))
  return(phia)
}

phia_make_couples <- function(phia) {
  phia_f <- filter(phia,gender==2)
  phia_m <- filter(phia,gender==1)
  
  phia_f <- phia_f %>%
    rename_with(~ paste0(.x, "_f", recycle0 = TRUE)) %>%
    filter(!is.na(partnerclusterid_f)) %>%
    mutate(husid_f = ifelse(husid_f %in% c(partid1_f,partid2_f,partid3_f),"",husid_f)) %>%
    pivot_longer(c(husid_f,partid1_f,partid2_f,partid3_f), names_to = "partner",
                 values_to = "partnerid") %>%
    filter(partnerid!="")
  phia_m <- phia_m %>%
    rename_with(~ paste0(.x, "_m", recycle0 = TRUE)) %>%
    filter(!is.na(partnerclusterid_m))
  
  phia_both <- phia_f %>%
    left_join(phia_m,
              by = c("partnerid"="personid_m"))
  return(phia_both)
}

mixing_plot_data <- function(phia_couples) {
  phia_both_neg <- phia_couples %>% filter(hivstatusfinal_f==2 & hivstatusfinal_m==2)
  neg_des <- svydesign(ids = phia_both_neg$personid_f, 
                       data = phia_both_neg, weights = phia_both_neg$new_wt)
  # table(phia_both_neg$fullstatus_f,phia_both_neg$fullstatus_m)
  svytable(~ fullstatus_f + fullstatus_m, design = neg_des)
  phia_mixing <- prop.table(svytable(~ fullstatus_f + fullstatus_m, design = neg_des)) / (prop.table(svytable(~ fullstatus_f, design = neg_des)) %*% t(prop.table(svytable( ~ fullstatus_m, design = neg_des))))
  # phia_mixing <- prop.table(table(phia_both_neg$fullstatus_f,phia_both_neg$fullstatus_m)) / (prop.table(table(phia_both_neg$fullstatus_f)) %*% t(prop.table(table(phia_both_neg$fullstatus_m))))
  phia_plotdat1 <- data.frame(phia_mixing) %>%
    rename(f_testing = fullstatus_f, m_testing = fullstatus_m, mixing = Freq) %>%
    mutate(mixing = ifelse(mixing==0,(1/10^1.5),mixing),
           cat = "Seroconcordant, Negative")
  
  phia_both_pos <- phia_couples %>% filter(hivstatusfinal_f==1 & hivstatusfinal_m==1)
  pos_des <- svydesign(ids = phia_both_pos$personid_f, 
                       data = phia_both_pos, weights = phia_both_pos$new_wt)
  # table(phia_both_pos$fullstatus_f,phia_both_pos$fullstatus_m)
  svytable(~ fullstatus_f + fullstatus_m, design = pos_des)
  phia_mixing <- prop.table(svytable(~ fullstatus_f + fullstatus_m, design = pos_des)) / (prop.table(svytable(~ fullstatus_f, design = pos_des)) %*% t(prop.table(svytable( ~ fullstatus_m, design = pos_des))))
  # phia_mixing <- prop.table(table(phia_both_pos$fullstatus_f,phia_both_pos$fullstatus_m)) / (prop.table(table(phia_both_pos$fullstatus_f)) %*% t(prop.table(table(phia_both_pos$fullstatus_m))))
  phia_plotdat2 <- data.frame(phia_mixing) %>%
    rename(f_testing = fullstatus_f, m_testing = fullstatus_m, mixing = Freq) %>%
    mutate(mixing = ifelse(mixing==0,(1/10^1.5),mixing),
           cat = "Seroconcordant, LHIV")
  
  phia_both_serdis_f <- phia_couples %>% filter(hivstatusfinal_f==1 & hivstatusfinal_m==2)
  serdis_f_des <- svydesign(ids = phia_both_serdis_f$personid_f, 
                       data = phia_both_serdis_f, weights = phia_both_serdis_f$new_wt)
  # table(phia_both_serdis_f$fullstatus_f,phia_both_serdis_f$fullstatus_m)
  svytable(~ fullstatus_f + fullstatus_m, design = serdis_f_des)
  phia_mixing <- prop.table(svytable(~ fullstatus_f + fullstatus_m, design = serdis_f_des)) / (prop.table(svytable(~ fullstatus_f, design = serdis_f_des)) %*% t(prop.table(svytable( ~ fullstatus_m, design = serdis_f_des))))
  # phia_mixing <- prop.table(table(phia_both_serdis_f$fullstatus_f,phia_both_serdis_f$fullstatus_m)) / (prop.table(table(phia_both_serdis_f$fullstatus_f)) %*% t(prop.table(table(phia_both_serdis_f$fullstatus_m))))
  phia_plotdat3 <- data.frame(phia_mixing) %>%
    rename(f_testing = fullstatus_f, m_testing = fullstatus_m, mixing = Freq) %>%
    mutate(mixing = ifelse(mixing==0,(1/10^1.5),mixing),
           cat = "Serodiscordant, FLHIV")
  
  phia_both_serdis_m <- phia_couples %>% filter(hivstatusfinal_f==2 & hivstatusfinal_m==1)
  serdis_m_des <- svydesign(ids = phia_both_serdis_m$personid_f, 
                            data = phia_both_serdis_m, weights = phia_both_serdis_m$new_wt)
  # table(phia_both_serdis_m$fullstatus_f,phia_both_serdis_m$fullstatus_m)
  svytable(~ fullstatus_f + fullstatus_m, design = serdis_m_des)
  phia_mixing <- prop.table(svytable(~ fullstatus_f + fullstatus_m, design = serdis_m_des)) / (prop.table(svytable(~ fullstatus_f, design = serdis_m_des)) %*% t(prop.table(svytable( ~ fullstatus_m, design = serdis_m_des))))
  # phia_mixing <- prop.table(table(phia_both_serdis_m$fullstatus_f,phia_both_serdis_m$fullstatus_m)) / (prop.table(table(phia_both_serdis_m$fullstatus_f)) %*% t(prop.table(table(phia_both_serdis_m$fullstatus_m))))
  phia_plotdat4 <- data.frame(phia_mixing) %>%
    rename(f_testing = fullstatus_f, m_testing = fullstatus_m, mixing = Freq) %>%
    mutate(mixing = ifelse(mixing==0,(1/10^1.5),mixing),
           cat = "Serodiscordant, MLHIV")
  
  phia_plotdat_all <- bind_rows(phia_plotdat1,
                                phia_plotdat2,
                                phia_plotdat3,
                                phia_plotdat4)
  
  phia_plotdat_all <- phia_plotdat_all %>%
    mutate(f_testing = factor(f_testing,
                              levels = c("HIV-, never tested", "HIV-, tested 1+ yrs ago",
                                         "HIV-, tested past year", "LHIV, unaware of status",
                                         "LHIV, aware of status, not on ART", "LHIV, on ART, not VS",
                                         "LHIV, VS")),
           m_testing = factor(m_testing,
                              levels = c("HIV-, never tested", "HIV-, tested 1+ yrs ago",
                                         "HIV-, tested past year", "LHIV, unaware of status",
                                         "LHIV, aware of status, not on ART", "LHIV, on ART, not VS",
                                         "LHIV, VS")))
  
  return(phia_plotdat_all)
}
