parse_awards <- function(x) {
  input <- read_award(x$files)
  
  # We know that the elements we want are:
  #org
  orgs <-  input$Award$Organization %>% 
    unlist %>% 
    as.data.frame(row.name = names(input$Award$Organization)) %>% 
    t %>% 
    set_colnames(c("orgId:ID(organization)", "Directorate", "Division"))
  
  # Program Element
  p_elem <- input$Award$ProgramElement %>% 
    unlist %>% 
    as.data.frame(row.name = names(input$Award$ProgramElement)) %>% 
    t %>% 
    set_colnames(c("progId:ID(program)", "Text"))
  
  # Program Reference
  p_ref <- input$Award$ProgramReference %>% 
    unlist %>% 
    as.data.frame(row.name = names(input$Award$ProgramReference)) %>% 
    t %>% 
    set_colnames(c("progId:ID(program)", "Text"))
  
  #award
  
  na_er <- function(x) {
    if(length(x) == 0) {return(NA)}
    return(x)
  }
  
  award <- data_frame(AwardID       = na_er(input$Award$AwardID),
                AwardTitle          = na_er(input$Award$AwardTitle),
                AwardAmount         = na_er(input$Award$AwardAmount),
                AwardEffectiveDate  = na_er(input$Award$AwardEffectiveDate),
                AwardExpirationDate = na_er(input$Award$AwardExpirationDate),
                AwardInstrument     = na_er(input$Award$AwardInstrument),
                AbstractNarration   = na_er(input$Award$AbstractNarration),
                MinAmdLetterDate    = na_er(input$Award$MinAmdLetterDate),
                MaxAmdLetterDate    = na_er(input$Award$MaxAmdLetterDate),
                ARRAAmount          = na_er(input$Award$ARRAAmount)) %>% 
    as.data.frame
  

  #person
  person <- input$Award$Investigator %>% 
    unlist %>%  
    as.data.frame(row.name = names(input$Award$Investigator)) %>% 
    t %>% 

  #institution
  
  institution <- input$Award$Institution %>% 
    unlist %>% 
    as.data.frame(row.name = names(input$Award$Institution)) %>% 
    t
  
  # We'll just return the different objects as part of a list.
  # The organization will happen elsewhere:
  return(list(orgs,
              p_elem,
              p_ref,
              award,
              person,
              institution))
}

build_csvs <- function(x) {
  
}