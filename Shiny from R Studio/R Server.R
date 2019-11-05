library(shiny)
library(rhandsontable)
library(ggplot2)
library(plotly)
library(DT)
library(shinyjs)

shinyServer(function
            (input, output, session){
              
              values <- reactiveValues()
              values$df <- data.frame(Age = numeric(0),
                                      Line = character(), 
                                      Sex = character(),
                                      Temp = numeric(0), 
                                      MEd = numeric(0),
                                      BW_actual = numeric(0),
                                      Intake_actual = numeric(0),
                                      LCTemp = numeric(0),
                                      Wm_BW = numeric(0),
                                      B_BW = numeric(0),
                                      Wi_BW = numeric(0),
                                      a_BW = numeric(0),
                                      b_BW = numeric(0),
                                      c_BW = numeric(0),
                                      Wm_BP = numeric(0),
                                      B_BP = numeric(0),
                                      Wi_BP= numeric(0),
                                      Wm_FP= numeric(0), 
                                      B_FP= numeric(0),
                                      Wi_FP= numeric(0),
                                     
                                      a_BF= numeric(0),
                                      c_BF= numeric(0),#### Change b_BF to c_BF to avoid duplication
                                     
                                      B_BF = numeric(0),
                                      Wi_BF= numeric(0),
                                      Wm_BF= numeric(0),
                                      
                                      BW_pred= numeric(0),
                                      BW_NRC= numeric(0),
                                      BWG_pred= numeric(0), 
                                      BPW= numeric(0),  
                                      BPD= numeric(0), 
                                      FPW= numeric(0), 
                                      FPD= numeric(0), 
                                      FW= numeric(0),  
                                      FG= numeric(0),  
                                      MEreq = numeric(0),
                                      FI_pred = numeric(0),
                                      Lys_Eft = numeric(0),
                                      MetpCis_Ef = numeric(0),
                                      Thr_Eft = numeric(0), 
                                      Val_Eft = numeric(0),
                                      Iso_Eft = numeric(0), 
                                      Trp_Eft = numeric(0),
                                      Lys_MM = numeric(0),
                                      MetpCys_MM = numeric(0),
                                      Thr_MM = numeric(0), 
                                      Val_MM = numeric(0),
                                      Iso_MM = numeric(0),
                                      Trp_MM = numeric(0),
                                      LysRatio_MM = numeric(0),
                                      MetpCysRatio_MM = numeric(0), 
                                      ThrRatio_MM = numeric(0),
                                      ValRatio_MM = numeric(0),
                                      IsoRatio_MM = numeric(0),
                                      TrpRatio_MM = numeric(0),
                                      Lys_EM = numeric(0),
                                      MetpCys_EM = numeric(0), 
                                      Thr_EM = numeric(0),
                                      Val_EM = numeric(0), 
                                      Iso_EM = numeric(0),
                                      Trp_EM = numeric(0),
                                      LysRatio_EM = numeric(0),
                                      MetpCysRatio_EM = numeric(0),
                                      ThrRatio_EM = numeric(0),
                                      ValRatio_EM = numeric(0), 
                                      IsoRatio_EM = numeric(0),
                                      TrpRatio_EM  = numeric(0),
                                      Lys_MM_FI = numeric(0), 
                                      MetpCys_MM_FI = numeric(0),
                                      Thr_MM_FI = numeric(0),
                                      Val_MM_FI = numeric(0),
                                      Iso_MM_FI = numeric(0),
                                      Trp_MM_FI = numeric(0),
                                      Lys_EM_FI = numeric(0),
                                      MetpCys_EM_FI = numeric(0),
                                      Thr_EM_FI = numeric(0),
                                      Val_EM_FI = numeric(0),
                                      Iso_EM_FI = numeric(0),
                                      Trp_EM_FI  = numeric(0),
                                      stringsAsFactors = FALSE)
              newEntry <- observeEvent(input$update, {
              values$df <- rbind(values$df,
                                   data.frame(stringsAsFactors = FALSE,
                                              #Age = as.numeric(input$text3, digits= 0),

                                              Age =  c(({as.numeric(input$text3[1]):as.numeric(input$text3[2])})), 
                                              

              
                                              Line =as.character(if(input$text3 > 0){input$text1}
                                                                  else {NA}),
                                              Sex = as.character(if(input$text3 > 0){input$text2}
                                                               else {NA}), 
                                              Temp = as.numeric(if(input$text3 > 0){input$text4}
                                                                else {NA}),
                                          
                                              
                                              
                                              #Line = input$text1,
                                              #Sex = input$text2,  
                                              #Temp = as.numeric(input$text4),
                                              MEd = as.numeric(input$text5),
                                              BW_actual = as.numeric(input$text6),
                                              Intake_actual = as.numeric(input$text7),
                                              LCTemp = as.numeric(if(input$text3 == 1){31}
                                                else if(input$text3 == 2 ){31}
                                                else if(input$text3 == 3 ){31}
                                                else if(input$text3 == 4 ){31}
                                                else if(input$text3 == 5 ){31}
                                                else if(input$text3 == 6){31}
                                                else if(input$text3 == 7){31}                
                                                else if(input$text3 == 8){28}
                                                else if(input$text3 == 9){28}
                                                else if(input$text3 == 10){28}
                                                else if(input$text3 == 11){28}
                                                else if(input$text3 == 12){28}
                                                else if(input$text3 == 13){28}
                                                else if(input$text3 == 14){28}
                                                else if(input$text3 == 15){24}
                                                else if(input$text3 == 16){24}
                                                else if(input$text3 == 17){24}
                                                else if(input$text3 == 18){24}
                                                else if(input$text3 == 19){24}
                                                else if(input$text3 == 20){24}
                                                else if(input$text3 == 21){24}
                                                
                                                else if(input$text3 == 22){22}
                                                else if(input$text3 == 23){22}
                                                else if(input$text3 == 24){22}
                                                else if(input$text3 == 25){22}
                                                else if(input$text3 == 26){22}
                                                else if(input$text3 == 27){22}
                                                else if(input$text3 == 28){22}
                                                else if(input$text3 == 29){20}
                                                
                                                else if(input$text3 == 30){20}
                                                else if(input$text3 == 31){20}
                                                else if(input$text3 == 32){20}
                                                else if(input$text3 == 33){20}
                                                else if(input$text3 == 34){20}
                                                else if(input$text3 == 35){20}
                                                else if(input$text3 == 36){18}
                                                else if(input$text3 == 37){18}
                                                else if(input$text3 == 38){18}
                                                else if(input$text3 == 39){18}
                                                
                                                else if(input$text3 == 40){18}
                                                else if(input$text3 == 41){18}
                                                else if(input$text3 == 42){18}
                                                else if(input$text3 == 43){18}
                                                else if(input$text3 == 44){18}
                                                else if(input$text3 == 45){18}
                                                else if(input$text3 == 46){18}
                                                else if(input$text3 == 47){18}
                                                else if(input$text3 == 48){18}
                                                else if(input$text3 == 49){18}
                                                
                                                else if(input$text3 == 50){18}
                                                else if(input$text3 == 51){18}
                                                else if(input$text3 == 52){18}
                                                else if(input$text3 == 53){18}
                                                else if(input$text3 == 54){18}
                                                else if(input$text3 == 55){18}
                                                else if(input$text3 == 56){18}
                                                else if(input$text3 == 57){18}
                                                else if(input$text3 == 58){18}
                                                else if(input$text3 == 59){18}
                                                else if(input$text3 == 60){18}

                                                ),
                                               
                                                #M1_SL = paste(input$text1, input$text2), #merge two inputs to create a new ID
                                                
                                                # BODY WEIGHT(kg)	
                                                Wm_BW = if(input$text1 == "Ross" && input$text2 == "Male"){8.3755}
                                                else if(input$text1 == "Ross" && input$text2 == "Female" ){6.754}
                                                else if(input$text1 == "Cobb" && input$text2 == "Male" ){8.111}
                                                else if(input$text1 == "Cobb" && input$text2 == "Female" ){6.565}
                                                else if(input$text1 == "Hubbard" && input$text2 == "Male" ){9.186}
                                                else if(input$text1 == "Hubbard" && input$text2 == "Female"){6.874}, # include coma after each variable
                                                #class(reactive(Wm_BW ())),
                  
                                              
                                               B_BW = if(input$text2 == "Male" && input$text1 == "Ross"){0.040} # use = instead <- 
                                               else if(input$text2 == "Male" && input$text1 == "Cobb"){0.042}
                                               else if(input$text2 == "Male" && input$text1 == "Hubbard"){0.037}
                                               else if(input$text2 == "Female" && input$text1 == "Ross"){0.040}
                                               else if(input$text2 == "Female" && input$text1 == "Cobb"){0.042}
                                               else if(input$text2 == "Female" && input$text1 == "Hubbard"){0.039}, # include coma after each variable
                                              
                                              
                                              Wi_BW = if(input$text2 == "Male" && input$text1 == "Ross"){0.044} # use = instead <- 
                                              else if(input$text2 == "Male" && input$text1 == "Cobb"){0.044}
                                              else if(input$text2 == "Male" && input$text1 == "Hubbard"){0.041}
                                              else if(input$text2 == "Female" && input$text1 == "Ross"){0.042}
                                              else if(input$text2 == "Female" && input$text1 == "Cobb"){0.045}
                                              else if(input$text2 == "Female" && input$text1 == "Hubbard"){0.038}, # include coma after each variable
                                              
                
                                              ##### US BODY WEIGHT(kg) until 49 days	by VL Daley, Matheus and Victor et al. 2019
                                              
                                              a_BW = if(input$text8 == "Early-developing"){6117}
                                              else if(input$text8 == "Late-developing"){6117},
                                              
                                              b_BW = if(input$text8 == "Early-developing"){0.047} # use = instead <- 
                                              else if(input$text8 == "Late-developing"){0.047},
                                              
                                              c_BW = if(input$text8 == "Early-developing"){33.38} # use = instead <- 
                                              else if(input$text8 == "Late-developing"){33.38},
                                              
                                              
                                              # BODY PROTEIN (g)	
                                              Wm_BP = if(input$text2 == "Male" && input$text1 == "Ross"){1.266} # use = instead <- 
                                              else if(input$text2 == "Male" && input$text1 == "Cobb"){1.296}
                                              else if(input$text2 == "Male" && input$text1 == "Hubbard"){1.477}
                                              else if(input$text2 == "Female" && input$text1 == "Ross"){0.901}
                                              else if(input$text2 == "Female" && input$text1 == "Cobb"){0.910}
                                              else if(input$text2 == "Female" && input$text1 == "Hubbard"){0.949}, # include coma after each variable
                                              
                                              B_BP = if(input$text2 == "Male" && input$text1 == "Ross"){0.039} # use = instead <- 
                                              else if(input$text2 == "Male" && input$text1 == "Cobb"){0.043}
                                              else if(input$text2 == "Male" && input$text1 == "Hubbard"){0.036}
                                              else if(input$text2 == "Female" && input$text1 == "Ross"){0.041}
                                              else if(input$text2 == "Female" && input$text1 == "Cobb"){0.043}
                                              else if(input$text2 == "Female" && input$text1 == "Hubbard"){0.040}, # include coma after each variable
                                              
                                              Wi_BP = if(input$text2 == "Male" && input$text1 == "Ross"){0.0055} # use = instead <- 
                                              else if(input$text2 == "Male" && input$text1 == "Cobb"){0.0060}
                                              else if(input$text2 == "Male" && input$text1 == "Hubbard"){0.0052}
                                              else if(input$text2 == "Female" && input$text1 == "Ross"){0.005}
                                              else if(input$text2 == "Female" && input$text1 == "Cobb"){0.0060}
                                              else if(input$text2 == "Female" && input$text1 == "Hubbard"){0.0050}, # include coma after each variable
                                             
                                               # FEATHER PROT(g)		
                                              Wm_FP = if(input$text2 == "Male" && input$text1 == "Ross"){0.2171} # use = instead <- 
                                              else if(input$text2 == "Male" && input$text1 == "Cobb"){0.1897}
                                              else if(input$text2 == "Male" && input$text1 == "Hubbard"){0.1773}
                                              else if(input$text2 == "Female" && input$text1 == "Ross"){0.1804}
                                              else if(input$text2 == "Female" && input$text1 == "Cobb"){0.1752}
                                              else if(input$text2 == "Female" && input$text1 == "Hubbard"){0.1700}, # include coma after each variable
                                              
                                              B_FP = if(input$text2 == "Male" && input$text1 == "Ross"){0.039} # use = instead <- 
                                              else if(input$text2 == "Male" && input$text1 == "Cobb"){0.045}
                                              else if(input$text2 == "Male" && input$text1 == "Hubbard"){0.042}
                                              else if(input$text2 == "Female" && input$text1 == "Ross"){0.042}
                                              else if(input$text2 == "Female" && input$text1 == "Cobb"){0.047}
                                              else if(input$text2 == "Female" && input$text1 == "Hubbard"){0.050}, # include coma after each variable
                                              
                                              Wi_FP = if(input$text2 == "Male" && input$text1 == "Ross"){0.00158} # use = instead <- 
                                              else if(input$text2 == "Male" && input$text1 == "Cobb"){0.00137}
                                              else if(input$text2 == "Male" && input$text1 == "Hubbard"){0.00137}
                                              else if(input$text2 == "Female" && input$text1 == "Ross"){0.00157}
                                              else if(input$text2 == "Female" && input$text1 == "Cobb"){0.00156}
                                              else if(input$text2 == "Female" && input$text1 == "Hubbard"){0.00149}, # include coma after each variable
                                              
                                              # BODY FAT (g)	
                                               
                                              a_BF = if(input$text2 == "Male" && input$text1 == "Ross"){-1.247} # use = instead <-
                                              else if(input$text2 == "Male" && input$text1 == "Cobb"){-1.225}
                                              else if(input$text2 == "Male" && input$text1 == "Hubbard"){-1.357}
                                              else if(input$text2 == "Female" && input$text1 == "Ross"){-1.3727}
                                              else if(input$text2 == "Female" && input$text1 == "Cobb"){-1.681}
                                              else if(input$text2 == "Female" && input$text1 == "Hubbard"){-1.5537}, # include coma after each variable

                                              c_BF = if(input$text2 == "Male" && input$text1  == "Ross"){1.157} # use = instead <-
                                              else if(input$text2 == "Male" && input$text1  == "Cobb"){1.157}
                                              else if(input$text2 == "Male" && input$text1  == "Hubbard"){1.157}
                                              else if(input$text2 == "Female" && input$text1  == "Ross"){1.1969}
                                              else if(input$text2 == "Female" && input$text1  == "Cobb"){1.2468}
                                              else if(input$text2 == "Female" && input$text1  == "Hubbard"){1.221},
                                              
                                              Wm_BF = if(input$text2 == "Male" && input$text1 == "Ross"){1.3434} # use = instead <- 
                                              else if(input$text2 == "Male" && input$text1 == "Cobb"){1.1703}
                                              else if(input$text2 == "Male" && input$text1 == "Hubbard"){1.3441}
                                              else if(input$text2 == "Female" && input$text1 == "Ross"){1.1809}
                                              else if(input$text2 == "Female" && input$text1 == "Cobb"){1.0695}
                                              else if(input$text2 == "Female" && input$text1 == "Hubbard"){1.1847}, # include coma after each variable
                                              
                                              B_BF = if(input$text2 == "Male" && input$text1 == "Ross"){0.039} # use = instead <- 
                                              else if(input$text2 == "Male" && input$text1 == "Cobb"){0.041}
                                              else if(input$text2 == "Male" && input$text1 == "Hubbard"){0.039}
                                              else if(input$text2 == "Female" && input$text1 == "Ross"){0.036}
                                              else if(input$text2 == "Female" && input$text1 == "Cobb"){0.040}
                                              else if(input$text2 == "Female" && input$text1 == "Hubbard"){0.036}, # include coma after each variable
                                              
                                              Wi_BF = if(input$text2 == "Male" && input$text1 == "Ross"){0.0023} # use = instead <- 
                                              else if(input$text2 == "Male" && input$text1 == "Cobb"){0.0020}
                                              else if(input$text2 == "Male" && input$text1 == "Hubbard"){0.0016}
                                              else if(input$text2 == "Female" && input$text1 == "Ross"){0.0022}
                                              else if(input$text2 == "Female" && input$text1 == "Cobb"){0.0019}
                                              else if(input$text2 == "Female" && input$text1 == "Hubbard"){0.017}, # include coma after each variable
                                              
                                              
                                              BW_pred= "NA",
                                              BW_NRC= "NA",
                                              BWG_pred = "NA",
                                              BPW= "NA",  
                                              BPD= "NA", 
                                              FPW= "NA", 
                                              FPD= "NA", 
                                              FW= "NA",  
                                              FG= "NA",           
                                              MEreq= "NA",
                                              FI_pred = "NA",
                                              Lys_Eft = "NA",
                                              MetpCis_Eft = "NA",#I change all ..._Ef to ..._Eft
                                              Thr_Eft = "NA", 
                                              Val_Eft = "NA",
                                              Iso_Eft = "NA",
                                              Trp_Eft = "NA",
                                              Lys_MM = "NA",
                                              MetpCys_MM = "NA",
                                              Thr_MM = "NA",
                                              Val_MM = "NA",
                                              Iso_MM = "NA",
                                              Trp_MM = "NA",
                                              LysRatio_MM = "NA",
                                              MetpCysRatio_MM = "NA",
                                              ThrRatio_MM = "NA",
                                              ValRatio_MM = "NA",
                                              IsoRatio_MM = "NA",
                                              TrpRatio_MM = "NA",
                                              Lys_EM = "NA",
                                              MetpCys_EM = "NA",
                                              Thr_EM = "NA",
                                              Val_EM = "NA",
                                              Iso_EM = "NA",
                                              Trp_EM = "NA",
                                              LysRatio_EM = "NA",
                                              MetpCysRatio_EM = "NA",
                                              ThrRatio_EM = "NA",
                                              ValRatio_EM = "NA",
                                              IsoRatio_EM = "NA",
                                              TrpRatio_EM  = "NA",
                                              
                                              Lys_MM_FI = "NA",
                                              MetpCys_MM_FI = "NA",
                                              Thr_MM_FI = "NA",
                                              Val_MM_FI = "NA",
                                              Iso_MM_FI = "NA",
                                              Trp_MM_FI = "NA",
                                              
                                              Lys_EM_FI = "NA",
                                              MetpCys_EM_FI = "NA",
                                              Thr_EM_FI = "NA",
                                              Val_EM_FI = "NA",
                                              Iso_EM_FI = "NA",
                                              Trp_EM_FI  = "NA"
                                              
                                   )
              )
              })
              
              ### ***********Create Table 1 for Inputs 
              
              output$table1 <- renderDataTable(   # Dataset with imputs, but no equations
                values$df,
                options=list(
                  pageLength = 14,
               "columnDefs" = list(
                list("targets" = c(8:78), 
                "visible" = FALSE)))
              )
          
              
                                             
              ############# *********Create T2 table Handson

              
              output$table2 <- renderRHandsontable({ digits=5# Dataset with imputs and equations from RESPONSE VARIABLES
                isolate({
                  
                values$df$BW_pred <- values$df$Wm_BW * (exp(-exp((log(-log(values$df$Wi_BW / values$df$Wm_BW)) - (values$df$B_BW * values$df$Age)   )))) 
                
                values$df$BW_NRC <- values$df$a_BW * exp(-exp(-values$df$b_BW *(values$df$Age - values$df$c_BW ))) /1000
                
                values$df$BWG_pred <- values$df$B_BW * values$df$BW_pred * log(values$df$Wm_BW/values$df$BW_pred)
                
                values$df$BPW <- values$df$Wm_BP*(exp(-exp((log(-log(values$df$Wi_BP/values$df$Wm_BP))-(values$df$B_BP*values$df$Age))))) # body protein weight
                values$df$BPD <- values$df$B_BP*values$df$BPW*log(values$df$Wm_BP/values$df$BPW)*1000 # body protein deposition
              
                values$df$FPW <- values$df$Wm_FP * (exp(-exp((log(-log(values$df$Wi_FP/values$df$Wm_FP))-(values$df$B_FP*values$df$Age)))))
                values$df$FPD <- values$df$B_FP * values$df$FPW*log(values$df$Wm_FP/values$df$FPW)*1000
                
                values$df$FW  <- 2.71828^(values$df$a_BF+ log(values$df$BPW*1000)*values$df$c_BF) #fat weight
                values$df$FG  <- values$df$B_BF*values$df$FW*log((values$df$Wm_BF*1000)/values$df$FW) # body fat deposition
               
                
                # isolate(
                #   if(input$text3 == 1)
                # {values$df$FG <- ave(values$df$FW, factor(input$text3), FUN=function(x) c(NA,diff(x)))
                # } else {
                # values$df$FG <- values$df$FW * 0.18} )##### Review conditional in R
                
                # isolate(
                #   if(input$text3 == 1)
                #   {values$df$FG <- values$df$FW * 0.18
                #   } else {
                #     values$df$FG <- ave(values$df$FW, factor(input$text3), FUN=function(x) c(NA,diff(x)))} )##### Review conditional in R
                
                
               
                  
                #########  ME Model and estimation of feed intake ##################################
                
                 # isolate(
                 #   if(values$df$LCTemp > input$text4)
                 # {values$df$MEreq = ((values$df$BW_pred^0.75) * 113 + 0.88*(input$text4 - values$df$LCTemp)+(13.5*values$df$FG)+(12.3*values$df$BPD))
                 # ;} else {
                 # values$df$MEreq = ((values$df$BW_pred ^0.75) * 113 + 6.73*(input$text4 - values$df$Temp)+(13.5*values$df$FG)+(12.3*values$df$BPD))
                 # ;} # Review conditional in R, not working
                 # )
                
              
                # input$text4 = as.numeric(input$text4)
                # value$df$LCTemp = as.numeric(values$df$LCTemp)
                # input$text5 = as.numeric(input$text5)
                
                # d$MEreq <- ifelse(d$Temp >= d$LCTemp, d$BW^0.75 * 113 + 0.88*(d$Temp-d$LCTemp)+(13.5*d$FG)+(12.3*d$BPD),
                #                   ifelse(d$Temp <= d$LCTemp, d$BW^0.75 * 113 + 6.73*(d$LCTemp-d$Temp)+(13.5*d$FG)+(12.3*d$BPD),"NA"))
                # d$FI <- d$MEreq / d$MEd
                

                # isolate(
                #   if(input$text4 > values$df$LCTemp)
                # {values$df$MEreq = ((values$df$BW_pred^0.75) * 113 + 0.88*(input$text4 - values$df$LCTemp)+(13.5*values$df$FG)+(12.3*values$df$BPD))
                # ;} else{
                #   values$df$MEreq = ((values$df$BW_pred^0.75) * 113 + 6.73*(values$df$LCTemp - input$text4)+(13.5*values$df$FG)+(12.3*values$df$BPD))
                #   ;} # Review conditional in R, not working
                # )
                # 
                # 
                #  values$df$FIpred <-  values$df$MEreq / input$text5 # Add above

               
    
                #########  Efficiency of AAs Utilization ###############
                # Reference: Nilva (2018)
                
                # Tables 
                
                isolate(
                  if(input$text3 > 0)
                  {values$df$Lys_Eft <- as.numeric(0.77)
                  } else {"NA"} )
                  
                isolate(
                  if(input$text3 > 0)
                  {values$df$MetpCis_Eft <- 0.78 #I change all ..._Ef to ..._Eft
                  } else {"NA"} )
                
                isolate(
                  if(input$text3 > 0)
                  {values$df$Thr_Eft <- 0.73
                  } else {"NA"} )
                
                isolate(
                  if(input$text3 > 0)
                  {values$df$Val_Eft <- 0.73
                  } else {"NA"} )
                
                isolate(
                  if(input$text3 > 0)
                  {values$df$Iso_Eft <- 0.69
                  } else {"NA"} )
                
                isolate(
                  if(input$text3 > 0)
                  {values$df$Trp_Eft <- 0.71
                  } else {"NA"} )
                
                
                #########  1) Mechanistic Models ################
                
                  values$df$Lys_MM <- ((75*as.numeric(values$df$BPD) + 18 * as.numeric(values$df$FPD))/as.numeric(values$df$Lys_Eft)) +     (as.numeric(values$df$Wm_BP)) ^ 0.73*(as.numeric(values$df$BPW)/as.numeric(values$df$Wm_BP)*151.2) +(0.01*as.numeric(values$df$FPW)*18)
                  values$df$MetpCys_MM <- ((36*as.numeric(values$df$BPD) + 89 * as.numeric(values$df$FPD))/as.numeric(values$df$MetpCis_Eft)) + (as.numeric(values$df$Wm_BP)) ^ 0.73*(as.numeric(values$df$BPW)/as.numeric(values$df$Wm_BP)*87.2) + (0.01*as.numeric(values$df$FPW)*89)
                  values$df$Thr_MM <- ((42*as.numeric(values$df$BPD) + 44 * as.numeric(values$df$FPD))/as.numeric(values$df$Thr_Eft)) +     (as.numeric(values$df$Wm_BP)) ^ 0.73*(as.numeric(values$df$BPW)/as.numeric(values$df$Wm_BP)*75.5) + (0.01*as.numeric(values$df$FPW)*44)
                  values$df$Val_MM <-  ((45*as.numeric(values$df$BPD) + 69 * as.numeric(values$df$FPD))/as.numeric(values$df$Val_Eft)) +     (as.numeric(values$df$Wm_BP)) ^ 0.73*(as.numeric(values$df$BPW)/as.numeric(values$df$Wm_BP)*219)  + (0.01*as.numeric(values$df$FPW)*69)
                  values$df$Iso_MM <-  ((40*as.numeric(values$df$BPD) + 47 * as.numeric(values$df$FPD))/as.numeric(values$df$Iso_Eft)) +     (as.numeric(values$df$Wm_BP)) ^ 0.73*(as.numeric(values$df$BPW)/as.numeric(values$df$Wm_BP)*134)  + (0.01*as.numeric(values$df$FPW)*47)
                  values$df$Trp_MM <-  ((11*as.numeric(values$df$BPD) + 07 * as.numeric(values$df$FPD))/as.numeric(values$df$Trp_Eft)) +     (as.numeric(values$df$Wm_BP)) ^ 0.73*(as.numeric(values$df$BPW)/as.numeric(values$df$Wm_BP)*37)   + (0.01*as.numeric(values$df$FPW)*07)
                   #I change all ..._Ef to ..._Eft
                                         
                
                # Ideal AA Ratio using Mechanistic Models 
                
                 values$df$LysRatio_MM <- values$df$Lys_MM/values$df$Lys_MM*100
                values$df$MetpCysRatio_MM <- values$df$MetpCys_MM/values$df$Lys_MM*100
                values$df$ThrRatio_MM<- values$df$Thr_MM/values$df$Lys_MM*100
                 values$df$ValRatio_MM <- values$df$Val_MM/values$df$Lys_MM*100
                values$df$IsoRatio_MM <- values$df$Iso_MM/values$df$Lys_MM*100
                values$df$TrpRatio_MM <-values$df$Trp_MM/values$df$Lys_MM*100 
                
                ######### 2) Empirical Models ################
              
                values$df$Lys_EM <- 45.1 * (as.numeric(values$df$BW_pred)^0.75)  + (-23.14+13.39*(as.numeric(values$df$BWG_pred)*1000))/as.numeric(values$df$Lys_Eft)
                values$df$MetpCys_EM <- 25.5 * (as.numeric(values$df$BW_pred)^0.75)  + (-28.342+9.68*(as.numeric(values$df$BWG_pred)*1000))/as.numeric(values$df$MetpCis_Eft)
                values$df$Thr_EM <- 21.9 * (as.numeric(values$df$BW_pred)^0.75)  +  (-11.15+9.13*(as.numeric(values$df$BWG_pred)*1000))/as.numeric(values$df$Thr_Eft)
                values$df$Val_EM <- 65.4 * (as.numeric(values$df$BW_pred)^0.75)  +  (-22.03+9.54*(as.numeric(values$df$BWG_pred)*1000))/as.numeric(values$df$Val_Eft)
                values$df$Iso_EM <- 32 *   (as.numeric(values$df$BW_pred)^0.75) +(-13.792+7.3444*(as.numeric(values$df$BWG_pred)*1000))/as.numeric(values$df$Iso_Eft)
                values$df$Trp_EM <- 9 *    (as.numeric(values$df$BW_pred)^0.75)  +   (-2.94+2.38*(as.numeric(values$df$BWG_pred)*1000))/as.numeric(values$df$Trp_Eft) #I change all ..._Ef to ..._Eft
           
                # Ideal AA Ratio using Empirical Models
                
                values$df$LysRatio_EM <-     values$df$Lys_EM/values$df$Lys_EM*100
                values$df$MetpCysRatio_EM <- values$df$MetpCys_EM/values$df$Lys_EM*100
                values$df$ThrRatio_EM <-     values$df$Thr_EM/values$df$Lys_EM*100
                values$df$ValRatio_EM <-     values$df$Val_EM/values$df$Lys_EM*100
                values$df$IsoRatio_EM <-     values$df$Iso_EM/values$df$Lys_EM*100
                 values$df$TrpRatio_EM <-    values$df$Trp_EM/values$df$Lys_EM*100
                
                
                 #  AAs requirements in % intake observed (FI)
                
                 # 1) Mechanistic
                  values$df$Lys_MM_FI <-     (values$df$Lys_MM/1000) / as.numeric(values$df$Intake_actual) * 100
                  values$df$MetpCys_MM_FI <- (values$df$MetpCys_MM/1000) / as.numeric(values$df$Intake_actual) * 100
                  values$df$Thr_MM_FI <-     (values$df$Thr_MM/1000) / as.numeric(values$df$Intake_actual) * 100
                  values$df$Val_MM_FI <-     (values$df$Val_MM/1000) / as.numeric(values$df$Intake_actual) * 100
                  values$df$Iso_MM_FI <-     (values$df$Iso_MM/1000) / as.numeric(values$df$Intake_actual) * 100 
                  values$df$Trp_MM_FI <-     (values$df$Trp_MM/1000) / as.numeric(values$df$Intake_actual) * 100
                 
                 # 1) Empiric
                  values$df$Lys_EM_FI <-     (values$df$Lys_EM/1000) /    as.numeric(values$df$Intake_actual) * 100
                  values$df$MetpCys_EM_FI <- (values$df$MetpCys_EM/1000) / as.numeric(values$df$Intake_actual) * 100 # Find error here: There was an "f" missing in the statement (values$df$Intake_actual)
                  values$df$Thr_EM_FI <-     (values$df$Thr_EM/1000) / as.numeric(values$df$Intake_actual) * 100
                  values$df$Val_EM_FI <-     (values$df$Val_EM /1000) / as.numeric(values$df$Intake_actual) * 100
                  values$df$Iso_EM_FI <-     (values$df$Iso_EM /1000) / as.numeric(values$df$Intake_actual) * 100
                  values$df$Trp_EM_FI <-     (values$df$Trp_EM /1000) / as.numeric(values$df$Intake_actual) * 100
                

                
                })
                rhandsontable(values$df,selectCallback = TRUE) #maxRows = input$Age
              })
            
              
              
              
              #if a user updates table1, table2 should also update.
              
              observeEvent(input$table2,{
                df <- hot_to_r(input$table2)
                df <- as.data.frame(df)
                values$df <- df
              })
              
             ########## refresh
              
             
              
              observeEvent(input$reset, {
                values$df <- NULL
              })  
              
                
      
     ###### Save  data set to local 
      ## create the save function
      
               saveData <- function(){
                 write.csv(values$df, file = "Predictions of AAs requirements for Broilers.csv", row.names = FALSE)
               }
      
      ## on save button click event, dataset will be saved to working directory
      observeEvent(input$saveBtn, saveData())
      
      
      ##### Dowload dataset
      
      
      output$BroilersData.csv <- downloadHandler(
        filename = function() {
          'Edited table.csv'
        },
        # what should go in place of table
        content = function(file) {
          write.csv(values$df, file)
        }
      )
      
      
      
      ############ Plots #######################
      
      
      # renderPlotly() also understands ggplot2 objects!
     
      x <- list(title = "Age, days")
      
      y1 <- list(title = "Lys,  mg/d")
      y2 <- list(title = "Met plus Cys, mg/d")
      y3 <- list(title = "Thr, mg/d")
      y4 <- list(title = "Val, mg/d")
      y5 <- list(title = "Ile, mg/d")
      y6 <- list(title = "Trp, mg/d")
      
      
      
     # 1 Lys
  
      output$plot1a <- renderPlotly({
        ggplot(values$df, aes(as.numeric(values$df$Age), as.numeric(values$df$Lys_MM))) +
          geom_bar((aes(fill = values$df$Sex)), stat="identity", position="dodge", show.legend=FALSE, size = 1)+ 
          xlab("Age, d")+ ylab("Lys intake (Mechanicistic), mg/d") +
          #  geom_text(aes(label= round(values$df$Lys_MM)),position=position_dodge(0.9), vjust=-0.2, size = 3) +
          theme(legend.position="bottom") + theme_set(theme_classic(base_size=13)) +
          scale_x_continuous(breaks = values$df$Age) 
       
      })
      
 
      output$plot1b <- renderPlotly({
        ggplot(values$df, aes(as.numeric(values$df$Age), as.numeric(values$df$Lys_EM)))+
          geom_bar((aes(fill = values$df$Sex)), stat="identity", position="dodge", show.legend=FALSE, size = 1)+ 
          xlab("Age, d")+ ylab("Lys intake (Empirical), mg/d") +
          #  geom_text(aes(label= round(values$df$Lys_EM, digits = 0)),position=position_dodge(width = 0.9), vjust=-0.2, size = 3) +
          theme(legend.position="bottom") + theme_set(theme_classic(base_size=13)) +
          scale_x_continuous(breaks = values$df$Age) 
        
      }) 
     
      
      output$plot1c <- renderPlotly({
        ggplot(values$df, aes(as.numeric(values$df$Age), as.numeric(values$df$Lys_MM))) +
          geom_bar((aes(fill = values$df$Line)), stat="identity", position="dodge", show.legend=FALSE, size = 1)+ 
          xlab("Age, d")+ ylab("Lys intake (Mechanicistic), mg/d") +
          #  geom_text(aes(label= round(values$df$Lys_MM, digits = 0)),position=position_dodge(0.9), vjust=-0.2, size = 3) +
          theme(legend.position="bottom") + theme_set(theme_classic(base_size=13)) +
          scale_x_continuous(breaks = values$df$Age) 
        
      })
      
      
      output$plot1d <- renderPlotly({
        ggplot(values$df, aes(as.numeric(values$df$Age), as.numeric(values$df$Lys_EM)))+
          geom_bar((aes(fill = values$df$Line)), stat="identity", position="dodge", show.legend=FALSE, size = 1)+ 
          xlab("Age, d")+ ylab("Lys intake (Empirical), mg/d") +
          #  geom_text(aes(label= round(values$df$Lys_EM, digits = 0)),position=position_dodge(width = 0.9), vjust=-0.2, size = 3) +
          theme(legend.position="bottom") + theme_set(theme_classic(base_size=13)) +
          scale_x_continuous(breaks = values$df$Age) 
        
      })    
      
      
      # 2 MetpCys_MM 
      
      output$plot2a <- renderPlotly({
        ggplot(values$df, aes(as.numeric(values$df$Age), as.numeric(values$df$MetpCys_MM)))+
          geom_bar((aes(fill = values$df$Sex)), stat="identity", position="dodge", show.legend=FALSE, size = 1)+ 
          xlab("Age, d")+ ylab("Met plus Cys intake (Mechanicistic), mg/d") +
          #  geom_text(aes(label= round(values$df$Lys_MM, digits = 0)),position=position_dodge(width = 0.9), vjust=-0.2, size = 3) +
          theme(legend.position="bottom") + theme_set(theme_classic(base_size=13)) +
          scale_x_continuous(breaks = values$df$Age) 
        
      })
      
      
      output$plot2b <- renderPlotly({
        ggplot(values$df, aes(as.numeric(values$df$Age), as.numeric(values$df$MetpCys_EM)))+
          geom_bar((aes(fill = values$df$Sex)), stat="identity", position="dodge", show.legend=FALSE, size = 1)+ 
          xlab("Age, d")+ ylab("Met plus Cys intake (Empirical), mg/d") +
          #  geom_text(aes(label= round(values$df$MetpCys_EM, digits = 0)),position=position_dodge(width=0.9), vjust=-0.2, size = 3) +
          theme(legend.position="bottom") + theme_set(theme_classic(base_size=13)) +
          scale_x_continuous(breaks = values$df$Age)
          
        
      })
      
     
      output$plot2c <- renderPlotly({
        ggplot(values$df, aes(as.numeric(values$df$Age), as.numeric(values$df$MetpCys_MM)))+
          geom_bar((aes(fill = values$df$Line)), stat="identity", position="dodge", show.legend=FALSE, size = 1)+ 
          xlab("Age, d")+ ylab("Met plus Cys intake (Mechanicistic), mg/d") +
          #        geom_text(aes(label= round(values$df$Lys_MM, digits = 0)),position=position_dodge(width = 0.9), vjust=-0.2, size = 3) +
          theme(legend.position="bottom") + theme_set(theme_classic(base_size=13)) +
          scale_x_continuous(breaks = values$df$Age) 
        
      })
      
      
      output$plot2d <- renderPlotly({
        ggplot(values$df, aes(as.numeric(values$df$Age), as.numeric(values$df$MetpCys_EM)))+
          geom_bar((aes(fill = values$df$Line)), stat="identity", position="dodge", show.legend=FALSE, size = 1)+ 
          xlab("Age, d")+ ylab("Met plus Cys intake (Empirical), mg/d") +
          #      geom_text(aes(label= round(values$df$MetpCys_EM, digits = 0)),position=position_dodge(width=0.9), vjust=-0.2, size = 3) +
          theme(legend.position="bottom") + theme_set(theme_classic(base_size=13)) +
          scale_x_continuous(breaks = values$df$Age)
        
        
      }) 
      
      # 3 Thr
      
      output$plot3a <- renderPlotly({
        ggplot(values$df, aes(as.numeric(values$df$Age), as.numeric(values$df$Thr_MM)))+
          geom_bar((aes(fill = values$df$Sex)), stat="identity", position="dodge", show.legend=FALSE, size = 1)+ 
          xlab("Age, d")+ ylab("Thr intake (Mechanicistic), mg/d") +
          #  geom_text(aes(label= round(values$df$Thr_MM, digits = 0)),position=position_dodge(width = 0.9), vjust=-0.2, size = 3) +
          theme(legend.position="bottom") + theme_set(theme_classic(base_size=13)) +
          scale_x_continuous(breaks = values$df$Age) 
        
      })
      
      
      output$plot3b <- renderPlotly({
        ggplot(values$df, aes(as.numeric(values$df$Age), as.numeric(values$df$Thr_EM)))+
          geom_bar((aes(fill = values$df$Sex)), stat="identity", position="dodge", show.legend=FALSE, size = 1)+ 
          xlab("Age, d")+ ylab("Thr intake (Empirical), mg/d") +
          #   geom_text(aes(label= round(values$df$Thr_EM, digits = 0)),position=position_dodge(width=0.9), vjust=-0.2, size = 3) +
          theme(legend.position="bottom") + theme_set(theme_classic(base_size=13)) +
          scale_x_continuous(breaks = values$df$Age)
        
    
      })
      
      output$plot3c <- renderPlotly({
        ggplot(values$df, aes(as.numeric(values$df$Age), as.numeric(values$df$Thr_MM)))+
          geom_bar((aes(fill = values$df$Line)), stat="identity", position="dodge", show.legend=FALSE, size = 1)+ 
          xlab("Age, d")+ ylab("Thr intake (Mechanicistic), mg/d") +
          #     geom_text(aes(label= round(values$df$Thr_MM, digits = 0)),position=position_dodge(width = 0.9), vjust=-0.2, size = 3) +
          theme(legend.position="bottom") + theme_set(theme_classic(base_size=13)) +
          scale_x_continuous(breaks = values$df$Age) 
        
      })
      
      
      output$plot3d <- renderPlotly({
        ggplot(values$df, aes(as.numeric(values$df$Age), as.numeric(values$df$Thr_EM)))+
          geom_bar((aes(fill = values$df$Line)), stat="identity", position="dodge", show.legend=FALSE, size = 1)+ 
          xlab("Age, d")+ ylab("Thr intake (Empirical), mg/d") +
          #   geom_text(aes(label= round(values$df$Thr_EM, digits = 0)),position=position_dodge(width=0.9), vjust=-0.2, size = 3) +
          theme(legend.position="bottom") + theme_set(theme_classic(base_size=13)) +
          scale_x_continuous(breaks = values$df$Age)
        
        
      })
      
      # 4 Val
      
      output$plot4a <- renderPlotly({
        ggplot(values$df, aes(as.numeric(values$df$Age), as.numeric(values$df$Val_MM)))+
          geom_bar((aes(fill = values$df$Sex)), stat="identity", position="dodge", show.legend=FALSE, size = 1)+ 
          xlab("Age, d")+ ylab("Val intake (Mechanicistic), mg/d") +
          #    geom_text(aes(label= round(values$df$Val_MM, digits = 0)),position=position_dodge(width = 0.9), vjust=-0.2, size = 3) +
          theme(legend.position="bottom") + theme_set(theme_classic(base_size=13)) +
          scale_x_continuous(breaks = values$df$Age) 
        
      })
      
      
      output$plot4b <- renderPlotly({
        ggplot(values$df, aes(as.numeric(values$df$Age), as.numeric(values$df$Val_EM)))+
          geom_bar((aes(fill = values$df$Sex)), stat="identity", position="dodge", show.legend=FALSE, size = 1)+ 
          xlab("Age, d")+ ylab("Val intake (Empirical), mg/d") +
          #   geom_text(aes(label= round(values$df$Val_EM, digits = 0)),position=position_dodge(width=0.9), vjust=-0.2, size = 3) +
          theme(legend.position="bottom") + theme_set(theme_classic(base_size=13)) +
          scale_x_continuous(breaks = values$df$Age)
        
        
      })
      
      output$plot4c <- renderPlotly({
        ggplot(values$df, aes(as.numeric(values$df$Age), as.numeric(values$df$Val_MM)))+
          geom_bar((aes(fill = values$df$Line)), stat="identity", position="dodge", show.legend=FALSE, size = 1)+ 
          xlab("Age, d")+ ylab("Val intake (Mechanicistic), mg/d") +
          #   geom_text(aes(label= round(values$df$Val_MM, digits = 0)),position=position_dodge(width = 0.9), vjust=-0.2, size = 3) +
          theme(legend.position="bottom") + theme_set(theme_classic(base_size=13)) +
          scale_x_continuous(breaks = values$df$Age) 
        
      })
      
      
      output$plot4d <- renderPlotly({
        ggplot(values$df, aes(as.numeric(values$df$Age), as.numeric(values$df$Val_EM)))+
          geom_bar((aes(fill = values$df$Line)), stat="identity", position="dodge", show.legend=FALSE, size = 1)+ 
          xlab("Age, d")+ ylab("Val intake (Empirical), mg/d") +
          #  geom_text(aes(label= round(values$df$Val_EM, digits = 0)),position=position_dodge(width=0.9), vjust=-0.2, size = 3) +
          theme(legend.position="bottom") + theme_set(theme_classic(base_size=13)) +
          scale_x_continuous(breaks = values$df$Age)
        
        
      })
      
      # 5 Iso
      
      
      output$plot5a <- renderPlotly({
        ggplot(values$df, aes(as.numeric(values$df$Age), as.numeric(values$df$Iso_MM)))+
          geom_bar((aes(fill = values$df$Sex)), stat="identity", position="dodge", show.legend=FALSE, size = 1)+ 
          xlab("Age, d")+ ylab("Iso intake (Mechanicistic), mg/d") +
          #  geom_text(aes(label= round(values$df$Iso_MM, digits = 0)),position=position_dodge(width = 0.9), vjust=-0.2, size = 3) +
          theme(legend.position="bottom") + theme_set(theme_classic(base_size=13)) +
          scale_x_continuous(breaks = values$df$Age) 
        
      })
      
      
      output$plot5b <- renderPlotly({
        ggplot(values$df, aes(as.numeric(values$df$Age), as.numeric(values$df$Iso_EM)))+
          geom_bar((aes(fill = values$df$Sex)), stat="identity", position="dodge", show.legend=FALSE, size = 1)+ 
          xlab("Age, d")+ ylab("Iso intake (Empirical), mg/d") +
          #  geom_text(aes(label= round(values$df$Iso_EM, digits = 0)),position=position_dodge(width=0.9), vjust=-0.2, size = 3) +
          theme(legend.position="bottom") + theme_set(theme_classic(base_size=13)) +
          scale_x_continuous(breaks = values$df$Age)

      })
      
      
      output$plot5c <- renderPlotly({
        ggplot(values$df, aes(as.numeric(values$df$Age), as.numeric(values$df$Iso_MM)))+
          geom_bar((aes(fill = values$df$Line)), stat="identity", position="dodge", show.legend=FALSE, size = 1)+ 
          xlab("Age, d")+ ylab("Iso intake (Mechanicistic), mg/d") +
          #  geom_text(aes(label= round(values$df$Iso_MM, digits = 0)),position=position_dodge(width = 0.9), vjust=-0.2, size = 3) +
          theme(legend.position="bottom") + theme_set(theme_classic(base_size=13)) +
          scale_x_continuous(breaks = values$df$Age) 
        
      })
      
      
      output$plot5d <- renderPlotly({
        ggplot(values$df, aes(as.numeric(values$df$Age), as.numeric(values$df$Iso_EM)))+
          geom_bar((aes(fill = values$df$Line)), stat="identity", position="dodge", show.legend=FALSE, size = 1)+ 
          xlab("Age, d")+ ylab("Iso intake (Empirical), mg/d") +
          #  geom_text(aes(label= round(values$df$Iso_EM, digits = 0)),position=position_dodge(width=0.9), vjust=-0.2, size = 3) +
          theme(legend.position="bottom") + theme_set(theme_classic(base_size=13)) +
          scale_x_continuous(breaks = values$df$Age)
        
      })
      
      
      
      # 6 Trp
      
      
      output$plot6a <- renderPlotly({
        ggplot(values$df, aes(as.numeric(values$df$Age), as.numeric(values$df$Trp_MM)))+
          geom_bar((aes(fill = values$df$Sex)), stat="identity", position="dodge", show.legend=FALSE, size = 1)+ 
          xlab("Age, d")+ ylab("Trp intake (Mechanicistic), mg/d") +
          #  geom_text(aes(label= round(values$df$Trp_MM, digits = 0)),position=position_dodge(width = 0.9), vjust=-0.2, size = 3) +
          theme(legend.position="bottom") + theme_set(theme_classic(base_size=13)) +
          scale_x_continuous(breaks = values$df$Age) 
        
      })
      
      
      output$plot6b <- renderPlotly({
        ggplot(values$df, aes(as.numeric(values$df$Age), as.numeric(values$df$Trp_EM)))+
          geom_bar((aes(fill = values$df$Sex)), stat="identity", position="dodge", show.legend=FALSE, size = 1)+ 
          xlab("Age, d")+ ylab("Trp intake (Empirical), mg/d") +
          #   geom_text(aes(label= round(values$df$Trp_EM, digits = 0)),position=position_dodge(width=0.9), vjust=-0.2, size = 3) +
          theme(legend.position="bottom") + theme_set(theme_classic(base_size=13)) +
          scale_x_continuous(breaks = values$df$Age)
        
      })   
    
      output$plot6c <- renderPlotly({
        ggplot(values$df, aes(as.numeric(values$df$Age), as.numeric(values$df$Trp_MM)))+
          geom_bar((aes(fill = values$df$Line)), stat="identity", position="dodge", show.legend=FALSE, size = 1)+ 
          xlab("Age, d")+ ylab("Trp intake (Mechanicistic), mg/d") +
          # geom_text(aes(label= round(values$df$Trp_MM, digits = 0)),position=position_dodge(width = 0.9), vjust=-0.2, size = 3) +
          theme(legend.position="bottom") + theme_set(theme_classic(base_size=13)) +
          scale_x_continuous(breaks = values$df$Age) 
        
      })
      
      
      output$plot6d <- renderPlotly({
        ggplot(values$df, aes(as.numeric(values$df$Age), as.numeric(values$df$Trp_EM)))+
          geom_bar((aes(fill = values$df$Line)), stat="identity", position="dodge", show.legend=FALSE, size = 1)+ 
          xlab("Age, d")+ ylab("Trp intake (Empirical), mg/d") +
         # geom_text(aes(label= round(values$df$Trp_EM, digits = 0)),position=position_dodge(width=0.9), vjust=-0.2, size = 3) +
          theme(legend.position="bottom") + theme_set(theme_classic(base_size=13)) +
          scale_x_continuous(breaks = values$df$Age)
        
      })   
          
     
      # 7 Grouth curve
      
      
      output$plot7 <- renderPlotly({
        ggplot(values$df, aes(as.numeric(values$df$Age), as.numeric(values$df$BW_pred)))+
          geom_point(color="green", size=1, show.legend=TRUE) +
          geom_point(aes(y = values$df$BW_NRC), colour="blue",size=1) +
          xlab("Age, d")+ ylab("Body weight, kg") +
          annotate("text", x= 20, y=4, label= "Estimate for the U.S. (blue) ", size=4) + 
          annotate("text", x= 20, y=3.6, label= "Estimate for the Brazil  (green) ", size=4) 
      })        
      
      
      # US
      output$plot7a <- renderPlotly({
        ggplot(values$df, aes(as.numeric(values$df$Age), as.numeric(values$df$BW_NRC)))+
          geom_bar((aes(fill = values$df$text8)), stat="identity", position="dodge", show.legend=FALSE, size = 1)+ 
          xlab("Age, d")+ ylab("Body weight, kg") +
          # geom_text(aes(label= round(values$df$Trp_EM, digits = 0)),position=position_dodge(width=0.9), vjust=-0.2, size = 3) +
          theme(legend.position="bottom") + theme_set(theme_classic(base_size=13)) +
          scale_x_continuous(breaks = values$df$Age) +
          annotate("text", x= 20, y=3.6, label= "Estimate for the U.S.", size=4)
        
      }) 
      
      # BR
      output$plot7b <- renderPlotly({
        ggplot(values$df, aes(as.numeric(values$df$Age), as.numeric(values$df$BW_pred)))+
          geom_bar((aes(fill = values$df$Line)), stat="identity", position="dodge", show.legend=FALSE, size = 1)+ 
          xlab("Age, d")+ ylab("Body weight, kg") +
          # geom_text(aes(label= round(values$df$Trp_EM, digits = 0)),position=position_dodge(width=0.9), vjust=-0.2, size = 3) +
          theme(legend.position="bottom") + theme_set(theme_classic(base_size=13)) +
          scale_x_continuous(breaks = values$df$Age) +
          annotate("text", x= 20, y=3.6, label= "Estimate for the Brazil", size=4)
        
      })   
      
     
      
      
      
            }

)
