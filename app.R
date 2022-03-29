# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#wrong default charts, dark mode check
#DRUN, RA9pf, remove some pointless stuff from summary? 

library(DT)
library(tidyverse)
library(shiny)
load("pbat.Rdata")
load("ppit.Rdata")
load("tbat.Rdata")
load("tpit.Rdata")

s_levels <- c("3","4","5","6","7","8","9","10","11","12","13","14",
             "15","16","17","18","19","20","21","22","23","24")

pbat <- pbat%>%
  mutate(season=factor(season,levels=s_levels),
        team=factor(team),
        player=factor(player))%>%
  arrange(desc(WhAT))
  
ppit <- ppit%>%
  mutate(season=factor(season,levels=s_levels),
         team=factor(team),
         player=factor(player))%>%
  arrange(desc(WhAT))
tbat <- tbat%>%
  mutate(season=factor(season,levels=s_levels),
         team=factor(team))%>%
  arrange(desc(RC))
tpit <- tpit%>%
    mutate(season=factor(season,levels=s_levels),
         team=factor(team))%>%
  arrange(desc(WhAT))


ui <- fluidPage(

    #title
    titlePanel("The Compendium of Blaseball Advanced Statistics"),

    # options to generate summary stats for players
    sidebarLayout(
            sidebarPanel(
              p(strong("Welcome")),
              p("The top table contains seasonal statistics for the selected data,
                and can be filtered and sorted by each column. Those filters are applied
                to the career table and downloads. View the linked glossary below for details
                about the calculation of WhAT and other statistics"),
              a(href="https://www.blaseball.wiki/w/SIBR:Sibrmetrics", "Glossary"),
              hr(),
            selectInput(
            "tablechoice",
            "Select Data",
            c("Player Offense","Player Pitching",
              "Team Offense","Team Defense"),
            selected="Player Offense"
          ),
            radioButtons(
              "chartchoice",
              "Select Chart:",
              c("Season","Career"),
              selected = "Season"
            ),
             uiOutput(
              "xaxis"
            ),
            uiOutput(
              "yaxis"
            ),
          selectInput(
            "columnchoice",
            "Select Columns",
            c("Standard","Advanced",
              "Combination","Give me everything"),
            selected="Advanced"
          ),
          numericInput(
            "PAmin",
            "Set minimum PA for Career Data",
            1000,
            0,
            20000,
            100
          )
          
        ),
    # Show a filtered dataTable and plot based on the filter, also download filtered dataset, and summary dataset
        mainPanel(
            plotOutput("swapplot", hover="hover_select"),
            dataTableOutput("hovertable"),
            hr(),
            h1("Seasonal Statistics"),
            dataTableOutput("battingtable"),
            downloadButton("downloadData", "Download"),
            hr(),
            br(),
            h1("Career Statistics"),
            dataTableOutput("summarytable"),
            downloadButton("downloadsummary"),
            hr(),
            br(),
            p(strong("by Sproutella, with data from SIBR"))
            )
        )
    )

server <- function(input, output, session) {
  # Reactive value for selected dataset
  in_react_frame <- reactive({
    switch(input$tablechoice,
           "Player Offense" = pbat,
           "Player Pitching" = ppit,
           "Team Offense"= tbat,
           "Team Defense"= tpit)
  })
  #reactive value for selected, columns needs to be reactive to dataset selected
  columnsel <- reactive({
    if (input$tablechoice=="Player Offense"){
    switch(input$columnchoice,
           "Standard" = c("player","season","team","league","PA","AB","R",
                          "H","1B","2B","3B","4B","HR","TB","SO","BB","HBP",
                          "SAC","RBI","SB","CS","BA","OBP","SLG","OPS","BABIP"),
           "Advanced" =c("player","season","team","league","PA","wOBA","wRC+",
                         "OPS+","wSB","BsR","WAA","WhAT","WhAT_PA","DRiP"),
           "Combination"=c("player","season","team","league","PA","AB","R",
                           "H","1B","2B","3B","4B","HR","TB","SO","BB","HBP",
                           "SAC","RBI","SB","CS","BA","OBP","SLG","OPS","BABIP",
                           "wOBA","wRC+","OPS+","wSB","BsR","WAA","WhAT",
                           "WhAT_PA","DRiP","HStars","BRStars"),
           "Give me everything"= names(in_react_frame()) 
           )
    }
    else if (input$tablechoice == "Player Pitching") {
      switch(input$columnchoice,
             "Standard" = c("player","season","team","league","G","SHO","IP",
                            "H","RA","HR","BB","SO","HBP","PA","ERA","FIP",
                            "WHIP","H/9","HR/9","BB/9","SO/9","SO/BB"),
             "Advanced" = c(
               "player","season","team","league","G","IP","PA","ERA-","FIP-",
               "ERA+","FIP+","WhAT","WhAT_IP","ERAAA","RA9","RA9avg","RA9opp",
               "DRiP/9","wBIPA","wBIPAteam","wBIPAopp","fBsR"),
             "Combination"=c("player","season","team","league","G","SHO","IP",
                             "H","RA","HR","BB","SO","HBP","PA","ERA","FIP",
                             "WHIP","H/9","HR/9","BB/9","SO/9","SO/BB",
                             "ERA-","FIP-","ERA+","FIP+","WhAT","WhAT_IP","ERAAA",
                             "RA9","RA9avg","RA9opp","DRiP/9","wBIPA","wBIPAteam",
                             "wBIPAopp","fBsR","PStars"),
             "Give me everything"= names(in_react_frame())
      )
    }
    else if (input$tablechoice == "Team Offense"){
      switch(input$columnchoice,
             "Standard" = c(
               "team","season","league","PA","AB","R","R/G","R/9",
               "H","1B","2B","3B","4B","HR","TB","SO","BB","HBP",
               "SAC","RBI","SB","CS","BA","OBP","SLG","OPS"),
             "Advanced" = c(
               "team","season","league","PA","RC","RC/G","wOBA","wRC+","OPS+",
               "wRC","BsR","wSB","BABIP","wBIPA","BB%","SO%"),
             "Combination"= c(
               "team","season","league","PA","AB","R","R/G","R/9",
               "H","1B","2B","3B","4B","HR","TB","SO","BB","HBP",
               "SAC","RBI","SB","CS","BA","OBP","SLG","OPS","RC","RC/G","wOBA",
               "wRC+","OPS+","wRC","BsR","wSB","BABIP","wBIPA","BB%","SO%"),
             "Give me everything"= names(in_react_frame())
      )
    }
    else if (input$tablechoice == "Team Defense") {
      switch(input$columnchoice,
             "Standard" = c("team","season","league","G","SHO","IP",
                            "H","RA","1B","2B","3B","4B","HR","BB","SO",
                            "SB","CS","DP","PA","DER","BABIP","ERA","FIP",
                            "WHIP","H/9","BA","OBP","SLG","OPS"),
             "Advanced" = c(
               "team","season","league","G","IP","PA","DRiP","DRiP/9","WhAT",
               "WhAT_IP","ERA-","FIP-","ERA+","FIP+","WHIP+","RA9opp","wBIPA","wBIPAopp",
               "fBsR","wSB","wGDP","UBR"),
             "Combination"= c("team","season","league","G","SHO","IP",
                              "H","RA","1B","2B","3B","4B","HR","BB","SO",
                              "SB","CS","DP","PA","DER","BABIP","ERA","FIP",
                              "WHIP","H/9","BA","OBP","SLG","OPS","DRiP",
                              "DRiP/9","WhAT", "WhAT_IP","ERA-","FIP-","ERA+","FIP+","WHIP+",
                              "RA9opp","wBIPA","wBIPAopp","fBsR","wSB","wGDP",
                              "UBR"),
             "Give me everything"= names(in_react_frame())
      )
    }
  })
  
    #filter reactive df by filters used in dataTable
    filtered_frame <-  reactive({
        frame <- req(in_react_frame())
        indexes <- req(input$battingtable_rows_all)
        frame[indexes,columnsel(),drop=FALSE]
    })
  #rounding rules, huh well this works kinda
    sig_list <- c("ERA","FIP","WHIP","BA","OBP","SLG","OPS","BABIP","wOBA","wRC+","OPS+","HStars","BRStars", "wBIPA","wBIPAteam","wBIPAopp",
                  "SO_9p","SO_BBp","SOrt_BBrt","SwC_Srt","OBP+","SLG+","BABIP+","wSB","wGDP","UBR","WAA_DRiP","BAT_RE","TTO",
                  "ISO","BB%","BB%+","SO%","SO%+","BB/SO","AB/HR","PF","ERA+","FIP+","WHIP+","E-F","H/9","HR/9","BB/9","SO/9",
                  "SO/BB","WhAT_IP","ERAAA","RA9","RA9avg","RA9opp","DRiP/9","fBsR","PStars","OHERA",
                  "DER","SW+MISS","S%","B%","CS%","SW%","F%","tfdRUNSsaved","twBIPAsaved",
                  "BAT%","C/S%","SW/S%","XBHIP%","BABIPp","BAT_Srt","BB_9p","C_SWING","C_Swrt","cfdRUNSsaved","cwBIPAsaved",
                  "defense_rating","DER+","dRPW","F_Srt","FDRA","fdRUNSsaved","FLYrt","Frtp","GROUNDrt","Grtp","wBIPAsaved",
                  "GtoF","GtoFp","H_9p","HR_9p","HR_H","HR_Hp","HRrt","HRrtp","RAA_TUN","wBIPAcomp","
                  wBIPAsaved","WhAT_TUN","wTUN","XBHIPpp","DRiP_PA","ERA-","FIP-")
    
    round_list <- c(
      "RBI","IP","R","BsR","wSB","wRC","BATR","WAA","WhAT","WhAT_PA","DRiP","wRAA","ER","RAA","RAT","RAT_TUN","OHRA","RA",
      "WhAT_DRiP","RS","R/G","R/9","RC","RC/G","DRUNS","HIUP", "HIP"
    )
    
    int_list <- c("PA","AB","H","1B","2B","3B","4B","HR","TB","SO","BB","HBP","SAC","RBI","SB","CS","GDP","GDPopps","HR4","HR5","Out",
                  "FC","TSO","TBB","SOCH","BBCH","SOMT","BBMT","BASEBB","G","SHO","STRIKES","BALLS","CALLED","SWINGING","FOULS","BATTED","BIP",
                  "DP","OUTS","PITCHES","T_RUN","U_O","R_SWEPT","SEC_Ent","SEC_Ex","BIPteam")
    
    char_list <- c("player","team","league","season","id","batter_team_id","pitcher_team_id")
    
    
    #create dataTable from reactive df
    output$battingtable <- renderDT(datatable(in_react_frame()[,columnsel(),drop=FALSE],
                            class="compact display nowrap",
                             filter = list(position="top",
                                           clear=FALSE,
                                           plain=T),
                             options = list(scrollX=TRUE,
                            columnDefs = list(
                              list(className = 'dt-center',targets = "_all")
                            ),
                            pageLength = 10
                             ),
                            rownames=FALSE
                          )%>%formatSignif(columnsel()%in% sig_list,digits=3)%>%
                            formatRound(columnsel()%in% round_list,digits=1)

    )
    
  
    
    #create download button for filtered reactive df
    output$downloadData <-downloadHandler(
            filename = function() {
                paste("data-", Sys.Date(), ".csv", sep="")
            },
            content = function(file) {
                write_csv(filtered_frame(), file)
            }
        )
 
#create reactive summary table from filtered reactive dataframe
    grouper <- reactive({
      switch(input$tablechoice,
             "Player Offense" = "player",
             "Player Pitching" = "player",
             "Team Offense"= "team",
             "Team Defense"= "team")
    })

    summary_data <- reactive({
        req(grouper())
        filtered_frame()%>%
           group_by(!!!rlang::syms(grouper()))%>%
            summarise(
            G={if("G"%in%names(.))sum(G, na.rm = TRUE)else NULL},
            SHO={if("SHO"%in%names(.))sum(SHO, na.rm = TRUE)else NULL},
            IP={if("IP"%in%names(.))sum(IP, na.rm = TRUE)else NULL},
            RA={if("RA"%in%names(.))sum(RA, na.rm = TRUE)else NULL},
            ER={if("ER"%in%names(.))sum(ER, na.rm = TRUE)else NULL},
            AB={if("AB"%in%names(.))sum(AB, na.rm = TRUE)else NULL},
            R={if("R"%in%names(.))sum(R, na.rm = TRUE)else NULL},
            H={if("H"%in%names(.))sum(H, na.rm = TRUE)else NULL},
            ERA={if("ERA"%in%names(.)) weighted.mean(ERA,PA,na.rm = TRUE) else NULL},
            FIP={if("FIP"%in%names(.)) weighted.mean(FIP,PA,na.rm = TRUE) else NULL},
            WHIP={if("WHIP"%in%names(.)) weighted.mean(WHIP,PA,na.rm = TRUE) else NULL},
            "H/9"={if("H/9"%in%names(.)) weighted.mean(.data[["H/9"]],PA,na.rm = TRUE) else NULL},
            "HR/9"={if("HR/9"%in%names(.)) weighted.mean(.data[["HR/9"]],PA,na.rm = TRUE) else NULL},
            "BB/9"={if("BB/9"%in%names(.)) weighted.mean(.data[["BB/9"]],PA,na.rm = TRUE) else NULL},
            "SO/9"={if("SO/9"%in%names(.)) weighted.mean(.data[["SO/9"]],PA,na.rm = TRUE) else NULL},
            "ERA-"={if("ERA-"%in%names(.)) weighted.mean(.data[["ERA-"]],PA,na.rm = TRUE) else NULL},
            "FIP-"={if("FIP-"%in%names(.)) weighted.mean(.data[["FIP-"]],PA,na.rm = TRUE) else NULL},
            "ERA+"={if("ERA+"%in%names(.)) weighted.mean(.data[["ERA+"]],PA,na.rm = TRUE) else NULL},
            "FIP+"={if("FIP+"%in%names(.)) weighted.mean(.data[["FIP+"]],PA,na.rm = TRUE) else NULL},
            "WHIP+"={if("WHIP+"%in%names(.)) weighted.mean(.data[["WHIP+"]],PA,na.rm = TRUE) else NULL},
            "E-F"={if("E-F"%in%names(.)) weighted.mean(.data[["E-F"]],PA,na.rm = TRUE) else NULL},
            HR={if("HR"%in%names(.))sum(HR, na.rm = TRUE)else NULL},
            SO={if("SO"%in%names(.))sum(SO, na.rm = TRUE)else NULL},
            BB={if("BB"%in%names(.))sum(BB, na.rm = TRUE)else NULL},
            HBP={if("HBP"%in%names(.))sum(HBP, na.rm = TRUE)else NULL},
            SAC={if("SAC"%in%names(.))sum(SAC, na.rm = TRUE)else NULL},
            RBI={if("RBI"%in%names(.))sum(RBI, na.rm = TRUE)else NULL},
            SB={if("SB"%in%names(.))sum(SB, na.rm = TRUE)else NULL},
            CS={if("CS"%in%names(.))sum(CS, na.rm = TRUE)else NULL},
            RS={if("RS"%in%names(.))sum(RS, na.rm = TRUE)else NULL},
            BA={if("BA"%in%names(.)) weighted.mean(BA,PA,na.rm = TRUE) else NULL},
            OBP={if("OBP"%in%names(.)) weighted.mean(OBP,PA,na.rm = TRUE) else NULL},
            SLG={if("SLG"%in%names(.)) weighted.mean(SLG,PA,na.rm = TRUE) else NULL},
            OPS={if("OPS"%in%names(.)) weighted.mean(OPS,PA,na.rm = TRUE) else NULL},
            "1B"={if("1B"%in%names(.))sum(.data[["1B"]], na.rm = TRUE)else NULL},
            "2B"={if("2B"%in%names(.))sum(.data[["2B"]], na.rm = TRUE)else NULL},
            "3B"={if("3B"%in%names(.))sum(.data[["3B"]], na.rm = TRUE)else NULL},
            "4B"={if("4B"%in%names(.))sum(.data[["4B"]], na.rm = TRUE)else NULL},
            TB={if("TB"%in%names(.))sum(TB, na.rm = TRUE)else NULL},
            BABIP={if("BABIP"%in%names(.)) weighted.mean(BABIP,PA,na.rm = TRUE) else NULL},
            wOBA={if("wOBA"%in%names(.)) weighted.mean(wOBA,PA,na.rm = TRUE) else NULL},
            "wRC+"={if("wRC+"%in%names(.)) weighted.mean(.data[["wRC+"]],PA,na.rm = TRUE) else NULL},
            "OPS+"={if("OPS+"%in%names(.)) weighted.mean(.data[["OPS+"]],PA,na.rm = TRUE) else NULL},
            wRC={if("wRC"%in%names(.))sum(wRC, na.rm = TRUE)else NULL},
            BATR={if("BATR"%in%names(.))sum(BATR, na.rm = TRUE)else NULL},
            BsR={if("BsR"%in%names(.))sum(BsR, na.rm = TRUE)else NULL},
            WAA={if("WAA"%in%names(.))sum(WAA, na.rm = TRUE)else NULL},
            "R/G"={if("R/G"%in%names(.)) weighted.mean(.data[["R/G"]],PA,na.rm = TRUE) else NULL},
            "R/9"={if("R/9"%in%names(.)) weighted.mean(.data[["R/9"]],PA,na.rm = TRUE) else NULL},
            RC={if("RC"%in%names(.))sum(RC, na.rm = TRUE)else NULL},
            "RC/G"={if("RC/G"%in%names(.)) weighted.mean(.data[["RC/G"]],PA,na.rm = TRUE) else NULL},
            DRiP_PA={if("DRiP_PA"%in%names(.)) weighted.mean(DRiP_PA,PA,na.rm = TRUE) else NULL},
            WhAT={if("WhAT"%in%names(.))sum(WhAT, na.rm = TRUE)else NULL},
            WhAT_PA={if("WhAT_PA"%in%names(.)) weighted.mean(WhAT_PA,PA,na.rm = TRUE) else NULL},
            WhAT_IP={if("WhAT_IP"%in%names(.)) weighted.mean(WhAT_IP,PA,na.rm = TRUE) else NULL},
            DRiP={if("DRiP"%in%names(.))sum(DRiP, na.rm = TRUE)else NULL},
            WhAT_DRiP={if("WhAT_DRiP"%in%names(.))sum(WhAT_DRiP, na.rm = TRUE)else NULL},
            ERAAA={if("ERAAA"%in%names(.)) weighted.mean(ERAAA,PA,na.rm = TRUE) else NULL},
            RA9={if("RA9"%in%names(.)) weighted.mean(RA9,PA,na.rm = TRUE) else NULL},
            RA9avg={if("RA9avg"%in%names(.)) weighted.mean(RA9avg,PA,na.rm = TRUE) else NULL},
            RA9opp={if("RA9opp"%in%names(.)) weighted.mean(RA9opp,PA,na.rm = TRUE) else NULL},
            "DRiP/9"={if("DRiP/9"%in%names(.)) weighted.mean(.data[["DRiP/9"]],PA,na.rm = TRUE) else NULL},
            wBIPA={if("wBIPA"%in%names(.)) weighted.mean(wBIPA,PA,na.rm = TRUE) else NULL},
            wBIPAteam={if("wBIPAteam"%in%names(.)) weighted.mean(wBIPAteam,PA,na.rm = TRUE) else NULL},
            wBIPAopp={if("wBIPAopp"%in%names(.)) weighted.mean(wBIPAopp,PA,na.rm = TRUE) else NULL},
            fBsR={if("fBsR"%in%names(.))sum(fBsR, na.rm = TRUE)else NULL},
            PStars={if("PStars"%in%names(.)) weighted.mean(PStars,PA,na.rm = TRUE) else NULL},
            OHRA={if("OHRA"%in%names(.))sum(OHRA, na.rm = TRUE)else NULL},
            OHERA={if("OHERA"%in%names(.)) weighted.mean(OHERA,PA,na.rm = TRUE) else NULL},
            DER={if("DER"%in%names(.)) weighted.mean(DER,PA,na.rm = TRUE) else NULL},
            HStars={if("HStars"%in%names(.)) weighted.mean(HStars,PA,na.rm = TRUE) else NULL},
            BRStars={if("BRStars"%in%names(.)) weighted.mean(BRStars,PA,na.rm = TRUE) else NULL},
            wRAA={if("wRAA"%in%names(.))sum(wRAA, na.rm = TRUE)else NULL},
            "OBP+"={if("OBP+"%in%names(.)) weighted.mean(.data[["OBP+"]],PA,na.rm = TRUE) else NULL},
            "SLG+"={if("SLG+"%in%names(.)) weighted.mean(.data[["SLG+"]],PA,na.rm = TRUE) else NULL},
            "BABIP+"={if("BABIP+"%in%names(.)) weighted.mean(.data[["BABIP+"]],PA,na.rm = TRUE) else NULL},
            wSB={if("wSB"%in%names(.))sum(wSB, na.rm = TRUE)else NULL},
            GDP={if("GDP"%in%names(.))sum(GDP, na.rm = TRUE)else NULL},
            GDPopps={if("GDPopps"%in%names(.))sum(GDPopps, na.rm = TRUE)else NULL},
            wGDP={if("wGDP"%in%names(.))sum(wGDP, na.rm = TRUE)else NULL},
            UBR={if("UBR"%in%names(.))sum(UBR, na.rm = TRUE)else NULL},
            WAA_DRiP={if("WAA_DRiP"%in%names(.))sum(WAA_DRiP, na.rm = TRUE)else NULL},
            BAT_RE={if("BAT_RE"%in%names(.))sum(BAT_RE, na.rm = TRUE)else NULL},
            TTO={if("TTO"%in%names(.)) weighted.mean(TTO,PA,na.rm = TRUE) else NULL},
            ISO={if("ISO"%in%names(.)) weighted.mean(ISO,PA,na.rm = TRUE) else NULL},
            "BB%"={if("BB%"%in%names(.)) weighted.mean(.data[["BB%"]],PA,na.rm = TRUE) else NULL},
            "BB%+"={if("BB%+"%in%names(.)) weighted.mean(.data[["BB%+"]],PA,na.rm = TRUE) else NULL},
            "SO%"={if("SO%"%in%names(.)) weighted.mean(.data[["SO%"]],PA,na.rm = TRUE) else NULL},
            "SO%+"={if("SO%+"%in%names(.)) weighted.mean(.data[["SO%+"]],PA,na.rm = TRUE) else NULL},
            STRIKES={if("STRIKES"%in%names(.))sum(STRIKES, na.rm = TRUE)else NULL},
            BALLS={if("BALLS"%in%names(.))sum(BALLS, na.rm = TRUE)else NULL},
            CALLED={if("CALLED"%in%names(.))sum(CALLED, na.rm = TRUE)else NULL},
            SWINGING={if("SWINGING"%in%names(.))sum(SWINGING, na.rm = TRUE)else NULL},
            FOULS={if("FOULS"%in%names(.))sum(FOULS, na.rm = TRUE)else NULL},
            BATTED={if("BATTED"%in%names(.))sum(BATTED, na.rm = TRUE)else NULL},
            "SW+MISS"={if("SW+MISS"%in%names(.)) weighted.mean(.data[["SW+MISS"]],PA,na.rm = TRUE) else NULL},
            "S%"={if("S%"%in%names(.)) weighted.mean(.data[["S%"]],PA,na.rm = TRUE) else NULL},
            "B%"={if("B%"%in%names(.)) weighted.mean(.data[["B%"]],PA,na.rm = TRUE) else NULL},
            "CS%"={if("CS%"%in%names(.)) weighted.mean(.data[["CS%"]],PA,na.rm = TRUE) else NULL},
            "SW%"={if("SW%"%in%names(.)) weighted.mean(.data[["SW%"]],PA,na.rm = TRUE) else NULL},
            "F%"={if("F%"%in%names(.)) weighted.mean(.data[["F%"]],PA,na.rm = TRUE) else NULL},
            "BAT%"={if("BAT%"%in%names(.)) weighted.mean(.data[["BAT%"]],PA,na.rm = TRUE) else NULL},
            "C/S%"={if("C/S%"%in%names(.)) weighted.mean(.data[["C/S%"]],PA,na.rm = TRUE) else NULL},
            "SW/S%"={if("SW/S%"%in%names(.)) weighted.mean(.data[["SW/S%"]],PA,na.rm = TRUE) else NULL},
            "DER+"={if("DER+"%in%names(.)) weighted.mean(.data[["DER+"]],PA,na.rm = TRUE) else NULL},
            BIP={if("BIP"%in%names(.))sum(BIP, na.rm = TRUE)else NULL},
            "XBHIP%"={if("XBHIP%"%in%names(.)) weighted.mean(.data[["XBHIP%"]],PA,na.rm = TRUE) else NULL},
            PF={if("PF"%in%names(.)) weighted.mean(PF,PA,na.rm = TRUE) else NULL},
            HR4={if("HR4"%in%names(.))sum(HR4, na.rm = TRUE)else NULL},
            HR5={if("HR5"%in%names(.))sum(HR5, na.rm = TRUE)else NULL},
            Out={if("Out"%in%names(.))sum(Out, na.rm = TRUE)else NULL},
            FC={if("FC"%in%names(.))sum(FC, na.rm = TRUE)else NULL},
            TSO={if("TSO"%in%names(.))sum(TSO, na.rm = TRUE)else NULL},
            TBB={if("TBB"%in%names(.))sum(TBB, na.rm = TRUE)else NULL},
            SOCH={if("SOCH"%in%names(.))sum(SOCH, na.rm = TRUE)else NULL},
            BBCH={if("BBCH"%in%names(.))sum(BBCH, na.rm = TRUE)else NULL},
            SOMT={if("SOMT"%in%names(.))sum(SOMT, na.rm = TRUE)else NULL},
            BBMT={if("BBMT"%in%names(.))sum(BBMT, na.rm = TRUE)else NULL},
            BASEBB={if("BASEBB"%in%names(.))sum(BASEBB, na.rm = TRUE)else NULL},
            BABIPp={if("BABIPp"%in%names(.)) weighted.mean(BABIPp,PA,na.rm = TRUE) else NULL},
            BAT_Srt={if("BAT_Srt"%in%names(.)) weighted.mean(BAT_Srt,PA,na.rm = TRUE) else NULL},
            BB_9p={if("BB_9p"%in%names(.)) weighted.mean(BB_9p,PA,na.rm = TRUE) else NULL},
            BIPteam={if("BIPteam"%in%names(.))sum(BIPteam, na.rm = TRUE)else NULL},
            C_SWING={if("C_SWING"%in%names(.)) weighted.mean(C_SWING,PA,na.rm = TRUE) else NULL},
            C_Swrt={if("C_Swrt"%in%names(.)) weighted.mean(C_Swrt,PA,na.rm = TRUE) else NULL},
            cfdRUNSsaved={if("cfdRUNSsaved"%in%names(.))sum(cfdRUNSsaved, na.rm = TRUE)else NULL},
            cwBIPAsaved ={if("cwBIPAsaved"%in%names(.))sum(cwBIPAsaved, na.rm = TRUE)else NULL},
            defense_rating={if("defense_rating"%in%names(.)) weighted.mean(defense_rating,PA,na.rm = TRUE) else NULL},
            DP={if("DP"%in%names(.))sum(DP, na.rm = TRUE)else NULL},
            F_Srt={if("F_Srt"%in%names(.)) weighted.mean(F_Srt,PA,na.rm = TRUE) else NULL},
            FDRA={if("FDRA"%in%names(.))sum(FDRA, na.rm = TRUE)else NULL},
            fdRUNSsaved={if("fdRUNSsaved"%in%names(.))sum(fdRUNSsaved, na.rm = TRUE)else NULL},
            FLY={if("FLY"%in%names(.))sum(FLY, na.rm = TRUE)else NULL},
            FLYrt={if("FLYrt"%in%names(.)) weighted.mean(FLYrt,PA,na.rm = TRUE) else NULL},
            Frtp={if("Frtp"%in%names(.)) weighted.mean(Frtp,PA,na.rm = TRUE) else NULL},
            GROUNDER={if("GROUNDER"%in%names(.))sum(GROUNDER, na.rm = TRUE)else NULL},
            GROUNDrt={if("GROUNDrt"%in%names(.)) weighted.mean(GROUNDrt,PA,na.rm = TRUE) else NULL},
            Grtp={if("Grtp"%in%names(.)) weighted.mean(Grtp,PA,na.rm = TRUE) else NULL},
            GtoF={if("GtoF"%in%names(.)) weighted.mean(GtoF,PA,na.rm = TRUE) else NULL},
            GtoFp={if("GtoFp"%in%names(.)) weighted.mean(GtoFp,PA,na.rm = TRUE) else NULL},
            H_9p={if("H_9p"%in%names(.)) weighted.mean(H_9p,PA,na.rm = TRUE) else NULL},
            HR_9p={if("HR_9p"%in%names(.)) weighted.mean(HR_9p,PA,na.rm = TRUE) else NULL},
            HR_H={if("HR_H"%in%names(.)) weighted.mean(HR_H,PA,na.rm = TRUE) else NULL},
            HR_Hp={if("HR_Hp"%in%names(.)) weighted.mean(HR_Hp,PA,na.rm = TRUE) else NULL},
            HRrt={if("HRrt"%in%names(.)) weighted.mean(HRrt,PA,na.rm = TRUE) else NULL},
            HRrtp={if("HRrtp"%in%names(.)) weighted.mean(HRrtp,PA,na.rm = TRUE) else NULL},
            OUTS={if("OUTS"%in%names(.))sum(OUTS, na.rm = TRUE)else NULL},
            PITCHES={if("PITCHES"%in%names(.))sum(PITCHES, na.rm = TRUE)else NULL},
            RAA={if("RAA"%in%names(.))sum(RAA, na.rm = TRUE)else NULL},
            RAT={if("RAT"%in%names(.))sum(RAT, na.rm = TRUE)else NULL},
            SO_9p={if("SO_9p"%in%names(.)) weighted.mean(SO_9p,PA,na.rm = TRUE) else NULL},
            SO_BBp={if("SO_BBp"%in%names(.)) weighted.mean(SO_BBp,PA,na.rm = TRUE) else NULL},
            SOrt_BBrt={if("SOrt_BBrt"%in%names(.)) weighted.mean(SOrt_BBrt,PA,na.rm = TRUE) else NULL},
            SwC_Srt={if("SwC_Srt"%in%names(.)) weighted.mean(SwC_Srt,PA,na.rm = TRUE) else NULL},
            tfdRUNSsaved={if("tfdRUNSsaved"%in%names(.))sum(tfdRUNSsaved, na.rm = TRUE)else NULL},
            twBIPAsaved={if("twBIPAsaved"%in%names(.))sum(twBIPAsaved, na.rm = TRUE)else NULL},
            U_O={if("U_O"%in%names(.))sum(U_O, na.rm = TRUE)else NULL},
            wBIPAcomp={if("wBIPAcomp"%in%names(.)) weighted.mean(wBIPAcomp,PA,na.rm = TRUE) else NULL},
            wBIPAsaved={if("wBIPAsaved"%in%names(.)) weighted.mean(wBIPAsaved,PA,na.rm = TRUE) else NULL},
            XBHIPpp={if("XBHIPpp"%in%names(.)) weighted.mean(XBHIPpp,PA,na.rm = TRUE) else NULL},
            R_SWEPT={if("R_SWEPT"%in%names(.))sum(R_SWEPT, na.rm = TRUE)else NULL},
            SEC_Ent={if("SEC_Ent"%in%names(.))sum(SEC_Ent, na.rm = TRUE)else NULL},
            SEC_Ex={if("SEC_Ex"%in%names(.))sum(SEC_Ex, na.rm = TRUE)else NULL},
            dRPW={if("dRPW"%in%names(.)) weighted.mean(dRPW,PA,na.rm = TRUE) else NULL},
            wTUN={if("wTUN"%in%names(.))sum(wTUN, na.rm = TRUE)else NULL},
            HIUP={if("HIUP"%in%names(.))sum(HIUP, na.rm = TRUE)else NULL},
            HIP={if("HIP"%in%names(.))sum(HIP, na.rm = TRUE)else NULL},
            RAA_TUN={if("RAA_TUN"%in%names(.))sum(RAA_TUN, na.rm = TRUE)else NULL},
            RAT_TUN={if("RAT_TUN"%in%names(.))sum(RAT_TUN, na.rm = TRUE)else NULL},
            WhAT_TUN={if("WhAT_TUN"%in%names(.))sum(WhAT_TUN, na.rm = TRUE)else NULL},
            T_RUN={if("T_RUN"%in%names(.))sum(T_RUN, na.rm = TRUE)else NULL},
            "SO/BB"={if("SO/BB"%in%names(.)) SO/BB else NULL},
            "AB/HR"={if("AB/HR"%in%names(.)) AB/HR else NULL},
            "BB/SO"={if("BB/SO"%in%names(.)) BB/SO else NULL},
            PA={if("PA"%in%names(.))sum(PA, na.rm = TRUE)else NULL},
            id={if("id"%in%names(.))first(id) else NULL}
            )%>%
          filter(PA>input$PAmin)
        })

    summary_data_sort<-  reactive({
      index <- match(names(filtered_frame()),names(summary_data())) 
      index <- index[!is.na(index)]
      summary_data()[index]
    })  
    
    #create output from summary reactive dataframe
    output$summarytable <- renderDT(
      datatable(
            summary_data_sort(),
            class="compact display nowrap",
            options = list(scrollX=TRUE,
                pageLength = 25,
                columnDefs = list(
                  list(className = 'dt-center', width = "25px", targets = "_all")
                )
            ),
            rownames=FALSE
       )%>%formatSignif(names(summary_data_sort())%in% sig_list,digits=3)%>%
        formatRound(names(summary_data_sort())%in% round_list,digits=1)
      
      )
    #create ui for plot, swap selectInput if chart when toggled
    output$xaxis <- renderUI({
      if (input$chartchoice == "Season") {
        selectInput("x", 
                    "Chart X Axis", 
                    choices=names(filtered_frame()),
                    selected="PA")
      } else {
        selectInput("x",
                    "Chart X Axis",
                    choices=names(summary_data()),
                    selected="PA") 
      }
    })
    output$yaxis <- renderUI({
      if (input$chartchoice == "Season") {
        selectInput("y",
                    "Chart Y Axis",
                    choices=names(filtered_frame()),
                    selected="WhAT")
      } else {
        selectInput("y",
                    "Chart Y Axis",
                    choices=names(summary_data()),
                    selected="WhAT") 
      }
    })
    #swap between 2 plot
    plotchoice <- reactive({
      switch(
        input$chartchoice,
        Season = filtered_frame(),
        Career = summary_data_sort()
      )
    })
    xinput <- reactive({
      as.name(input$x)
    })
    yinput <- reactive({
      as.name(input$y)
    })
    output$swapplot <- renderPlot({
      ggplot(req(plotchoice()), aes_(xinput(),yinput()))+geom_point(size=rel(2.5))
    })
    
    hovercols <- reactive({
    switch(
       input$chartchoice,
       Season = c(grouper(),"season",input$x,input$y),
       Career = c(grouper(),input$x,input$y)
          )
      })

    #create datatable from hoverinfo: need to fix how hovertable rounds data
    hoveredDT <- reactive({
      req(input$hover_select)
      frame <- nearPoints(plotchoice(), input$hover_select,maxpoints = 1)
      frame[,hovercols(),drop=FALSE]       
    })
    hover_list  <- reactive({
      hoveredDT()%>%
        select(where(is.numeric))%>%
        colnames()
    })
    
    output$hovertable <- renderDataTable(datatable(hoveredDT()[,hovercols(),drop=FALSE],
                                        class="compact display",
                                        options = list(
                                        searching=FALSE,
                                        paging=FALSE,
                                        columnDefs = list(
                                        list(className = 'dt-center', width = "25px", targets = "_all")
                                                   )
                                                  ),
                                                   rownames=FALSE
                          )%>%formatSignif(hover_list(),digits = 5)
    )
    

    output$downloadsummary <-downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write_csv(summary_data_sort(), file)
      }
    )
}  
# Run the application 
shinyApp(ui = ui, server = server)
