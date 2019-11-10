#Cole Adams
#Pitcher Scrim 2018-2019 Shiny App
#2/12/19

library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(ggplot2)
library(readr)
library(knitr)
library(xtable)
library(DT)
library(yaml)

#Load In All Data
baseball2018 <- read.csv(file = 'ecuP.csv', header = TRUE, row.names = NULL)

#Pitcher plays for NC State
ncsuPBaseball2018 <- baseball2018 %>% filter(PitcherTeam == "ECU_PIR")
#Batter plays for NC State
ncsuBBaseball2018 <- baseball2018 %>% filter(BatterTeam == "NOR_WOL" | BatterTeam == "NOR_WOL2")

#Renaming Set-Up
shinySet <- ncsuBBaseball2018
pshinySet <- ncsuPBaseball2018

#Strikezone Plot Set-Up
topKzone <- 3.4
botKzone <- 1.75
inKzone <- -.71
outKzone <- .71
kZone <- data.frame(
  x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
  y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
)

# Server
shinyServer(function(input, output,session){
  #SERVER DATA 
  # Reactive Inital Set-Up
  
  pitch <- reactive(if(input$pitchType == 'All'){return(c('Fastball', 'Changeup', 'Curveball'))} else{return(input$pitchType)})
  pitcher <- reactive(if(input$iPit == 'Team'){return(c('Agnos, Jake', 'Burleson, Alec', 'Kuchmaner, Jake', 'Lanier, Sam', 'Smith, Tyler', 'Voliva, Evan', 'Williams, Gavin'))} else{return(input$iPit)})
  splits <- reactive(if(input$splits == 'All'){return(c('Left', 'Right'))} else{return(input$splits)})
  
  #Reactive Data Set-Up
  playerPitchDat <- reactive(pshinySet[pshinySet$TruePitch %in% pitch() & pshinySet$Pitcher %in% pitcher() & pshinySet$BatterSide %in% splits(),])
  playerPitDat <- reactive(pshinySet[pshinySet$Pitcher %in% pitcher() & pshinySet$BatterSide %in% splits(),])
    
    
## Plot Set-Up

# Strike Zone Plot
output$pitcherPlot <- renderPlotly(
{
  strikeZone <- ggplot(filter(playerPitDat(), Strikes == 2), aes(x=PlateLocSide, y=PlateLocHeight)) + geom_point(aes(color = TruePitch), size=1.5) +  geom_path(data=kZone, aes(x,y),  lwd=.5, col="black") + scale_x_continuous(limits = c(-2, 2)) +scale_y_continuous(limits = c(0, 4)) + labs(color = "Pitch Type")
  pstrikeZone <- ggplotly(strikeZone)
  pstrikeZone
})

# Spin Rate Plot
output$spinPlot <- renderPlotly(
  {
    spinZone <- ggplot(playerPitchDat(), aes(x = 1:length(X), y = SpinRate)) + geom_point(aes(color =  factor(BatterTeam))) + geom_smooth(color = '#E2493E', se = FALSE) + xlab('Pitch') + labs(color = "Opponent")
    pspinZone <- ggplotly(spinZone) 
    pspinZone
  }
)

# Velo Plot
output$veloPlot <- renderPlotly(
  {
    spinZone <- ggplot(playerPitchDat(), aes(x = 1:length(X), y = RelSpeed)) + geom_point(aes(color =  factor(BatterTeam))) + geom_smooth(color = '#E2493E', se = FALSE) + xlab('Pitch') + labs(color = "Opponent")
    pspinZone <- ggplotly(spinZone)
    pspinZone
  }
)

# Stat Creation
  stats <- reactive({
  pvelo <- round(summary((playerPitchDat())$RelSpeed)[['Mean']],2) 

  pexit <- round(summary((playerPitchDat())$ExitSpeed)[['Mean']],2) 
  
  swing <- round((summary((playerPitchDat())$PitchCall)[['FoulBall']] + summary((playerPitchDat())$PitchCall)[['InPlay']] + summary((playerPitchDat())$PitchCall)[['StrikeSwinging']]) / (summary((playerPitchDat())$PitchCall)[['BallCalled']] + summary((playerPitchDat())$PitchCall)[['FoulBall']] + summary((playerPitchDat())$PitchCall)[['HitByPitch']] + summary((playerPitchDat())$PitchCall)[['InPlay']] + summary((playerPitchDat())$PitchCall)[['StrikeCalled']] + summary((playerPitchDat())$PitchCall)[['StrikeSwinging']] ),2)

  contact <- round((summary((playerPitchDat())$PitchCall)[['FoulBall']] + summary((playerPitchDat())$PitchCall)[['InPlay']]) / (summary((playerPitchDat())$PitchCall)[['FoulBall']] + summary((playerPitchDat())$PitchCall)[['InPlay']] + summary((playerPitchDat())$PitchCall)[['StrikeSwinging']]),2)

  pzone <- round(summary((playerPitchDat())$SoB)[['Strike']] / (summary((playerPitchDat())$SoB)[['Strike']] + summary((playerPitchDat())$SoB)[['Ball']]),2)

  pthrown <- length(playerPitchDat()$TruePitch)
  
  pthrownpct <- round(length(playerPitchDat()$TruePitch) / length(playerPitDat()$TruePitch),2)
  
  pext <- round(summary((playerPitchDat())$Extension)[['Mean']],2)
  
  pspinrate <- round(summary((playerPitchDat())$SpinRate)[['Mean']],2)
  
  phorzbreak <- round(summary((playerPitchDat())$HorzBreak)[['Mean']],2)
  
  pvertbreak <- round(summary((playerPitchDat())$VertBreak)[['Mean']],2)
  
  pheight <- round(summary((playerPitchDat())$RelHeight)[['Mean']],2)
    
  ppfxx <- round(summary((playerPitchDat())$pfxx)[['Mean']],2)
  
  ppfxz <- round(summary((playerPitchDat())$pfxz)[['Mean']],2)
  
  avg <- (summary((playerPitchDat())$PlayResult)[['Single']]+
          summary((playerPitchDat())$PlayResult)[['Double']] + 
          summary((playerPitchDat())$PlayResult)[['Triple']] + 
          summary((playerPitchDat())$PlayResult)[['HomeRun']]) /  
         (summary((playerPitchDat())$PlayResult)[['Single']] + 
          summary((playerPitchDat())$PlayResult)[['Double']] +
          summary((playerPitchDat())$PlayResult)[['Triple']] + 
          summary((playerPitchDat())$PlayResult)[['HomeRun']] +
          summary((playerPitchDat())$PlayResult)[['Out']])
  
  obp <- (summary((playerPitchDat())$PlayResult)[['Single']]+
          summary((playerPitchDat())$PlayResult)[['Double']] + 
          summary((playerPitchDat())$PlayResult)[['Triple']] + 
          summary((playerPitchDat())$PlayResult)[['HomeRun']] +
          summary((playerPitchDat())$PlayResult)[['Walk']] +
          summary((playerPitchDat())$PlayResult)[['HitByPitch']]) /  
         (summary((playerPitchDat())$PlayResult)[['Single']] +
          summary((playerPitchDat())$PlayResult)[['Double']] + 
          summary((playerPitchDat())$PlayResult)[['Triple']] + 
          summary((playerPitchDat())$PlayResult)[['HomeRun']] +
          summary((playerPitchDat())$PlayResult)[['Walk']] +
          summary((playerPitchDat())$PlayResult)[['HitByPitch']] +
          summary((playerPitchDat())$PlayResult)[['Sacrifice']] +
          summary((playerPitchDat())$PlayResult)[['Out']])
  
  slug <- (summary((playerPitchDat())$PlayResult)[['Single']] * 1 +
           summary((playerPitchDat())$PlayResult)[['Double']] * 2 + 
           summary((playerPitchDat())$PlayResult)[['Triple']] * 3 + 
           summary((playerPitchDat())$PlayResult)[['HomeRun']] * 4) /  
          (summary((playerPitchDat())$PlayResult)[['Single']] +
           summary((playerPitchDat())$PlayResult)[['Double']] + 
           summary((playerPitchDat())$PlayResult)[['Triple']] + 
           summary((playerPitchDat())$PlayResult)[['HomeRun']] +
           summary((playerPitchDat())$PlayResult)[['Out']])
  
  ISO <- paste(sub("^(-?)0.", "\\1.", sprintf("%.3f", (slug - avg))))
  
  tripSlash <- paste(sub("^(-?)0.", "\\1.", sprintf("%.3f", avg)),sub("^(-?)0.", "\\1.", sprintf("%.3f", obp)),sub("^(-?)0.", "\\1.", sprintf("%.3f", slug)), sep = '/')
  
  #Establishment of wOBA Formula
  wobaForm <- (summary((playerPitchDat())$PlayResult)[['Single']] * .878 +
                summary((playerPitchDat())$PlayResult)[['Double']] * 1.242	 +
                summary((playerPitchDat())$PlayResult)[['Triple']] * 1.569 +
                summary((playerPitchDat())$PlayResult)[['HomeRun']] * 2.015 + 
                summary((playerPitchDat())$PlayResult)[['Walk']] * .691 +
                summary((playerPitchDat())$PlayResult)[['HitByPitch']] * .721) /  
               (length(playerPitchDat()$PlayResult) -
               (summary((playerPitchDat())$PlayResult)[['Error']] +
                summary((playerPitchDat())$PlayResult)[["NA's"]]))
   
  woba <- paste(sub("^(-?)0.", "\\1.", sprintf("%.3f", wobaForm)))
    
  KPct <- round(summary((playerPitchDat())$KorBB)[['Strikeout']] /
               (summary((playerPitchDat())$KorBB)[['Strikeout']] +
                summary((playerPitchDat())$KorBB)[['InPlay']] +
                summary((playerPitchDat())$KorBB)[['Walk']]), 2)
          
  
  BBPct <- round(summary((playerPitchDat())$KorBB)[['Walk']] /
                (summary((playerPitchDat())$KorBB)[['Strikeout']] +
                 summary((playerPitchDat())$KorBB)[['InPlay']] +
                 summary((playerPitchDat())$KorBB)[['Walk']]), 2)
  
  swstr <- round(summary(((playerPitchDat())$PitchCall))[['StrikeSwinging']] /
                (length(playerPitchDat()$PitchCall)), 2)
  
  calledstrike <- round(summary(((playerPitchDat())$PitchCall))[['StrikeCalled']] /
                 (length(playerPitchDat()$PitchCall)), 2)

  oswing <-  round((
    table(playerPitchDat()$SoB, playerPitchDat()$PitchCall)[['Ball', 'StrikeSwinging']] +
      table(playerPitchDat()$SoB, playerPitchDat()$PitchCall)[['Ball', 'FoulBall']] +
      table(playerPitchDat()$SoB, playerPitchDat()$PitchCall)[['Ball', 'InPlay']]) 
    /
      (length(subset(playerPitchDat()$SoB,playerPitchDat()$SoB == 'Ball'))), 2)
  
  zswing <-  round((
    table(playerPitchDat()$SoB, playerPitchDat()$PitchCall)[['Strike', 'StrikeSwinging']] +
      table(playerPitchDat()$SoB, playerPitchDat()$PitchCall)[['Strike', 'FoulBall']] +
      table(playerPitchDat()$SoB, playerPitchDat()$PitchCall)[['Strike', 'InPlay']]) 
    /
      (length(subset(playerPitchDat()$SoB,playerPitchDat()$SoB == 'Strike'))), 2)
  
  ocontact <-  round((
    table(playerPitchDat()$SoB, playerPitchDat()$PitchCall)[['Ball', 'FoulBall']] +
      table(playerPitchDat()$SoB, playerPitchDat()$PitchCall)[['Ball', 'InPlay']]) 
    /
      (table(playerPitchDat()$SoB, playerPitchDat()$PitchCall)[['Ball', 'FoulBall']] +
         table(playerPitchDat()$SoB, playerPitchDat()$PitchCall)[['Ball', 'InPlay']]+
         table(playerPitchDat()$SoB, playerPitchDat()$PitchCall)[['Ball', 'StrikeSwinging']]), 2)
  
  zcontact <-  round((
    table(playerPitchDat()$SoB, playerPitchDat()$PitchCall)[['Strike', 'FoulBall']] +
      table(playerPitchDat()$SoB, playerPitchDat()$PitchCall)[['Strike', 'InPlay']]) 
    /
      (table(playerPitchDat()$SoB, playerPitchDat()$PitchCall)[['Strike', 'FoulBall']] +
         table(playerPitchDat()$SoB, playerPitchDat()$PitchCall)[['Strike', 'InPlay']]+
         table(playerPitchDat()$SoB, playerPitchDat()$PitchCall)[['Strike', 'StrikeSwinging']]), 2)
  
  #Ball In Play Creation
  DB <- round((summary((playerPitchDat())$BBType)[['DB']] /
              (length(playerPitchDat()$BBType) - summary((playerPitchDat())$BBType)[["NA's"]])), 2)
  
  GB <- round((summary((playerPitchDat())$BBType)[['GB']] /
              (length(playerPitchDat()$BBType) - summary((playerPitchDat())$BBType)[["NA's"]])), 2)
  
  LD <- round((summary((playerPitchDat())$BBType)[['LD']] /
              (length(playerPitchDat()$BBType) - summary((playerPitchDat())$BBType)[["NA's"]])), 2)
  
  HD <- round((summary((playerPitchDat())$BBType)[['HD']] /
              (length(playerPitchDat()$BBType) - summary((playerPitchDat())$BBType)[["NA's"]])), 2)
  
  FB <- round((summary((playerPitchDat())$BBType)[['FB']] /
              (length(playerPitchDat()$BBType) - summary((playerPitchDat())$BBType)[["NA's"]])), 2)
  
  PU <- round((summary((playerPitchDat())$BBType)[['PU']] /
              (length(playerPitchDat()$BBType) - summary((playerPitchDat())$BBType)[["NA's"]])), 2)
  ### NOT ACTIVE
  # db <- 0
  # 
  # 
  # for(i in 1:length(playerPitchDat() %>% filter(!(is.na(Angle)))))
  # {
  #   print(i)
  #   if((playerPitchDat() %>% filter(!(is.na(Angle))))[i, 'Angle'] < 0)
  #   {
  #     db <- db + 1
  #   }
  # }
  # print(db)
  
  #Table Set-Up
  statsdf <- list(pthrown = pthrown, pthrownpct = pthrownpct, pvelo = pvelo, pext = pext, pspinrate = pspinrate, phorzbreak = phorzbreak, pvertbreak = pvertbreak, pzone = pzone, pexit = pexit, pheight = pheight, ppfxx = ppfxx, ppfxz = ppfxz, tripSlash = tripSlash, ISO = ISO, woba = woba, KPct = KPct, BBPct = BBPct, swstr = swstr, swing = swing, contact = contact, calledstrike = calledstrike, oswing = oswing, ocontact = ocontact, zswing = zswing, zcontact = zcontact, DB = DB, GB = GB, LD = LD, HD = HD, FB = FB, PU = PU)
  statsdf
  })
  
  
  output$mytable<- renderTable({
    a <- stats()
    table <- data.frame(Stat = c('# Thrown', '% Thrown', 'Ave. Velo', 'Extension', 'Ave. Spin', 'Horz. Break', 'Vert. Break', 'Zone %', 'Exit Velo', 'Release Height', 'pfxx', 'pfxz', 'Slash', "ISO", 'wOBA', 'K%', 'BB%','SwStr %', 'Swing %', 'Contact %', "Called Strike %", 'O-Swing %', 'DB%', 'GB%', 'LD%', 'HD%', 'FB%', 'PU%'), Values = c(a$pthrown, a$pthrownpct, a$pvelo, a$pext, a$pspinrate, a$phorzbreak, a$pvertbreak, a$pzone, a$pexit, a$pheight, a$ppfxx, a$ppfxz, a$tripSlash, a$ISO, a$woba, a$KPct, a$BBPct, a$swstr, a$swing, a$contact, a$calledstrike, a$oswing, a$DB, a$GB, a$LD, a$HD, a$FB, a$PU))
  
  })
  
############################
############################
############################
  
  #Large Plot Initial Data Set-Up
  zpitch <- reactive(if(input$zpitchType == 'All'){return(c('Fastball', 'Changeup', 'Curveball'))} else{return(input$zpitchType)})
  zpitcher <- reactive(if(input$zPit == 'All'){return(c('Agnos, Jake', 'Burleson, Alec', 'Kuchmaner, Jake', 'Lanier, Sam', 'Smith, Tyler', 'Voliva, Evan', 'Williams, Gavin'))} else{return(input$zPit)})
  zsplits <- reactive(if(input$zsplits == 'All'){return(c('Left', 'Right'))} else{return(input$zsplits)})  
  
  zplayerPitchDat <- reactive(pshinySet[pshinySet$TruePitch %in% zpitch() & pshinySet$Pitcher %in% zpitcher() & pshinySet$BatterSide %in% zsplits(),])
  zplayerPitDat <- reactive(pshinySet[pshinySet$Pitcher %in% zpitcher() & pshinySet$BatterSide %in% zsplits(),])
  
  #Large Plot
  output$zPlots <- renderPlotly(
    {
      zstrikeZone <- ggplot(zplayerPitchDat(), aes(x=PlateLocSide, y=PlateLocHeight)) + geom_point(aes(color = TruePitch), size=1.5) +  geom_path(data=kZone, aes(x,y),  lwd=.5, col="black") + scale_x_continuous(limits = c(-2, 2)) +scale_y_continuous(limits = c(0, 4)) + labs(color = "Pitch Type")
      zpstrikeZone <- ggplotly(zstrikeZone) %>% layout(height = 800, width = 1000)
      zpstrikeZone
    })
  
  ### NOT ACTIVE
  # output$zPlots2 <- renderPlotly(
  #   {
  #     zstrikeZone2 <- ggplot(filter(zplayerPitchDat(), Strikes == 2), aes(x=PlateLocSide, y=PlateLocHeight)) + geom_point(aes(color = TruePitch), size=1.5) +  geom_path(data=kZone, aes(x,y),  lwd=.5, col="black") + scale_x_continuous(limits = c(-2, 2)) +scale_y_continuous(limits = c(0, 4)) + labs(color = "Pitch Type")
  #     zpstrikeZone2 <- ggplotly(zstrikeZone2) %>% layout(height = 800, width = 1000)
  #     zpstrikeZone2
  #   })

})    