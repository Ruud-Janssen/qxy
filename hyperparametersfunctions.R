regressorvariables = function(y, Data) {
  Nt = nrow(Data)
  
  x = as.data.frame(matrix(nrow = Nt, ncol = 0))
  
  x$ratingdiff = rep(NA, Nt)  
  
  x$Uncertainty = Data$Uncertainty
  x$Uncertainty2 = Data$Uncertainty2
  x$Surface = Data$Surface
  x$Court = Data$Court
  
  x$PSWthisplayer = rep(NA, Nt)
  x$PSLthisplayer = rep(NA, Nt)
  
  x$ratingClaydiff = rep(NA, Nt) 
  x$ratingHarddiff = rep(NA, Nt) 
  x$ratingGrassdiff = rep(NA, Nt) 
  x$ratingdiffCurrentSurface = rep(NA, Nt)
  
  
  #DummyBo5 = 1 if Bo5 = 1 and ratingdiff > 0, DummyBo5 = -1 if Bo5 = 1 and ratingdiff < 0,  
  #DummyBo5 = 0 if Bo5 = 0 or ratingdiff = 0 !!
  x$DummyBo5 = rep(NA, Nt)
  
  x$DummyBo5TimesRatingdiff_5SameSign= rep(NA, Nt)
  x$DummyBo5TimesRatingdiff = rep(NA, Nt) 
  x$DummyBo5TimesRatingdiff1_5SameSign= rep(NA, Nt)
  x$DummyBo5TimesRatingdiffSquaredSameSign= rep(NA, Nt)
  x$DummyBo5TimesRatingdiffthirdSameSign= rep(NA, Nt)
  
  x$DummyHard = rep(NA, Nt)
  x$DummyClay = rep(NA, Nt)
  x$DummyGrass = rep(NA, Nt)

  x$DummyBo5TimesAvgRatingdiff = rep(NA, Nt) 
  
  x$RetiredDiff = rep(NA,Nt)
  x$WalkoverDiff = rep(NA, Nt)
  x$RetiredOrWalkoverDiff = rep(NA, Nt)
  x$FatigueDiff = rep(NA,Nt)
  x$FatigueDiffTimesBo5 = rep(NA, Nt)
  
  x$HeadtoHead = rep(NA,Nt)
  x$HeadtoHeadPercentageWeightedsqN = rep(NA, Nt)
  x$LastHeadtoHead = rep(NA, Nt)
  
  x$HomeDiff = rep(NA, Nt)
  
  for (i in 1 : Nt){
    
    #winner's viewpoint
    if(y[i] == 1) {
      x$PSWthisplayer[i] = Data$PSW[i]
      x$PSLthisplayer[i] = Data$PSL[i]
      
      x$ratingdiff[i] = Data$Winner_rating[i] - Data$Loser_rating[i]
      x$ratingClaydiff[i] = Data$Winner_ratingClay[i] - Data$Loser_ratingClay[i]
      x$ratingHarddiff[i] = Data$Winner_ratingHard[i] - Data$Loser_ratingHard[i]
      x$ratingGrassdiff[i] = Data$Winner_ratingGrass[i] - Data$Loser_ratingGrass[i]
      x$RetiredDiff[i] = Data$Winner_retired_last_game[i] - Data$Loser_retired_last_game[i]
      x$WalkoverDiff[i] = Data$Winner_walkover_last_game[i] - Data$Loser_walkover_last_game[i]
      x$RetiredOrWalkoverDiff[i] = x$RetiredDiff[i] + x$WalkoverDiff[i]
      x$FatigueDiff[i] = Data$Winner_fatigue[i] - Data$Loser_fatigue[i]
      x$HeadtoHead[i] = Data$HeadtoHead[i]
      if(Data$HeadtoHeadMatches[i] != 0) {
        x$HeadtoHeadPercentageWeightedsqN[i] = ((Data$HeadtoHead[i] + 0.5 * Data$HeadtoHeadMatches[i]) / (Data$HeadtoHeadMatches[i]) - 0.5) * Data$HeadtoHeadMatches[i] ^ 0.5   
      } else {
        x$HeadtoHeadPercentageWeightedsqN[i] = 0  
      }
      x$LastHeadtoHead[i] = Data$LastHeadtoHead[i]
      x$HomeDiff[i] = Data$WinnerisHome[i] - Data$LoserisHome[i]
      
      #loser's viewpoint
    } else{
      x$PSWthisplayer[i] = Data$PSL[i]
      x$PSLthisplayer[i] = Data$PSW[i]
      x$ratingdiff[i] = -(Data$Winner_rating[i] - Data$Loser_rating[i])
      x$ratingClaydiff[i] = -(Data$Winner_ratingClay[i] - Data$Loser_ratingClay[i])
      x$ratingHarddiff[i] = -(Data$Winner_ratingHard[i] - Data$Loser_ratingHard[i])
      x$ratingGrassdiff[i] = -(Data$Winner_ratingGrass[i] - Data$Loser_ratingGrass[i])
      x$RetiredDiff[i] = Data$Loser_retired_last_game[i] - Data$Winner_retired_last_game[i]
      x$WalkoverDiff[i] = Data$Loser_walkover_last_game[i] - Data$Winner_walkover_last_game[i] 
      x$RetiredOrWalkoverDiff[i] = x$RetiredDiff[i] + x$WalkoverDiff[i]
      x$FatigueDiff[i] = Data$Loser_fatigue[i] - Data$Winner_fatigue[i]
      x$HeadtoHead[i] = -Data$HeadtoHead[i]
      if(Data$HeadtoHeadMatches[i] != 0) {
        x$HeadtoHeadPercentageWeightedsqN[i] = -(((Data$HeadtoHead[i] + 0.5 * Data$HeadtoHeadMatches[i]) / (Data$HeadtoHeadMatches[i]) - 0.5) * Data$HeadtoHeadMatches[i] ^ 0.5)   
      } else {
        x$HeadtoHeadPercentageWeightedsqN[i] = 0  
      }
      x$LastHeadtoHead[i] = -Data$LastHeadtoHead[i]
      x$HomeDiff[i] = Data$LoserisHome[i] -  Data$WinnerisHome[i] 
    }
    
    
    surface = Data$Surface[i]
    
    if(surface == "Clay") {
      x$ratingdiffCurrentSurface[i] = x$ratingClaydiff[i]
    } else if(surface == "Hard") {
      x$ratingdiffCurrentSurface[i] = x$ratingHarddiff[i]
    } else if(surface == "Grass") {
      x$ratingdiffCurrentSurface[i] = x$ratingGrassdiff[i]
    } 
    
    if(Data$Best.of[i] == 5){
      x$DummyBo5TimesRatingdiff[i] = x$ratingdiff[i]
      
      x$DummyBo5TimesAvgRatingdiff[i] = 0.5 * (x$ratingdiff[i] + x$ratingdiffCurrentSurface[i])
      
      if(x$ratingdiff[i] > 0) {
        x$DummyBo5[i] = 1
      } else if(x$ratingdiff[i] < 0) {
        x$DummyBo5[i] = -1
      } else {
        x$DummyBo5[i] = 0
      }

      
    } else {
      x$DummyBo5TimesRatingdiff[i] = 0
      x$DummyBo5TimesAvgRatingdiff[i] = 0
      x$DummyBo5[i] = 0
    }
    
    if(x$DummyBo5TimesRatingdiff[i] > 0) {
      x$DummyBo5TimesRatingdiff_5SameSign[i] = x$DummyBo5TimesRatingdiff[i] ^ 0.5
      x$DummyBo5TimesRatingdiffSquaredSameSign[i] = (x$DummyBo5TimesRatingdiff[i]) ^ 2
      x$DummyBo5TimesRatingdiff1_5SameSign[i] = x$DummyBo5TimesRatingdiff[i] ^ 1.5
      x$DummyBo5TimesRatingdiffthirdSameSign[i] = x$DummyBo5TimesRatingdiff[i] ^ 3
    } else {
      x$DummyBo5TimesRatingdiffSquaredSameSign[i] = - (x$DummyBo5TimesRatingdiff[i]) ^ 2
      x$DummyBo5TimesRatingdiff_5SameSign[i] = -(abs(x$DummyBo5TimesRatingdiff[i])) ^ 0.5
      x$DummyBo5TimesRatingdiff1_5SameSign[i] = -(abs(x$DummyBo5TimesRatingdiff[i])) ^ 1.5
      x$DummyBo5TimesRatingdiffthirdSameSign[i] = -(abs(x$DummyBo5TimesRatingdiff[i])) ^ 3
    }
    
    x$FatigueDiffTimesBo5[i] = x$DummyBo5[i] * x$FatigueDiff[i]
    
    if(surface == "Clay") {
      x$DummyClay[i] = 1
      x$DummyGrass[i] = 0
      x$DummyHard[i] = 0
    } else if(surface == "Grass") {
      x$DummyClay[i] = 0
      x$DummyGrass[i] = 1
      x$DummyHard[i] = 0
    } else if(surface == "Hard") {
      x$DummyClay[i] = 0
      x$DummyGrass[i] = 0
      x$DummyHard[i] = 1
    }
  }
  return(x)
}

cvpredictions = function(results, Reg, xcv, ycv, q) {
  
  
  cvpred = predict(Reg, xcv, type = "response")
  
  Npred = length(cvpred)
  
  bets = as.data.frame(matrix(nrow = Npred, ncol = 0))
  bets$result = rep(0, Npred)
  bets$bet = rep(0, Npred)
  bets$br = rep(1, Npred)
  
  #Unfortunately, there appears to be a big upset in the beginning of the dataset, causing bad ROI results
  for(i in 1 : Npred) {
    winexpectation = cvpred[i]
    lossexpectation = 1 - cvpred[i]
    if(!is.na(xcv$PSLthisplayer[i]) & !is.na(xcv$PSWthisplayer[i])){# & results$Br[q] > 0) {
      if(winexpectation * xcv$PSWthisplayer[i] - 1 > 0.05) {
        results$Nrbets[q] = results$Nrbets[q] + 1
        bets$bet[i] = (winexpectation * xcv$PSWthisplayer[i] - 1) / (xcv$PSWthisplayer[i] - 1)
        #WHY DOES THIS NOT WORK 
        #bets$bet[i] = 1/4 * results$Br[q] * bets$bet[i]
        #bet won
        if(ycv[i] == 1 ) {
          bets$result[i] = bets$bet[i]*(xcv$PSWthisplayer[i] - 1) 
          #bet loss
        } else {
          bets$result[i] = -bets$bet[i]
        }
        
      } else if(lossexpectation * xcv$PSLthisplayer[i] - 1 > 0.05) {
        results$Nrbets[q] = results$Nrbets[q] + 1
        bets$bet[i] = (lossexpectation * xcv$PSLthisplayer[i] - 1) / (xcv$PSLthisplayer[i] - 1)
        #WHY DOES THIS NOT WORK? 
        #bets$bet[i] =  1/4 * results$Br[q] * bets$bet[i]
        #bet won
        if(ycv[i] == 0){
          bets$result[i] = bets$bet[i] * (xcv$PSLthisplayer[i] - 1)
        } else {
          bets$result[i] = -bets$bet[i]
        } 
      }
      
    }
    if( i > 1) {
      bets$br[i] = bets$br[i - 1] + bets$result[i]
    } else {
      bets$br[i] = 1 + bets$result[i]
    }
  }
  
  results$LogLossOutOfSample[q] = LogLoss(cvpred, ycv)
  
  results$Br[q] = bets$br[Npred]
  results$ROI[q] = sum(bets$result) / sum(bets$bet)
  
  results$
    ple[q] = LogLoss(cvpred, ycv)
  
  clay = (xcv$Surface == "Clay")
  if(sum(clay) > 0) {
    results$BrClay[q] = bets$br[Npred]
    results$LogLossOutOfSampleClay[q] = LogLoss(cvpred[clay], ycv[clay])
  }
  
  grass = (xcv$Surface == "Grass")
  if(sum(grass) > 0) {
    results$BrGrass[q] = bets$br[Npred]
    results$LogLossOutOfSampleGrass[q] = LogLoss(cvpred[grass], ycv[grass])
  }
  
  
  hard = (xcv$Surface == "Hard")
  if(sum(hard) > 0) {
    results$BrHard[q] = bets$br[Npred]
    results$LogLossOutOfSampleHard[q] = LogLoss(cvpred[hard], ycv[hard])
  }
  
  return(results)
}


