relevantVariables = function(dataset){
  
  variables                   = as.data.frame(matrix(nrow = nrow(dataset), ncol = 0))
  variables$ratingdiff        = dataset$ratingdiff
  variables$ratingClaydiff    = dataset$ratingClaydiff
  variables$ratingHarddiff    = dataset$ratingHarddiff
  variables$ratingGrassdiff   = dataset$ratingGrassdiff
  variables$ratingNotHarddiff = dataset$ratingNotHarddiff
  #variables$ratingBo3diff     = dataset$ratingBo3diff
  #variables$ratingBo5diff     = dataset$ratingBo5diff
  
  
  variables$glickodiff          <- dataset$glickodiff
  variables$glickoClaydiff      <- dataset$glickoClaydiff 
  variables$glickoHarddiff      <- dataset$glickoHarddiff
  variables$glickoGrassdiff     <- dataset$glickoGrassdiff 
  variables$glickoNotHarddiff   <- dataset$glickoNotHarddiff
  variables$glickoGamesdiff     <- dataset$glickoGamesdiff   
  variables$glickoHardGamesdiff <- dataset$glickoHardGamesdiff 
  variables$Bo5                 <- dataset$Bo5 
  variables$LeftieDiff          <- dataset$LeftieDiff
  
  variables$Round              <- dataset$Round
  
  
  #variables$DummyBo3                       = 1 - dataset$DummyBo5 
  #variables$DummyBo5                       = dataset$DummyBo5
  #variables$DummyBo5TimesAvgRatingdiff     = dataset$DummyBo5TimesAvgRatingdiff
  #variables$DummyBo5Timesratingdiff        = dataset$DummyBo5 * dataset$ratingdiff
  #variables$DummyBo5TimesratingHarddiff    = dataset$DummyBo5 * dataset$ratingHarddiff
  #variables$DummyBo5TimesratingGrassdiff   = dataset$DummyBo5 * dataset$ratingGrassdiff
  #variables$DummyBo5TimesratingClaydiff    = dataset$DummyBo5 * dataset$ratingClaydiff
  #variables$DummyBo5TimesratingNotHarddiff = dataset$DummyBo5 * dataset$ratingNotHarddiff
  #variables$DummyBo5TimesAvgRatingdiff2    = dataset$DummyBo5 * 1/2 * (dataset$ratingHarddiff + 
  #                                                                       dataset$ratingNotHarddiff )
   
  
    
  variables$RetiredDiff           = dataset$RetiredDiff
  variables$WalkoverDiff          = dataset$WalkoverDiff
  #variables$RetiredOrWalkoverDiff = dataset$RetiredOrWalkoverDiff

  variables$FatigueDiff         = dataset$FatigueDiff
  #variables$FatigueDiffTimesBo5 = dataset$FatigueDiffTimesBo5
  variables$HeadtoHeaddiff          = dataset$HeadtoHeaddiff
  #variables$LastHeadtoHead      = dataset$LastHeadtoHead
  
  #variables$HeadtoHeadPercentageWeightedsqN = dataset$HeadtoHeadPercentageWeightedsqN
  variables$HomeDiff                        = dataset$HomeDiff
  
  #variables$ThisBoxSkillDiff           = dataset$ThisBoxSkillDiff
  #variables$ThisBoxSkillDiffPlusScores = dataset$ThisBoxSkillDiffPlusScores
  #variables$ThisBoxSkillRatingMethod   = dataset$ThisBoxSkillRatingMethod
  
  #variables$recentGamesDiff = dataset$recentGamesDiff
  
  #variables$PSW = dataset$PSWthisplayer
  #variables$PSL = dataset$PSLthisplayer
  
  
  #variables$PSWthisplayer = dataset$PSW
  #variables$PSLthisplayer = dataset$PSL
  
  #variables$PSWImpr  = dataset$PSLthisplayer /(dataset$PSLthisplayer + dataset$PSWthisplayer)
  #variables$B365Impr = dataset$B365Lthisplayer /(dataset$B365Lthisplayer + dataset$B365Wthisplayer)
  
  variables$y = dataset$y

  
  #variables = variables[!is.na(variables$PSWImpr), ]
  #variables = variables[!is.na(variables$B365Impr), ]
  
  #variables = variables[!is.na(variables$COPercentGamesDiff), ]
  #variables = variables[!is.na(variables$COPercentGamesThisSurfaceDiff), ]
  #variables = variables[!is.na(variables$COPercentPointsThisSurfaceDiff), ]
  
  return(variables)
}
