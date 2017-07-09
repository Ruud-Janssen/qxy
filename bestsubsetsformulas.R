relevantVariables = function(dataset){
  
  variables = as.data.frame(matrix(nrow = nrow(dataset), ncol = 0))
  variables$ratingdiff = dataset$ratingdiff
  variables$ratingClaydiff = dataset$ratingClaydiff
  variables$ratingHarddiff = dataset$ratingHarddiff
  variables$ratingGrassdiff = dataset$ratingGrassdiff
  variables$ratingNotHarddiff = dataset$ratingNotHarddiff
  
  variables$DummyBo5 = dataset$DummyBo5
  variables$DummyBo5TimesAvgRatingdiff = dataset$DummyBo5TimesAvgRatingdiff
  variables$DummyBo5Timesratingdiff = dataset$DummyBo5 * dataset$ratingdiff
  variables$DummyBo5TimesratingHarddiff = dataset$DummyBo5 * dataset$ratingHarddiff
  variables$DummyBo5TimesratingGrassdiff = dataset$DummyBo5 * dataset$ratingGrassdiff
  variables$DummyBo5TimesratingClaydiff = dataset$DummyBo5 * dataset$ratingClaydiff
  variables$DummyBo5TimesratingNotHarddiff = dataset$DummyBo5 * dataset$ratingNotHarddiff
  variables$DummyBo5TimesAvgRatingdiff2 = dataset$DummyBo5 * 1/2 * (dataset$ratingHarddiff + 
                                                                   dataset$ratingNotHarddiff )
    
  variables$RetiredDiff = dataset$RetiredDiff
  variables$WalkoverDiff = dataset$WalkoverDiff
  variables$RetiredOrWalkoverDiff = dataset$RetiredOrWalkoverDiff

  variables$FatigueDiff = dataset$FatigueDiff
  variables$FatigueDiffTimesBo5 = dataset$FatigueDiffTimesBo5
  variables$HeadtoHead = dataset$HeadtoHead
  variables$LastHeadtoHead = dataset$LastHeadtoHead
  
  variables$HeadtoHeadPercentageWeightedsqN = dataset$HeadtoHeadPercentageWeightedsqN
  variables$HomeDiff = dataset$HomeDiff
  
  variables$ThisBoxSkillDiff = dataset$ThisBoxSkillDiff
  variables$ThisBoxSkillDiffPlusScores = dataset$ThisBoxSkillDiffPlusScores
  
  variables$y = dataset$y
  #variables$PSWImpr = dataset$PSLthisplayer /(dataset$PSLthisplayer + dataset$PSWthisplayer)
  
  return(variables)
}
  
removeUncertainMatches = function(x, edge){
  index_keep = (x$Uncertainty < edge)
  x = x[index_keep, ]
  return(x)
}

getXThisSurface = function(x, surface){
  indexSurface = (x$Surface == surface)
  x = x[indexSurface, ]
  return(x)
}
  