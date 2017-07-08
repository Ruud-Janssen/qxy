relevantVariables = function(dataset){
  
  variables = as.data.frame(matrix(nrow = nrow(dataset), ncol = 0))
  variables$ratingdiff = dataset$ratingdiff
  variables$ratingClaydiff = dataset$ratingClaydiff
  variables$ratingHarddiff = dataset$ratingHarddiff
  variables$ratingGrassdiff = dataset$ratingGrassdiff
  
  variables$DummyBo5 = dataset$DummyBo5
  variables$DummyBo5TimesAvgRatingdiff = dataset$DummyBo5TimesAvgRatingdiff
  variables$DummyBo5Timesratingdiff = dataset$DummyBo5 * dataset$ratingdiff
  variables$DummyBo5TimesratingHarddiff = dataset$DummyBo5 * dataset$ratingHarddiff
  variables$DummyBo5TimesratingGrassdiff = dataset$DummyBo5 * dataset$ratingGrassdiff
  variables$DummyBo5TimesratingClaydiff = dataset$DummyBo5 * dataset$ratingClaydiff
    
  variables$RetiredDiff = dataset$RetiredDiff
  variables$WalkoverDiff = dataset$WalkoverDiff
  variables$RetiredOrWalkoverDiff = dataset$RetiredOrWalkoverDiff
  variables$RetiredOrWalkoverDiff = dataset$RetiredOrWalkoverDiff
  variables$FatigueDiff = dataset$FatigueDiff
  variables$FatigueDiffTimesBo5 = dataset$FatigueDiffTimesBo5
  variables$HeadtoHead = dataset$HeadtoHead
  variables$LastHeadtoHead = dataset$LastHeadtoHead
  
  variables$HeadtoHeadPercentageWeightedsqN = dataset$HeadtoHeadPercentageWeightedsqN
  variables$HomeDiff = dataset$HomeDiff
  
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
  