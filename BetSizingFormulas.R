result = function(predictions, x, y) {
  res = list()
  
  Npred = length(predictions)
  
  bets = list()
  
  bets$result = rep(0, Npred)
  bets$br = rep(1, Npred + 1)
  bets$bet = rep(0, Npred)
  
  results = list()
  results$Nrbets = 0
  
  minEdge = 0.03
  maxBrWage = 0.8
  
  for(i in 1 : Npred) {
    winexpectation = predictions[i]
    lossexpectation = 1 - predictions[i]
    if(!is.na(x$PSLthisplayer[i]) & !is.na(x$PSWthisplayer[i])){
      if(winexpectation * x$PSWthisplayer[i] - 1 > minEdge) {
        results$Nrbets = results$Nrbets + 1
        bets$bet[i] = maxBrWage * bets$br[i] * (winexpectation * x$PSWthisplayer[i] - 1) / (x$PSWthisplayer[i] - 1)
        #constant BR bet
        #bets$bet[i] = (winexpectation * x$PSWthisplayer[i] - 1) / (x$PSWthisplayer[i] - 1)
        bets$bet[i] = 1

        if(y[i] == 1 ) {
          bets$result[i] = bets$bet[i] * (x$PSWthisplayer[i] - 1) 
        } else {
          bets$result[i] = -bets$bet[i]
        }
        
      } else if(lossexpectation * x$PSLthisplayer[i] - 1 > minEdge) {
        results$Nrbets = results$Nrbets + 1
        bets$bet[i] = maxBrWage * bets$br[i] * (lossexpectation * x$PSLthisplayer[i] - 1) / (x$PSLthisplayer[i] - 1)
        #constant BR bet
        #bets$bet[i] = (lossexpectation * x$PSLthisplayer[i] - 1) / (x$PSLthisplayer[i] - 1)
        bets$bet[i] = 1
        if(y[i] == 0){
          bets$result[i] = bets$bet[i] * (x$PSLthisplayer[i] - 1)
        } else {
          bets$result[i] = -bets$bet[i]
        } 
      }
      
    }
    bets$br[i + 1] = bets$br[i] + bets$result[i]
  }
  
  
  
  results$Br = bets$br[Npred + 1]
  results$ROI = sum(bets$result) / sum(bets$bet)
  
  combined = list("results" = results, "bets" = bets)
  
  return(combined)
}