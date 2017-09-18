##FOUND ON https://gist.github.com/JeffSackmann/

#not convinced about this, since using binomial distribution where chance
# to win a set is sum(dbinom(x = 6 : 10, size = 10, prob = p)) + 
#dbinom(5, 10, prob = gameProb(0.51)) *  p ^ 2 /(p ^ 2 + (1 - p) ^ 2)
#gives different results for a set

ch <- function(a, b) {
  factorial(a)/(factorial(b)*factorial(a-b))
}


setOutcome <- function(final, sGames, rGames, vw, g, h) {
  
  pOutcome = 0
  for (j in 0 : sGames) {
    for (k in 0 : rGames) {
      if ((j + k) == (6 - 1 - vw)) {
        m = sGames - j
        n = rGames - k
        p = (g ^ j) * (h ^ k) * ((1 - g) ^ m) * ((1 - h) ^ n) * 
          ch(sGames, j) * ch(rGames, k) * final
        pOutcome = pOutcome + p
      } else {
        next()
      }
    }
  }
  return(pOutcome)
}



setGeneral <- function(s, u, v = 0, w = 0, tb = 1){
  
  ## calculate the probability of the current server winning
  ## a 6-game, tiebreak set, given prob. of server winning any
  ## given service point (s) or return point (u), and the current
  ## game score (v, w)
  ## get prob of current server winning a service game:
  
  g = gameProb(s) ## gameProb is another gist
  h = gameProb(u)

  if (tb == 1) {
    if (v == 7) {return(1)}
    else if (w == 7) {return(0)}
    else if ((v == 6) & ((v-w) > 1)) { return (1) }
    else if ((w == 6) & ((w-v) > 1)) { return(0) }
  } else {
    if ((v >= 6) & ((v-w) > 1)) { return(1) }
    else if ((w >= 6) & ((w-v) > 1))  {return (0) }
  }

  ## if not over, re-adjust down to no higher than 6-6

  while (TRUE) {
  
    if ((v + w) > 12) {
      v = V - 1
      w = W - 1
    } else {
      break
    }
  }

  ## if no tiebreak, chance that server wins set is ratio of server's prob of winning
  ## two games in a row to returner's prob of winning two games in a row

  if (tb == 0){ 
    deuceprob = (g * h)/((g * h) + (1 - g) * (1 - h))
  }
  outcomes = {}

  ## special cases, 11 games or more already

  if ((v+w) == 12) {
    if (tb == 1){
      tp = tiebreakProb(s, u) ## tiebreakProb is another gist
      outcomes['76'] = tp
      outcomes['67'] = 1 - tp
    } else {
      outcomes['75'] = deuceprob
      outcomes['57'] = 1-deuceprob 
    }
  } else if ((v+w) == 11) {
    if (tb == 1) {
      tp = tiebreakProb((1 - u), (1 - s))
      if (v == 6) {
        outcomes['75'] = g
        x = (1 - g)
        outcomes['76'] = x * (1 - tp)
        outcomes['67'] = x * tp
      } else {
        outcomes['57'] = 1 - g
        x = g
        outcomes['76'] = x * (1 - tp)
        outcomes['67'] = x * tp
      }
    } else{
      if (v == 6) {
        outcomes['75'] = g
        outcomes['57'] = 0
        f = 1 - g ## f is p(getting to 6-6)
      } else{
        outcomes['57'] = 1-g
        outcomes['75'] = 0
        f = g ## f is p(getting to 6-6)
      }
      outcomes['75'] = outcomes['75'] + f * deuceprob
      outcomes['57'] = outcomes['57'] + f * (1 - deuceprob)   
    }
  } else{
  
    ## win probabilities
    for (i in 0 : 4) { ## i = 0
      t = 6 + i - v - w ## total games remaining in set
      if (t < 1) {next}
      if ((t %% 2) == 0){
        final = h
        sGames = t / 2
        rGames = sGames - 1
      } else {
        final = g
        sGames = (t - 1) / 2
        rGames = (t - 1) / 2
      }
      pOutcome = setOutcome(final, sGames, rGames, v, g, h)
      key = paste('6', i, sep = "")
      outcomes[key] = pOutcome
    }
  }

    ## loss probabilities
    ## this section isn't necessary, but I wrote it for informal
    ## testing purposes

  for (i in 0 : 4) {
    t = 6 + i - v - w ## total games in set; here it's 6
    if (t < 1) {next()}
    if ((t %% 2) == 0) {
      final = 1-h
      sGames = t/2
      rGames = sGames - 1
    } else {
      final = 1-g
      sGames = (t-1)/2
      rGames = (t-1)/2
    }
  
    pOutcome = setOutcome(final, sGames, rGames, w, (1-g), (1-h))
    key = paste(i, '6', sep = "")
    outcomes[key] = pOutcome  
  }

  ## prob of getting to 5-5
  t = 10 - v - w
  if (t %% 2 == 0) {
    sGames = t / 2
    rGames = t / 2
  } else{
    sGames = (t - 1) / 2 + 1
    rGames = (t - 1) / 2
  }
  f = setOutcome(1, sGames, rGames, v, g, h)

  if (tb == 1) {
    outcomes['75'] = f * g * h
    outcomes['57'] = f * (1 - g) * (1 - h)
    x = f * g * (1 - h) + f * (1 - g) * h ## p(getting to 6-6)    
    if (((v+w) %% 2) == 0) {
      tp = tiebreakProb(s, u)
    } else {
      tp = tiebreakProb(u, s)
    }
    outcomes['76'] = x * tp
    outcomes['67'] = x - x*tp
  } else {
    outcomes['75'] = f * deuceprob
    outcomes['57'] = f * (1 - deuceprob)   
  }
  
  return(sum(outcomes[c('60', '61', '62', '63', '64', '75', '76')]))
}

gameOutcome <- function(s, a, b) {
  ch((a + b), a) * (s ^ a)*((1 - s) ^ b) * s
}

gameProb <- function(s, v=0, w=0) {
  
  ## function calculates the probability of server winning
  ## a single game, given p(winning any given point) [s],
  ## and the current point score.
  ## v, w = current game score, where love = 0, 15 = 1, etc.
  ## - e.g. 30-15 is v=2, w=1
  ## check if game is already over:

  if ((v >= 4) & ((v-w) >= 2)) {
    return(1)
  } else if((w >= 4) & ((w-v) >= 2)) {
    return(0)
  }

  ## if deuce or ad score e.g. 5-4, reduce to e.g. 3-2
  while(TRUE) {
    if ((v+w) > 6) {
      v = v - 1
      w = w - 1
    } else {
      break
    }
  }

## specific probabilities:

  if (w == 0) {
    w0 = gameOutcome(s, 3-v, 0)
    } else {
      w0 = 0
    }

  if (w <= 1) {
    w15 = gameOutcome(s, 3-v, 1-w)
  } else{ 
    w15 = 0
  }

  if (w <= 2) {
    w30 = gameOutcome(s, 3-v, 2-w)
  } else{ 
    w30 = 0
  }

  if (v == 4) {
    wAd = 0 
    lAd = s
    d = 1-s
  } else if (w == 4) {
    wAd = 1 - s
    lAd = 0
    d = s
  }  else{
    wAd = 0
    lAd = 0
    a = 3 - v
    b = 3 - w
    d = ch((a+b), a)*(s**a)*((1-s)**b)
  }

  if (v <= 2) {
    l30 = gameOutcome((1-s), 3-w, 2-v)
  } else{ 
    l30 = 0
  }

  if (v <= 1) {
    l15 = gameOutcome((1-s), 3-w, 1-v)
  } else{ 
    l15 = 0
  }

  if (v == 0) {
    l0 = gameOutcome((1-s), 3-w, 0)
  } else{
    l0 = 0
  }

  ## given d = prob of getting to deuce,
  ## math to divide up further outcomes
  
  denom = s**2 + (1-s)**2
  wd = (d*(s**2))/denom
  ld = (d*((1-s)**2))/denom
  win = w0 + w15 + w30 + wd + wAd
  lose = l0 + l15 + l30 + ld + lAd
  return (win)
}

tiebreakProb <- function(s, t, v = 0, w = 0, p = 7){
  
  ## calculate the probability that the current server wins a best-of-p tiebreak.
  ## s = p(server wins service point)
  ## t = p(current server wins return point)
  ## v, w = current score
  ## check if tiebreak is already over:
  
  if ((v >= p) & ((v-w) >= 2)) {
    return(1)
  } else if ((w >= p) & (w-v) >= 2){
    return(0)
  }

  ## re-adjust so that point score is not higher than p;
  ## e.g., if p=7 and score is 8-8, adjust to 6-6, which
  ## is logically equivalent

  while (TRUE) {
    if ((v+w) > 2*(p-1)) {
      v = v - 1
      w = w - 1
    } else {
      break
    }
  }

  outcomes = {} 
  ## track probability of each possible score
  ## this is messy and probably not optimal, figuring out
  ## how many points remain, and how many are on each
  ## player's serve:

  for (i in 0 : (p - 2)) {
    remain = p + i - v - w
    if (remain < 1) {
      next
    } 
    
    if ((remain %% 2) == 1) {
      if ((v + w) %% 2 == 0) { ## sr[rs[sr
        if ((remain - 1) %% 4 == 0) { ## ...s
          svc = (remain + 1) / 2 
          ret = (remain - 1) / 2
        } else {
          svc = (remain - 1) / 2
          ret = (remain + 1) / 2
        }
      } else { ## ss[rr[ss[
        if ((remain-1) %% 4 == 0) {## ...s
          svc = (remain + 1) / 2 
          ret = (remain - 1) / 2
        } else{
          svc = (remain+1)/2
          ret = (remain-1)/2
        }
      }
    } else{
      if ((v+w) %% 2 == 0) { ## sr[rs[sr
        svc = remain / 2
        ret = remain / 2
      } else { ## ss[rr[ss[
        svc = (remain - 2) / 2
        ret = (remain - 2) / 2 
        if ((remain %% 4) == 0) {
          svc = svc + 1
          ret = ret + 1
        } else{
          svc = svc + 2
        }
      }
    }

    ## who serves the last point?

    if ((v+w) %% 2 == 0) {
      ##if remain in [1, 4, 5, 8, 9, 12, 13, 16, 17, 20, 21]: ## pattern: remain % 4 in [0, 1]
      if ((remain %% 4) %in% c(0, 1)) {
        final = s
        svc = svc - 1
      } else {
        final = t
        ret = ret - 1
      }
    } else {
      ##if remain in [3, 4, 7, 8, 11, 12, 15, 16, 19, 20]:
      if ((remain %% 4) %in% c(3, 0)) {
        final = t
        ret = ret - 1
      } else {
        final = s
        svc = svc - 1
      }
    }

    pOutcome = 0

    for (j in 0 : svc) {
      for (k in 0 : ret) {
          if ((j+k) == (p - 1 - v)) {
            m = svc - j
            n = ret - k
            pr = (s ^ j) * (t ^ k)*((1 - s) ^ m) * ((1 - t) ^ n) * 
              ch(svc, j) * ch(ret, k) * final
            pOutcome = pOutcome + pr
          } else {
            next
          }
      }
    }
    key = paste(p, i, sep ="")
    outcomes[key] = pOutcome
  }

  if (remain %% 2 == 1) {
    if ((v+w) %% 2 == 0) {## sr[rs[sr
      if ((remain-1) %% 4 == 0) {## ...s
        svc = (remain + 1) / 2 
        ret = (remain - 1) / 2
      } else {
        svc = (remain - 1) / 2
        ret = (remain + 1) / 2
      }
    } else{ ## ss[rr[ss[
      if ((remain - 1) %% 4 == 0){## ...s
        svc = (remain + 1) / 2 
        ret = (remain - 1) / 2
      } else {
        svc = (remain + 1) / 2
        ret = (remain - 1) / 2   
      }
    }
  } else{
    if ((v+w) %% 2 == 0){ ## sr[rs[sr
      svc = remain / 2
      ret = remain / 2
    } else{ ## ss[rr[ss[
      svc = (remain - 2) / 2
      ret = (remain - 2) / 2 
      if (remain %% 4 == 0) {
        svc = svc + 1
        ret = ret + 1
      } else {
        svc = svc + 2
      }
    }
  }

## probability of getting to (p-1)-(p-1) (e.g. 6-6)

  final = 1
  x = 0

  for (j in 0 : svc){
    for (k in 0 : ret) {
      if ((j+k) == (p - 1 - v)) {
        m = svc - j
        n = ret - k
        pr = (s ^ j) * (t ^ k) * ((1 - s) ^ m)*((1 - t) ^ n) * 
          ch(svc, j) * ch(ret, k) * final
        x = x + pr
      } else {
        next
      }
    }
  }

  outcomes['+'] = (x * s * t) / ((s * t) + (1 - s) * (1 - t))

  ## add up all positive outcomes
  return (sum(outcomes))
}
