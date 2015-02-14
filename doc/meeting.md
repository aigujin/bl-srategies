Meeting at INESC, on 2014-06-14
=====
Price data
----
- pick some stocks and check 
- get price form data stream ( iterate )

Black-Litterman
--
## Strategy
- create true from non ranking (at t+1)
- same omega, different views

## Story: paper structure
build on top of ssrn paper  
#### 1. use paper and reproduce strategy

- {views: meanTPER from t-1; conf: coef of variation}

#### 2. _Rank matters_
- {views: baseline rankings t-1 (ranking on PT); conf=coef. of variation}

- {views: true ranking(PT), meanTPER from t; conf: 1}

#### 3. Last part
- {views: true ranking (EPS), meanTPER from t; conf: 1, coef of variation}


## result assumptions:
- ranking god
- average god
- naive
- default

## Constrains
Run the code with no short-sale

