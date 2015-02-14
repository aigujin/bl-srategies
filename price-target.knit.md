---
title: "Meeting report"
author: "Artur Aiguzhinov"
date: 'Setember 3rd 2014 '
output:
  pdf_document:
    highlight: espresso
---
## High price targe values

On my last meeting with Ana Paula, we discussed the problem of high values in price targets and, subsequently, optimistic expected returns.

Since we don't have access to detailed history of `IBES` database, I suggested to use `Datastream` as they have consensus history of price targets. I downloaded  mean (`mn.PT.DS`), median (`md.PT.DS`), and number of price targets (`num.PT.DS`). At the meeting, I had only mean price target series and I could not perform a good analysis as the values were all skewed. That is why, I obtained median and the number of price targets.

If both databases (`IBES` and `Datastream`) are in-sync, then the ratio of same metric from both databases would be close or equal to 1.

First step was to compare the total number of observations in `Datastream` and `IBES`.


