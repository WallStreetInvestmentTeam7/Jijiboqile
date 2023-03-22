# w1-w2:
Before I start again, I would not want to modify my previous CCI Trend Following strategy into a short term mean reversion strategy by directly adjusting the parameters, although technically this is entirely feasible. This is because even if it were possible, it might be theoretically difficult to explain, for example, why would you use the CCI indicator in a short-term strategy when it is more suited to a long-term indicator in many cases? So I decided to do some research on this first.

Short-term trading focuses on the fluctuating price action of a financial instrument for quick profits, whereas long-term trading focuses on more fundamental aspects and aims to make steady returns over a longer timeframe. Therefore, short-term trading is seen as a more speculative investment type rather than a traditional buy and hold approach.

### Strategy ideas
1. Day trading
    + Buy and sell multiple instruments throughout the day with the aim of closing out positions before the market shuts    
    + Features: Can be used for any asset class or financial market; Avoid slippage; Don't need lots of indicators, only focus on price
    + Stop loss: Consider the previous day's low and high price: yesterday's high marked the point where sentiment changed and the sellers came back into the market, pushing the price lower.
https://www.cmcmarkets.com/en/trading-guides/day-trading

2. Swing trading
    + Swing traders focus on taking a position within a larger move, which could last several days or weeks. It is the longest style of short-term trading, as it takes advantage of medium-term movements too. Swing traders will attempt to spot a trend and capitalise on the rises and falls within the overall price movement. They will often rely on technical analysis to identify the entry and exit points for each trade.
https://www.ig.com/en/trading-strategies/swing-trading-strategies--a-beginners-guide-190712

### Questions:
1. Does a daily trading strategy mean that I have to trade every day? Or does it also require a condition before proceeding, and I trend for a short-period?
    + The idea of day trading may suggest that traders need to trade every day, but I think this idea of trading may not be suitable for our project as we can only set the trading strategy for the next day on a daily basis. As the number of trades is very limited, this strategy does not guarantee that the position will be cleared at the close.

2. Slippage?
    + 20% overnight gap. This would seem to make day trading very attractive, but for the same reason as above, we cannot use this strategy.

3. Will RSI and stochastic oscillator perform well in short-term trading?
    + The RSI compares the relative strength or weakness of a stock compared to other stocks in the market. The stochastic oscillator is used to decide whether a stock is expensive or cheap based on the stock's closing price range over a period of time. (Maybe these two can used to help with the Bollinger Band Squeeze strategy)
https://www.investopedia.com/articles/trading/09/short-term-trading.asp

### Possible strategies in order of priority, if the results are not good I will try next one
1. Bollinger Band Squeeze
https://school.stockcharts.com/doku.php?id=trading_strategies:bollinger_band_squeeze
According to this research (Short Term Trading Models – Mean Reversion Trading Strategies and the Black Swan Events, https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3538891), Bollinger Bands alone cannot be considered a sign for buy or sell signals, they just show when the asset is overbought or oversold. 

Q4: What is the difference between trading in overbought and oversold areas and trading in non-overbought and non-oversold areas in a short-term strategy?

2. 2-period RSI strategy
https://school.stockcharts.com/doku.php?id=trading_strategies:rsi2

3. My existing CCI Trend Following 
Change the CCI indicator to RSI indicator or something else

#### Use market orders to trade when appropriate and use limit orders to cut losses.

### Patterns (considerable):
1. Head and Shoulders
https://www.investopedia.com/terms/h/head-shoulders.asp
2. Triangles
https://www.investopedia.com/terms/t/triangle.asp
3. Double Tops
https://www.investopedia.com/terms/d/doubletop.asp
4. Double Bottoms
https://www.investopedia.com/terms/d/doublebottom.asp
5. Doji reversal pattern (volume spike+support at this price level)
https://www.investopedia.com/articles/trading/06/daytradingretail.asp

#### Update 16 Feb 2022: Zhangyuan Xu suddenly claims to have changed his strategy to short-term mean reversion. For diversity reasons, I have to re-enable the previous CCI Mean reversion strategy.

# w3:
Now I have to go back to my previous CCI Trend Following Strategy. 

First of all, I need to update my trading conditions, because when I implemented this strategy last semester, due to time issues, it was semi-finished. To be specific, I had previously only set up a trade between two CCI indicator lines, but did not consider how the CCI value crossed the two lines (i.e. whether it crossed from above or below). This obviously poses a risk of loss. To solve this problem, I set up a variable cciYesterday, where I can combine the two days of cci values to determine the crossover pattern.

After I drew some charts for analysis, I decided that this previous approach of using two other CCI indicator lines for position management was silly. For example, when the cci value fluctuated between the cciOverSold and cciOverSold*cciStop lines, my original plan was that I might buy when the cci value reached -110 from high to low and then sell when the cci value reached -110 from low to high. This has almost no chance of being profitable, the only profit comes from price fluctuations in the same cci value and not necessarily in a positive way. And since almost all strategies are essentially buying at the lows and selling at the highs for profit, why don't I return to the simplest idea of cci mean reversion for profit?
So I discussed the issue of strategy merger with Zhangyuan, and we agreed to temporarily use the same idea to make the strategy, and then consider how to merge it next week.

# w4:
For now, my strategy is defined in terms of the overbought and oversold zones of cci - in the overbought zone, the more the value of cci deviates from the overbought line of cci, the more likely I think it has a return. In this case, I will take more shorts, and vice versa.

## Position management: 
In the area below cciOverSold, the buying operation is continued based on the difference between the value of cci and cciOverSold. In areas above cciOverBought, continuous selling and shorting operations are performed based on the difference between the value of cci and cciOverSold.

cciStop have been deleted, because I believe this is not a good idea for my new cciOverboughtOversold strategy. 

The macd indicator might be considered removed as it can be replaced by other long-term strategies with similar functionality (if they perform well) when strategies are merged.
***
In addition to researching strategies, I was working on a small program that visualises every trade made in everyone's strategy and came across an interesting problem. This is very similar to the question mentioned in the stack overflow below, that is, after the global variable is initialized, the assignment to it inside the function does not work. 
https://stackoverflow.com/questions/44983596/r-global-assignment-of-vector-element-works-only-inside-a-function
It has been sovled by using Global variable identifiers (<<-) when appending values.
The visualisation program is done, named visualisation.R
***
I improved my position sizing based on the method Zhangyuan and Zheyu worked out (letting each time series have the same weight on the overall PD). This resulted in an overall PD ratio of 1.42 in the in sample data (all part 1) with a profit of approximately 100,000, of which 4 time series achieved PDs of around 2 and above. In the first 500 days of P2 (which we call pre-out-of-sample), it performed even better, with an overall PD ratio of 4.98. Among them, time series 3 has a ratio of 5.59, and there are 2 time series with PD ratio greater than 2, and one with PD ratio greater than 1. It's heartening.

Since my strategy performed the best, we decided to use my strategy as the core, combined with Kechen and Zhangyuan's approach, if needed. 

We first completed the visualization of all strategies at the meeting and analyzed them according to the data of Part 1.  

We found that Kechen's strategy can be very sensitive to find the low point of the price, and can always add positions at the right time, so I use his KDJ indicator as a supplementary judgment condition for my strategy. Because his strategy can't run time series 7 (which will cause unexplained errors), I removed time series 7. This time I got a total PD ratio of 1.06 in the in-sample data and a total PD ratio of 1.41 in the pre-out-of-sample, which looks like a reduction in profit. However, after carefully comparing the images we found that while some of the time series had lower profits, all of my losing time series had lower losses. So we all agreed that even though the previous results looked attractive, we still needed to combine my strategy with Kechen's.

From another perspective, in the analysis, we found that although Zhangyuan claimed that his strategy was a mean reversion idea, his strategy actually behaved in the same way as the trend following strategy. This is actually good news, because just like the macd indicator, his strategy may be a better stop loss for my current strategy (after combining with Kechen). I hope that in the next week, Zhangyuan's strategy can be compared with the macd indicator to become a better stop loss strategy.

# w5-w6:
Zhangyuan made a significant change to his strategy, making it a speculative one trading only one day at a time. Also, he came back to a mean reversion idea. However, due to the success of the strategy and the strict trading conditions, his trades can be steadily profitable in most cases. In order to increase profits, he used extremely high positions. This entails extremely high risk while making his strategy the most profitable among us (PD as high as 6+), but we had to rethink how to combine strategies.

My merger with Kechen's strategy is unquestionable, but how to merge Zhangyuan's speculative strategy with Zheyu's or my strategy becomes the question. Zhenyu's strategy can be considered a trend following, while Zhangyuan's is a mean reversion strategy, then their strategies must not be triggered at the same time. Meanwhile, Zhangyuan's correlation variable can suggest the likelihood of a trend following in the next 45 days, and conversely, the lower the correlation, the higher the likelihood of a mean reversion in the next 45 days. It is like eliminating some of the wrong answers in a multiple choice question, leaving answers that are not necessarily right, but less likely to be wrong.

On the other hand, everyone has different rules for clearing positions in their strategy. However, since you cannot disrupt someone else's strategy by clearing their positions because of their own strategy's clearing rules, each person adds a store so that only their own positions can be cleared when needed.

After solving these two main problems, our strategy was basically merged.

# w7:
The main work this week is the adjustment of parameters. 

In many attempts, I found that my strategy's position was positively correlated with PD in both in-sample data and pre-out-of-sample data, so I plan to increase the position. But in Zhangyuan's test of a series of real US stock data, we found that it is not uncommon for the market to experience sharp declines for multiple consecutive days (although this is almost absent in the data of Part 1 and Part 2). Such a situation is fatal for our merging strategy, which is mainly based on mean-reversion ideas. Zhangyuan's strategy is to buy and sell on the first day and liquidate all the positions on the second day, so this will not affect him very much. But our strategy requires long-term holding until a suitable time to sell, so it will lose a lot or even go bankrupt. This is contrary to our previous assumption that "since my strategy position is not large, I can choose not to set a stop loss" and means that the removal of MACD is inappropriate.

At the same time, since Zheyu's strategy performed very well in this market, I also tried to use his advantages to make up for the shortcomings of my strategy, that is, to use his judgment condition - correlation. The logic is this, if the past data shows a strong extreme trend and the past data has a 0.5 correlation with the future data, I will give up the trade.

In general, I re-added the MACD variable as stop loss and assisted with zheyu's correlation variable and other variables. This effectively avoids bankrupt and reduces the loss of extreme market prices to an acceptable range. Next, I need to further optimize the parameters of MACD to make the trigger frequency of stop loss more reasonable. More details can be found in my minutes 16.

But even though there are already two small strategies with stop loss, the stop loss of my strategy is still not mandatory to prohibit raising to high positions, so in some markets it is still possible to lose high positions. After the parameter adjustment test, I found that the current stop loss method is really difficult to achieve the desired effect, so I decided to use the Bollinger Band to stop loss. 

# w8
## Before getting P3 data

We optimized the parameters of the BBands and CCI indicators. Based on the visualized results, we found that CCI's mean reversion strategy did not always produce correct results, which we guessed was due to the high frequency of trades. Therefore, we adjusted the value of the cci indicator for overbought and oversold judgments to make it more difficult to trigger. We were pleasantly surprised that as the number of transactions decreased, the proportion of correct transactions increased. After weighing the pd ratio, the rate of return and the maximum drawdown, we decided to use plus or minus 130 as the overbought and oversold range. After that, we considered how to stop loss from the two aspects of the sensitivity and width of BBands, and adjusted the lookback and standard deviation of BBands.

## After getting P3 data
We got an overall PD ratio of about 1.7 with a 5.8% return and a max drawdown of 34k. We immediately looked at the candlestick chart of the part 3 data and found that the image was characterized by large ups and downs. In theory, then, Zheyu's strategy, which focuses on trend following, should take the lead. We ran a series of tests that confirmed our idea. Since Zheyu's strategy and Zhangyuan's strategy are mutually exclusive, Zheyu's correlation indicator can help Zhangyuan trade only at the right time, so although Zhangyuan's strategy is also based on the idea of mean reversion, it is still profitable. Kechen and I's strategy is overall loss-making in this market-led situation, but due to position control, our loss amount is very controllable. Overall, I think our diversity is adequate - so that we can benefit from most markets, and our position control and risk management are excellent, nice job my friends!

During the discussion, we discovered a new problem to be studied:
    + My combined strategy with Kechen is either very profitable or very losing in time series 1, 3, and 6. Could it be the effect of their relatively low price?