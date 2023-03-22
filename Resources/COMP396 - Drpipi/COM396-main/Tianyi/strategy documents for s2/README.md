# visualisation.R
This code is written from a tooling perspective, and may be difficult for someone other than the author to run, but I'll try to explain it. 

***
NOTE: Since this code is only useful in the test session and will not be put into the final team1 code, it uses global variables for convenience.
***
In general, this file records the current position of the strategy for each day of the 10 time series and records it in a matrix called "strategyMatrix" through global assignment. The full matrix should have as many rows as the number of days the strategy was running, and as many columns as the number of time series the strategy was running on. For example, if the strategy runs for 1000 days for entire 10 time series, the strategyMatrix should have 1000 rows and 10 columns (actually there will be an extra row because the first row is empty when initialized). It's row is recording the specific number of current positions, while the column is the information of date.

## Before Running 
This visualization cannot be run independently of strategies, as it records changes in the current position during the strategy run. Therefore, in order to run this code, a runnable strategy file is required first. This strategy file can be any strategy file in the strategies folder of backtester_v5.6, but it needs to be able to run separately and produce results. Secondly, you may need to change the ncol value of strategyMatrix and runningDays, based on the actual situation. Then, the only thing you need to do is adding these code into the strategy file according to their location, which is significant!

In order to facilitate understanding of how it works, I made three important comments on lines 24, 28 and 30 of the code, representing the position of getOrder, the position of the lookback of the strategy and the position of the for loop that traverses the time series. Please be careful not to place the code in the wrong place when running, otherwise it may cause errors. 

## Find the results
If the code works correctly, you will find the corresponding images under the same path of main.R, they will end with png suffix. The name of the image should correspond to the number of the time series you are actually running. If you are running only part of the time series, the numbers of the images will change accordingly. For example, if you only run 1,2,3,7,8,9 time series, you will only find six pictures named accordingly. Provided the number of graphs is more than 6, please check when the images were created, the extra images may have been left over from the previous test.
