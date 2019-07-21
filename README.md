# FE590
Statistical learning Final Project

In this project, we try to find some relationship betweeen the sugar futures and other assets including 25 stocks that from food section and sugar producer companies and three major sugar producing countries’ currency exchange rates (India,Brazil and Thailand) and the ethanol futures which is the other mojor product using the same raw material of sugar. We try to predict the sugar futures daily return using these assets. 
Athough according to the theory that the commodity futures price may be mainly infuluenced by the basic demand and supply situation which is more macro in sence, we can still try to find some formerly indicator to predic the short term price trend. 
In practice, the sugar and ethanol futures data are downloaded from Quandl and other exchange ratees and stock prices are from Yahoo Finance. After clearing the data, we combined them to a whole data set (called “all_data”) and each column in the data set represents one asset. The first 25 column names are stocks’ codes and next 3 are BRL, INR and THB which are exchange rates and the final two are ethanol and sugar denoting the futures price.
