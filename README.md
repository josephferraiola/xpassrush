# xPassRush - 2023 NFL Big Data Bowl Submission

Our submission to this year's NFL Big Data Bowl is geared towards the coaching track, with the development of an expected pass rush metric. Before getting to our modeling process, we'll go through some of the data cleaning steps that allowed for the clean implementation of our metric and application. One of the first things we did was to add a player's primary position to the dataset, which was later used when building the model, and then standardizing the tracking data to remove conflation and any outliers. 

Now that we had our data ready, our choice of model was a XGBoost binary logistic to classify whether a player would rush the passer or not. Each of our primary positions ("CB", "EDGE", "IDL", "Off-Ball LB", "Safety") received their own model so the probabilities wouldn't be skewed by position. After all, an EDGE rusher will have a much greater likelihood of rushing the passer than a Safety, for example. After numerous rounds of feature testing, we finally settled on these four features: the y or vertical distance from the ball, the x or horizontal distance from the line of scrimmage, the player’s direction, and their crept up distance. Crept up distance was defined as the player’s movement from the initial frame of a play to the frame before the ball snap. All other variables are taken at the frame before the ball snap. 

With our features and target variable set, all that was left was to build the model. We utilized folding validation to build out our model to ensure it performed well on all subsets of the data on not just on a certain sample. We didn’t want it to be really accurate for one team but not so useful for another. Folding allows for the training data to be about 80% of the data for example, and then the model is predicting on the remaining 20% of the data, which would be done until all data contains predictions. (Note these aren’t the exact values we used, just for illustration purposes).

We immediately saw great performance from the model. The first key check was to see the xPassRush values when the player actually rushed the passer or didn’t. When players did rush the passer, they had an average 91% chance of rushing the passer. When they didn’t, they had an average 6% chance of rushing the passer. This large gap showed us we were on the right track with out modeling approach. We used a few other measures to confirm the model’s performance, including area under the curve and its distribution, which all showed great results. This model allowed for three key new metrics. xPassRush is simply the player’s expected probability of rushing the passer. Pass Rush Over Expected (PROE) shows us which players rush more often than not and vice versa. Mean Squared Error (MSE) shows us which players are more unpredictable in their rushing tendencies, stemming from the differences in their xPassRush and actual times rushing the passer.

We hope that xPassRush, MSE, and PROE can be valuable tools for coaches and pro scouts in their preparation for upcoming opponents and self-scouting. With further development xPassRush could even become a player development tool that helps QB and OL take mental reps to pickup on the subtle nuances and tells for certain defender alignments. An app could be created where a player has to predict which defender is going to rush the passer using the pre-snap information from the All-22. The player would input his prediction by checking off the players he expects to rush and summarize in a notes section what offensive protection call would then take place to block that group of pass rushers. Once that information is submitted the app would provide feedback to the player on whether his prediction was correct or not. xPassRush provides coaches and players alike with the ability to evaluate defenses before the snap and we believe its value is immeasurable. Thank you for reading!


