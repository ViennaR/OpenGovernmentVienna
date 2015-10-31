## Gathering data. 
It would be nice to harvest informations about voting (results and some basic meta data, e.g. topic) and deputies (party, sex, age, education, etc.).
Obtaining data on deputies seems to be quite easy web scrapping (e.g. with an rvest package) but obtaining information on voting results on the deputy level seems to be a little challenging as it appears only in transcriptions and only for some voting. In the worst case we will end up only with data on which party voted for (and which against) a given law (this can be obtained easily) - this will seriously limit possible analysis but we still will be able to do some visualizations and to develop an expert system.
## Analysis
Having data on voting results and context data on deputies we can run many different analysis, e.g. try to assess impact of different deputies characteristic on their votes, or try to predict results of one part of voting on the basis of other ones.
## Visualization
As always we can try to plot some graphs, maps, etc. and obviously we can try to do that in shiny :-)
## Expert system
We can try to develop an application which will choose some acts, ask user how he would vote and then try to asses deputy or party which is the most similar to the user. This can be done in a very simple way (random sample of acts, ranking as a simple sum of similar responses) but much more complicated algorithms can be developed as well (e.g. trying to avoid asking about asks with similar voting results, weighting responses by importance of a topic to the user, etc.)
