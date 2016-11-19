# Project: Words 4 Music

### [Project Description](doc/Project4_desc.md)

![image](http://cdn.newsapi.com.au/image/v1/f7131c018870330120dbe4b73bb7695c?width=650)

Term: Fall 2016

+ [Data link](https://courseworks2.columbia.edu/courses/11849/files/folder/Project_Files?preview=763391)-(**courseworks login required**)
+ [Data description](doc/readme.html)
+ Contributor's name: Jaime Gacitua
+ Project title: **GBM to Recommend song lyrics**
+ Project summary: A model is proposed to predict song lyrics given song features. A brief description of the model is below.

***

+ We have a dictionary of 5000 words and, 2700 songs.
+ We have a matrix that, for every song, indicates how many times each word appears (bag of words)
+ We also have access to [multiple features](http://labrosa.ee.columbia.edu/millionsong/pages/example-track-description), for every song
+ The 19 **features** chosen to predict words are the following:
    1. tempo.median
    2. tempo.var
    3. tatums.median
    4. tatums.var
    5. loudness.median
    6. loudness.var
    7. duration
    8. timbre (median of each of the 12 dimensions)

+ The word matrix is converted into a binary matrix.
    * If a song is present in a song, the value is 1. Otherwise, 0.
+ For each column (word) of the matrix, a **Generalized Boosting Model (GBM)** was fitted, with bernoulli responses.
    * The 19 features are the input, and the (0-1) word column is the output.
    * In total 5000 GBM models are fit.

+ Parameter tuning was done using cross validation.
	* The error is calculated using the sum of ranks.

+ The model trains in around 30 minutes

+ The best average sum of ranks achieved was 0.229, and the simplest model for that result was n=100 trees and depth=8.


Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.
