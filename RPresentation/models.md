NLP Model for n-gram prediction
========================================================
author: Andrei Pazniak
date: 2016-05-31
autosize: true
font-family: 'Helvetica'

Overview of best models
========================================================

- Naive(stupid) back-off model
- Good-Turing smoothing <https://class.coursera.org/nlp/lecture/32>
- Katz's back-off model <https://en.wikipedia.org/wiki/Katz%27s_back-off_model>
- Kneser-Ney smoothing <https://en.wikipedia.org/wiki/Kneser%E2%80%93Ney_smoothing>

Analyse of algorithm implementation:

- CPU usage for all models are the same. Each approach requires N-1 cluster index searches for N-gram. It is possible to reduce cluster index searches for all models if we add a second index to our table.  
- Memory usage for all models are almost the same.


Preprocessing data for models
========================================================

Preprocess of input text is required. The following functions will help to build best results

- Remove punctuation
- Remove strip whitespaces
- Remove special symbols, especially emotions.
- Transform to lower

Example:

```r
  transFuncs<- list(removePunctuation, stripWhitespace, removeNumbers, content_transformer( function(x) iconv(enc2utf8(x), sub = " ")), 
                    content_transformer(tolower))
  corpus <- tm_map(corpus, tm_reduce, tmFuns = transFuncs)
```

Build n-grams and frequencies tables
========================================================

Preprocessing steps

1. Partitioning data to reduce memory consumption.
2. Building n-grams for n=1:4. Writing results to file.
3. Filter all n-grams with frequency less than 2.
4. Adding columns for each n-gram about start, last (n+1)-gram and mid (n+2) gram. This step significantly improves performance for Kneser-Ney smoothing.
5. Building tables for Good-Turing Smoothing.


|type              | CompressedSize| Size|
|:-----------------|--------------:|----:|
|Input Data        |            261|  509|
|Frequencies table |             25|  118|

Results
========================================================

![Example](example.png)

|PerformanceFor                   |AverageTime | Attemtps|MinMemory |
|:--------------------------------|:-----------|--------:|:---------|
|Kneser-Ney Smoothing             |90 ms       |      100|118 MB    |
|Naive with Good-Turing Smoothing |420 ms      |      100|118 MB    |

Steps to improve the model:

- Input data are relatively small, it should be at least 10x times bigger.
- We can significantly decrease memory consumption and improve CPU performance by transforming string to compact mode.
- [Boosting](https://en.wikipedia.org/wiki/Boosting_(machine_learning) different type of models. 

