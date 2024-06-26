---
title: "Text Mining _C_ (v1)"
output:
  html_document:
    keep_md: true
---

This markdown document contains the outputs I refer to in my [Substack post](https://nycdna.substack.com/p/text-mining-tom-mccarthys-c) on text mining Tom McCarthy's brilliant novel [_C_](https://www.penguinrandomhouse.com/books/201638/c-by-tom-mccarthy/), as well as the code I use to get those outputs.

A significant amount of the code below is adapted from Julia Silge and David Robinson’s [_Text Mining with R: A Tidy Approach_](https://www.tidytextmining.com/). Additionally, I imagine there are better or more elegant ways of doing many of the things I do below. My general approach was to just cobble together whatever I needed to make this project work.

### Preprocessing

``` r
library(tidyverse)

# load text file and make it into a dataframe; some minor cleanup

# I toss out the headers for the novel's parts but preserve their titles (e.g., "Caul")

c_raw <- read.delim("C.txt", header = FALSE, sep = "\n")
c_raw <- c_raw[["V1"]]
c_df <- tibble(paragraph = 1:length(c_raw), text = c_raw) |>
  mutate(chapter = cumsum(str_detect(text, regex("CHAPTER",
                                                 ignore_case = FALSE))),
         .before = paragraph) |>
  mutate(section = cumsum(str_detect(text, regex("SECTION",
                                                 ignore_case = FALSE))),
         .after = chapter) |>
  mutate(text = trimws(gsub(regex("PART [0-9]+"), "", text)))

c_df <- c_df[!grepl("CHAPTER [0-9]+|SECTION [0-9]+", c_df$text), ]

# load tidytext's stopwords list and customize it

library(tidytext)
data(stop_words)

stop_words <- bind_rows(stop_words, tibble(word = c("beneath", "set",
                                                    "sets", "setting",
                                                    "tell", "tells",
                                                    "telling", "told"),
                                           lexicon = "custom")) |>
  filter(!word %in% c("c", "c's"))

# tokenize and stem words

library(SnowballC)

c_tidy_blocked <- c_df |>
  unnest_tokens(word, text)

# add a marker every 100 words (before removing stop words) such that text can be partitioned into blocks of uniform length

block_length <- 100

c_tidy_blocked$block <- rep(seq(1, 1 + (nrow(c_tidy_blocked)) %/% block_length),
                            each = block_length,
                            length.out = nrow(c_tidy_blocked))
  
c_tidy <- c_tidy_blocked |>
  relocate(block, .after = section) |>
  anti_join(stop_words, by = "word") |>
  mutate(word = wordStem(word, language = "english"))
```

### Overall unigram frequency analysis

#### Basic frequency:

Here are the ten most frequently appearing word stems in _C_:


```
## # A tibble: 10 × 2
##    word      n
##    <chr> <int>
##  1 serg   1129
##  2 time    265
##  3 hand    248
##  4 head    183
##  5 move    180
##  6 line    171
##  7 run     165
##  8 start   152
##  9 eye     150
## 10 word    142
```

#### tf-idf:

And here are the top ten word stems in _C_ by tf–idf, after assembling a corpus of public domain novels in which to calculate tf–idf:


``` r
library(gutenbergr)

# gather public domain novels to create a corpus in which to calculate tf–idf

novels_df <- gutenberg_download(c(768, 766, 110, 42671,
                                  526, 4300, 432, 67138),
                                meta_fields = "title",
                                mirror = "http://mirror.csclub.uwaterloo.ca/gutenberg/")

novels_df <- novels_df[!(novels_df$text == ""), ]

# tokenize and stem words in public domain novels

# I'm not worrying about extraneous text in the public domain novels (e.g., titles, tables of contents); retaining that text doesn't strike me as ultra-consequential for the limited purposes of this tf–idf analysis

novels_tidy <- novels_df |>
  unnest_tokens(word, text) |>
  mutate(word = str_extract(word, "[a-zà-ÿ0-9'’]+")) |> # get rid of underscores used for emphasis in UTF-8 encoding
  mutate(word = gsub("’", "'", word)) |>
  anti_join(stop_words, by = "word") |>
  mutate(word = wordStem(word, language = "english")) # stem all words for consistency with the approach I took with C

# sort words in each public domain novel by frequency within that novel

novels_tidy_sorted <- count(novels_tidy, title, word, sort = TRUE)

# merge dataframes

big_dataframe <- rbind(novels_tidy_sorted, c_tidy_sorted |>
                         mutate(title = "C", .before = "word"))

# calculate tf-idf

c_top_tf_idf <- big_dataframe |>
  bind_tf_idf(word, title, n) |>
  filter(title == "C") |>
  slice_max(tf_idf, n = 10, with_ties = FALSE) |>
  select(word, tf_idf)

c_top_tf_idf
```

```
## # A tibble: 10 × 2
##    word      tf_idf
##    <chr>      <dbl>
##  1 serg     0.0255 
##  2 carrefax 0.00618
##  3 sophi    0.00420
##  4 widsun   0.00343
##  5 audrey   0.00293
##  6 learmont 0.00284
##  7 gibb     0.00239
##  8 clair    0.00204
##  9 macauley 0.00203
## 10 dobai    0.00194
```

### Overall bigram frequency analysis

#### Basic frequency analysis:

Here I tokenize _C_ into bigrams instead of unigrams:


``` r
# bigrams will not be sensitive to sentence or paragraph boundaries and will include titles of novel's parts

# any bigram that contains a stop word will be removed

c_df_collapsed <- tibble(line = 1, text = paste(c_df$text, collapse = " ")) |>
  mutate(text = gsub("  ", " ", text))

c_bigrams_tidy <- c_df_collapsed |>
  unnest_tokens(bigram, text, token = "ngrams", n = 2) |>
  separate(bigram, c("word1", "word2"), sep = " ") |>
  filter(!word1 %in% stop_words$word) |>
  mutate(word1 = wordStem(word1, language = "english")) |>
  filter(!word2 %in% stop_words$word) |>
  mutate(word2 = wordStem(word2, language = "english")) |>
  unite(bigram, word1, word2, sep = " ")
```

And here are the 20 most frequent bigrams in _C_:


```
## # A tibble: 20 × 2
##    bigram              n
##    <chr>           <int>
##  1 miss dobai         41
##  2 dr filip           40
##  3 miss hubbard       27
##  4 crypt park         19
##  5 serg answer        16
##  6 kite balloon       15
##  7 serg repli         15
##  8 walpond skinner    15
##  9 dr learmont        14
## 10 mulberri lawn      14
## 11 day school         13
## 12 herr landmess      13
## 13 serg watch         13
## 14 c c                12
## 15 serg feel          12
## 16 tabl top           11
## 17 prospect parent    10
## 18 tabl tilt          10
## 19 mosaic garden       9
## 20 black ink           8
```

Here's a graph of those top 20 bigrams:

<img src="Text-Mining-C_files/figure-html/bigram_graph-1.png" width="85%" style="display: block; margin: auto;" />

#### tf-idf:

Now I check to see if the top 20 bigrams in _C_ remain basically the same if we calculate them based on tf-idf instead of simple frequency:


``` r
# get bigrams of public domain novels

novels_df_collapsed <- setNames(aggregate
                                (novels_df$text,
                                  list(novels_df$title),
                                  paste, collapse=" "),
                                c("title", "text")) |>
  mutate(text = gsub("  ", " ", text))

novels_bigrams_tidy <- novels_df_collapsed |>
  unnest_tokens(bigram, text, token = "ngrams", n = 2) |>
  separate(bigram, c("word1", "word2"), sep = " ") |>
  mutate(word1 = str_extract(word1, "[a-zà-ÿ0-9'’]+")) |>
  mutate(word1 = gsub("’", "'", word1)) |>
  filter(!word1 %in% stop_words$word) |>
  mutate(word1 = wordStem(word1, language = "english")) |>
  mutate(word2 = str_extract(word2, "[a-zà-ÿ0-9'’]+")) |>
  mutate(word1 = gsub("’", "'", word1)) |>
  filter(!word2 %in% stop_words$word) |>
  mutate(word2 = wordStem(word2, language = "english")) |>
  unite(bigram, word1, word2, sep = " ")

novels_bigrams_tidy_sorted <- count(novels_bigrams_tidy,
                                    title, bigram, sort = TRUE)
  
big_bigram_dataframe <- rbind(novels_bigrams_tidy_sorted,
                              c_bigrams_tidy_sorted |>
                         mutate(title = "C", .before = "bigram"))

# calculate top 20 bigrams in C by tf-idf

c_top_bigram_tf_idf <- big_bigram_dataframe |>
  bind_tf_idf(bigram, title, n) |>
  filter(title == "C") |>
  slice_max(tf_idf, n = 20, with_ties = FALSE) |>
  select(bigram, tf_idf)

c_top_bigram_tf_idf
```

```
##             bigram      tf_idf
## 1       miss dobai 0.005276840
## 2         dr filip 0.005148136
## 3     miss hubbard 0.003474992
## 4       crypt park 0.002445365
## 5      serg answer 0.002059255
## 6     kite balloon 0.001930551
## 7       serg repli 0.001930551
## 8  walpond skinner 0.001930551
## 9      dr learmont 0.001801848
## 10   mulberri lawn 0.001801848
## 11      day school 0.001673144
## 12   herr landmess 0.001673144
## 13      serg watch 0.001673144
## 14       serg feel 0.001544441
## 15 prospect parent 0.001287034
## 16       tabl tilt 0.001287034
## 17   mosaic garden 0.001158331
## 18       black ink 0.001029627
## 19       lead serg 0.001029627
## 20    school pupil 0.001029627
```

They're close to being the same, with the notable omission of "c c," which disappears due to strings like "reverend Nicholas Dudley C. C." in [_Ulysses_](https://www.gutenberg.org/cache/epub/4300/pg4300-images.html) and "'It would be an incalculable loss if,' &c., &c." in [_Heart of Darkness_](https://www.gutenberg.org/files/219/219-h/219-h.htm).

### Frequency analysis of select words starting with C

Here I look at the frequency in _C_ of the following words (which I'll call "Special Words"), based on appearances per chapter, section, or block of 100 words:


```
                                                             
 "C"      "cipher"                  "copper"                 
 "call"   "cocaine"                 "crypt/cryptography/etc."
 "carbon" "code"                    "cyan/cyanide"           
 "centre" "communication/Comintern" "cyst"                   
 "Christ" "connect"                 "sea"                    
```

#### Aggregate frequency:

First I look at the aggregate frequency of all Special Words across chapters, sections, and blocks of 100 words.


``` r
# filter unigrams of C down to only Special Words and merge tokens I count as being instances of the same word for the limited purposes of this analysis (e.g., "carbon" and "cc")

# really not loving the inelegance of this whole chunk of code, but this works for now

special_words_filtered <- c_tidy |>
  filter(chapter > 0) |>
  filter(grepl("\\bc\\b|\\bcc\\b|\\bcall\\b|^carbon|^centr|^christ|^cipher|^cocain|\\bcode\\b|^communic|^comintern|^connect|^copper|^crypt|^cyan|^cyst|\\bsea\\b", word)) |>
  filter(!word %in% c("c'est"))

special_words_filtered_merged <- special_words_filtered |>
  mutate(word = gsub(regex("\\bcc\\b|\\bcc'd\\b|^carbon[a-zà-ÿ]+\\b"), "carbon", word)) |>
  mutate(word = gsub(regex("^centr|centr[a-zà-ÿ]+\\b"), "centre", word)) |>
  mutate(word = gsub(regex("^christ[a-zà-ÿ]+\\b"), "christ", word)) |>
  mutate(word = gsub(regex("^cipher[a-zà-ÿ]+\\b"), "cipher", word)) |>
  mutate(word = gsub(regex("^cocain"), "cocaine", word)) |>
  mutate(word = gsub(regex("^communic\\b|comintern"), "communication", word)) |>
  mutate(word = gsub(regex("^connect[a-zà-ÿ]+\\b"), "connect", word)) |>
  mutate(word = gsub(regex("^crypt[a-zà-ÿ]+\\b"), "crypt", word)) |>
  mutate(word = gsub(regex("^cyan[a-zà-ÿ]+\\b"), "cyan", word)) |>
  mutate(word = gsub(regex("^cyst[a-zà-ÿ]+\\b"), "cyst", word))

# get total number of words in each chapter/section/block (inclusive of stopwords)

total_words_by_chapter <- c_df |>
  unnest_tokens(word, text) |>
  filter(chapter > 0) |>
  count(chapter) |>
  rename(total = n)

total_words_by_section <- c_df |>
  unnest_tokens(word, text) |>
  filter(section > 0) |>
  count(section) |>
  rename(total = n)

total_words_by_block <- c_tidy_blocked |>
  count(block) |>
  rename(total = n)

# get number of Special Words by chapter/section/block

special_words_sorted_by_chapter <- count(special_words_filtered_merged, chapter, word, sort = TRUE) |>
  arrange(word)

special_words_sorted_by_section <- count(special_words_filtered_merged, section, word, sort = TRUE) |>
  arrange(word)

special_words_sorted_by_block <- count(special_words_filtered_merged, block, word, sort = TRUE) |>
  arrange(word)

# calculate and plot aggregate frequency of all Special Words by chapter/section/block

aggregate_word_frequency_by_chapter <- special_words_sorted_by_chapter |>
  group_by(chapter) |>
  summarise(n = sum(n)) |>
  left_join(total_words_by_chapter, by = "chapter") |>
  mutate(percentage = n/total)

aggregate_word_frequency_by_section <- special_words_sorted_by_section |>
  group_by(section) |>
  summarise(n = sum(n)) |>
  left_join(total_words_by_section, by = "section") |>
  mutate(percentage = n/total)

aggregate_word_frequency_by_block <- special_words_sorted_by_block |>
  group_by(block) |>
  summarise(n = sum(n)) |>
  left_join(total_words_by_block, by = "block") |>
  mutate(percentage = n/total)

library(patchwork)
theme_set(theme_classic())

plot1.1 <- ggplot(aggregate_word_frequency_by_chapter, aes(chapter, percentage)) +
  geom_line(colour = "#ED1C24") +
  scale_x_continuous(limits = c(1,12), breaks = seq(0, 100), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.title.y = element_text(vjust = 2, family = "mono"),
        axis.title.x = element_text(family = "mono"))

plot1.2 <- ggplot(aggregate_word_frequency_by_section, aes(section, percentage)) +
  geom_line(colour = "#ED1C24") +
  scale_x_continuous(limits = c(1,39), breaks = seq(0, 100, by = 5), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(family = "mono"))

plot1.3 <- ggplot(aggregate_word_frequency_by_block, aes(block, percentage)) +
  geom_line(colour = "#ED1C24") +
  scale_x_continuous(limits = c(1,1194), breaks = seq(0, 1194, by = 200), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(family = "mono"))

plot1.1 + plot1.2 + plot1.3 + plot_annotation(title = "aggregate frequency of all Special Words in C", subtitle = "by chapter, section, and block of 100 words") & theme(plot.title = element_text(family = "mono", face = "bold"), plot.subtitle = element_text(family = "mono"))
```

<img src="Text-Mining-C_files/figure-html/special_words_aggregrate_frequency-1.png" width="100%" style="display: block; margin: auto;" />

I calculate Pearson correlation coefficients for the above data to check for any linear trend:


```
 chapter
 correlation: 0.4282063 
 p-value: 0.1649067 

 section
 correlation: 0.09153818 
 p-value: 0.5900058 

 block
 correlation: 0.1137324 
 p-value: 0.04683194
```

There's a positive correlation between frequency of special words and the forward temporal progression of the novel. If calculated based on 100-word blocks, the increase has a modest level of statistical significance.

#### Individual frequency:

Now I look at the frequency of individual Special Words across chapters/sections/blocks:


``` r
# calculate frequency of individual Special Words across chapters/sections/blocks:

special_word_frequency_by_chapter <- special_words_sorted_by_chapter |>
  pivot_wider(names_from = word, values_from = n, values_fill = 0) |>
  left_join(total_words_by_chapter, by = "chapter") |>
  mutate(across(c(2:16), ~ .x/total))

special_word_frequency_by_section <- special_words_sorted_by_section |>
  pivot_wider(names_from = word, values_from = n, values_fill = 0) |>
  left_join(total_words_by_section, by = "section") |>
  mutate(across(c(2:16), ~ .x/total))

special_word_frequency_by_block <- special_words_sorted_by_block |>
  pivot_wider(names_from = word, values_from = n, values_fill = 0) |>
  left_join(total_words_by_block, by = "block") |>
  mutate(across(c(2:16), ~ .x/total))

# plot individual frequencies of all 15 Special Words words across chapters/sections/blocks:

plot2.1 <- ggplot(special_word_frequency_by_chapter, aes(chapter)) +
  geom_line(aes(y = c, colour = "c")) +
  geom_line(aes(y = call, colour = "call")) +
  geom_line(aes(y = carbon, colour = "carbon")) +
  geom_line(aes(y = centre, colour = "centre")) +
  geom_line(aes(y = christ, colour = "christ")) +
  geom_line(aes(y = cipher, colour = "cipher")) +
  geom_line(aes(y = cocaine, colour = "cocaine")) +
  geom_line(aes(y = code, colour = "code")) +
  geom_line(aes(y = communication, colour = "communication")) +
  geom_line(aes(y = connect, colour = "connect")) +
  geom_line(aes(y = copper, colour = "copper")) +
  geom_line(aes(y = crypt, colour = "crypt")) +
  geom_line(aes(y = cyan, colour = "cyan")) +
  geom_line(aes(y = cyst, colour = "cyst")) +
  geom_line(aes(y = sea, colour = "sea")) +
  scale_x_continuous(limits = c(1, 12), breaks = seq(0, 100), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_colour_manual(values = c("#ED1C24","#EF7B10","#0060A8", "#B26300", "#FFD329", "#007D32", "#F4A9BE", "#A1A5A7", "#9B0058", "#0019A8", "#00FFA1", "#9364CC", "cyan", "#6CBE45", "#BD3844")) +
  labs(x = "chapter", y = "frequency", colour = "word") +
  ggtitle("frequency of Special Words in C", subtitle = "by chapter") +
  theme(text = element_text(family = "mono"), plot.title = element_text(face = "bold"))

plot2.2 <- ggplot(special_word_frequency_by_section, aes(section)) +
  geom_line(aes(y = c, colour = "c")) +
  geom_line(aes(y = call, colour = "call")) +
  geom_line(aes(y = carbon, colour = "carbon")) +
  geom_line(aes(y = centre, colour = "centre")) +
  geom_line(aes(y = christ, colour = "christ")) +
  geom_line(aes(y = cipher, colour = "cipher")) +
  geom_line(aes(y = cocaine, colour = "cocaine")) +
  geom_line(aes(y = code, colour = "code")) +
  geom_line(aes(y = communication, colour = "communication")) +
  geom_line(aes(y = connect, colour = "connect")) +
  geom_line(aes(y = copper, colour = "copper")) +
  geom_line(aes(y = crypt, colour = "crypt")) +
  geom_line(aes(y = cyan, colour = "cyan")) +
  geom_line(aes(y = cyst, colour = "cyst")) +
  geom_line(aes(y = sea, colour = "sea")) +
  scale_x_continuous(limits = c(1, 39), breaks = seq(0, 100, by = 5), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_colour_manual(values = c("#ED1C24","#EF7B10","#0060A8", "#B26300", "#FFD329", "#007D32", "#F4A9BE", "#A1A5A7", "#9B0058", "#0019A8", "#00FFA1", "#9364CC", "cyan", "#6CBE45", "#BD3844")) +
  labs(x = "section", y = "frequency", colour = "word") +
  ggtitle("frequency of Special Words in C", subtitle = "by section") +
  theme(text = element_text(family = "mono"), plot.title = element_text(face = "bold"))

plot2.3 <- ggplot(special_word_frequency_by_block, aes(block)) +
  geom_line(aes(y = c, colour = "c")) +
  geom_line(aes(y = call, colour = "call")) +
  geom_line(aes(y = carbon, colour = "carbon")) +
  geom_line(aes(y = centre, colour = "centre")) +
  geom_line(aes(y = christ, colour = "christ")) +
  geom_line(aes(y = cipher, colour = "cipher")) +
  geom_line(aes(y = cocaine, colour = "cocaine")) +
  geom_line(aes(y = code, colour = "code")) +
  geom_line(aes(y = communication, colour = "communication")) +
  geom_line(aes(y = connect, colour = "connect")) +
  geom_line(aes(y = copper, colour = "copper")) +
  geom_line(aes(y = crypt, colour = "crypt")) +
  geom_line(aes(y = cyan, colour = "cyan")) +
  geom_line(aes(y = cyst, colour = "cyst")) +
  geom_line(aes(y = sea, colour = "sea")) +
  scale_x_continuous(limits = c(1, 1194), breaks = seq(0, 1194, by = 100), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_colour_manual(values = c("#ED1C24","#EF7B10","#0060A8", "#B26300", "#FFD329", "#007D32", "#F4A9BE", "#A1A5A7", "#9B0058", "#0019A8", "#00FFA1", "#9364CC", "cyan", "#6CBE45", "#BD3844")) +
  labs(x = "block", y = "frequency", colour = "word") +
  ggtitle("frequency of Special Words in C", subtitle = "by block of 100 words") +
  theme(text = element_text(family = "mono"), plot.title = element_text(face = "bold"))

plot2.1
```

<img src="Text-Mining-C_files/figure-html/special_words_individual_frequency-1.png" width="100%" style="display: block; margin: auto;" />

Here too, I calculate Pearson correlation coefficients for the above data to check for linear trends:


```
                        chapter (cor)  (p-value) section (cor)  (p-value)
C                          0.59430914 0.04156302   0.372257782 0.02327622
call                       0.65638226 0.02043243   0.262603905 0.11636095
carbon                     0.68696457 0.01358749   0.183639602 0.27660310
centre                     0.17404367 0.58852329   0.008183258 0.96166118
Christ                     0.46010316 0.13230753   0.186610673 0.26877068
cipher                    -0.35194219 0.26190322  -0.197370968 0.24163243
cocaine                    0.37609472 0.22824323   0.174002157 0.30302414
code                      -0.02203525 0.94580773  -0.106876018 0.52895915
communication/Comintern    0.43158429 0.16123557   0.204511029 0.22467959
connect                    0.29831599 0.34628679   0.124195826 0.46394437
copper                    -0.35094058 0.26335796  -0.336889128 0.04145846
crypt/cryptography/etc.   -0.33285542 0.29042639  -0.222174074 0.18628932
cyan/cyanide              -0.24036615 0.45173685  -0.146899314 0.38561052
cyst                       0.12383162 0.70140343  -0.012729301 0.94039399
sea                        0.52000123 0.08309977   0.252453438 0.13169644
                         block (cor)    (p-value)
C                        0.148998259 9.045638e-03
call                     0.160466840 4.896097e-03
carbon                   0.087724412 1.257142e-01
centre                  -0.016587834 7.725807e-01
Christ                   0.057795275 3.135941e-01
cipher                  -0.172314273 2.489722e-03
cocaine                  0.063187843 2.704993e-01
code                    -0.101499067 7.625805e-02
communication/Comintern  0.099546681 8.211400e-02
connect                 -0.007695203 8.933534e-01
copper                  -0.077859389 1.743140e-01
crypt/cryptography/etc. -0.252886045 7.513388e-06
cyan/cyanide            -0.171244374 2.651229e-03
cyst                    -0.009865364 8.635387e-01
sea                      0.114460260 4.543315e-02
```

### Pairwise correlation analysis of special words

Finally, I look at the phi coefficients for pairs of Special Words within the same chapter/section/block of _C_ (that is, how correlated the appearance of one Special Word within a given chapter/section/block is with the appearance of another Special Word within that same chapter/section/block).


``` r
library(widyr)

special_word_pairs <- special_words_filtered_merged |>
  group_by(word)
  
word_pairs_by_chapter <- pairwise_cor(special_word_pairs, word, chapter, sort = TRUE)

word_pairs_by_section <- pairwise_cor(special_word_pairs, word, section, sort = TRUE)

word_pairs_by_block <- pairwise_cor(special_word_pairs, word, block, sort = TRUE)
```

Here are the 10 Special Words most correlated with "c" by chapter:


``` r
word_pairs_by_chapter |>
  filter(item1 == "c") |>
  slice_max(correlation, n = 10, with_ties = FALSE)
```

```
## # A tibble: 10 × 3
##    item1 item2         correlation
##    <chr> <chr>               <dbl>
##  1 c     communication       0.529
##  2 c     carbon              0.507
##  3 c     cocaine             0.488
##  4 c     cyst                0.378
##  5 c     call                0.357
##  6 c     centre              0.357
##  7 c     copper              0.314
##  8 c     code                0.314
##  9 c     connect             0.314
## 10 c     sea                 0.293
```

Here are the 10 Special Words most correlated with "call" by chapter:


``` r
word_pairs_by_chapter |>
  filter(item1 == "call") |>
  slice_max(correlation, n = 10, with_ties = FALSE)
```

```
## # A tibble: 10 × 3
##    item1 item2         correlation
##    <chr> <chr>               <dbl>
##  1 call  communication       0.674
##  2 call  sea                 0.522
##  3 call  crypt               0.426
##  4 call  copper              0.357
##  5 call  connect             0.357
##  6 call  c                   0.357
##  7 call  christ              0.302
##  8 call  carbon              0.302
##  9 call  cocaine             0.174
## 10 call  cyan                0.135
```

And here's a visualization of every pair of Special Words that has a positive correlation within chapters (left) and 100-word blocks (right) of _C_:

<img src="Text-Mining-C_files/figure-html/top_pairwise_graphs-1.png" width="100%" style="display: block; margin: auto;" />

### Coda

Here I just generate a couple of added visualizations I use in my Substack post:

<img src="Text-Mining-C_files/figure-html/extra-1.png" width="100%" style="display: block; margin: auto;" /><img src="Text-Mining-C_files/figure-html/extra-2.png" width="100%" style="display: block; margin: auto;" /><img src="Text-Mining-C_files/figure-html/extra-3.png" width="100%" style="display: block; margin: auto;" /><img src="Text-Mining-C_files/figure-html/extra-4.png" width="100%" style="display: block; margin: auto;" />
