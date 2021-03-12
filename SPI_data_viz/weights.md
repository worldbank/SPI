
## Overall Index Scores

Our statistical performance indicators have a three level structure, and our overall score will be formed by sequentially aggregating each level.

To begin we produce a score for each dimension, which is an unweighted average of the indicators within that pillar.  For instance, the Standards and Methods dimension will be formed by taking the unweighted average of the indicators for the system of national accounts in use, national accounts base year, classification of national industry, CPI base year, classification of household consumption, etc.

After computing a score for each dimension, a score for each pillar is computed, which is the average score of the dimensions in in that pillar. By default, for pillars 1, 2, 4, and 5, the unweighted average of the dimensions within each pillar is taken.  For pillar 3 on data products, we take a weighted average of the dimensions, where the weights are based on the number of SDGs in each dimension (6 SDGs in dimension 3.1 on social statistics, 6 SDGs in dimension 3.2 on economic statistics, 2 in dimension 3.3 on environmental statistics, and 2 in dimension 3.4 on institutional statistics).  

**However, the user can change these weights using the scrollbar.**  In order adjust the weights of a dimension, you can use the corresponding toggle in the bar on the right side of the screen.  In order to produce an overall score for the pillar, we constrain the weights to sum to 1.  This will happen in the background, so it is not obvious from the slider positions.

After calculating the scores for each dimension The SPI index is the average across the 5 dimensions.  **The user can use the corresponding toggle in the bar on the right side of the screen.**  In order to produce an overall score, we constrain the dimension weights to sum to 1.  This will happen in the background.