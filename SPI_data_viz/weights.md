
## Overall Index Scores

Our statistical performance indicators have a three level structure, and our overall score will be formed by sequentially aggregating each level.

To begin we produce a score for each pillar, which is an unweighted average of the indicators within that pillar.  For instance, the Census and Surveys pillar will be formed by taking the unweighted average of the Population Census score, the Agriculture Census score, the Business Census score, the Labor Force Survey score, the Health Survey score, etc.  

<img src="https://latex.codecogs.com/gif.download?SPI.PILLAR%7Bctds%7D%20%3D%20%5Csum_%7Bi%3D1%7D%5E%7BN_I%7D%20%5Cfrac%7BSPI.IND_%7Bctdsi%7D%7D%7BN_I%7D" />

where SPI.PILLAR is pillar s, in dimension d, in time period t, and country c.  SPI.IND is an indicator (e.g. population census score).

After computing a score for the pillar, we then compute a dimension score, which is the average of the pillars in that dimension. For dimensions 1, 2, 4, and 5, we take the unweighted average of the pillars in the dimension.  However, for Dimension 3 on data products, we take a weighted average of the pillars, where the weights are based on the number of SDGs in each pillar (6 SDGs in Pillar 3.1 on social statistics, 6 SDGs in Pillar 3.2 on economic statistics, 2 in Pillar 3.3 on environmental statistics, and 2 in Pillar 3.4 on institutional statistics).  We take the perspective that all SDGs are of equal importance, and therefore weight our pillars accordingly.

<img src="https://latex.codecogs.com/gif.download?SPI.DIM_%7Bctd%7D%20%3D%20%5Csum_%7Bs%3D1%7D%5E%7BN_S%7D%20%5Cfrac%7B%5Comega_%7Bds%7D%20%5Ctimes%20SPI.PILLAR%7Bctds%7D%7D%7BN_S%7D" />

Where &omega;	 is the weight for pillar s in dimension d.  In order adjust the weights of a pillar, you can use the corresponding toggle in the bar on the right side of the screen.  In order to produce an overall score for the dimension, we constrain the weights to sum to 1.  This will happen in the background, so it is not obvious from the slider positions.

After calculating the scores for each dimension The SPI index is the average across the 5 dimensions.

The SPI index is scaled to have a maximum score of 100 and a minimum of 0. A score of 100 would indicate that a country has every single element that we measure in place. A score of 0 indicates that none are in place. To be precise:

<img src="https://latex.codecogs.com/gif.download?SPI.INDEX_%7Bct%7D%20%3D%20%5Csum_%7Bd%3D1%7D%5E%7BN_D%7D%20%5Cfrac%7B%5Calpha_d%20%5Ctimes%20SPI.DIM_%7Bctd%7D%7D%7BN_D%7D" />


Where SPI.INDEX is the SPI overall index. &alpha;	is the weight for dimension d.  In order adjust the weights of a dimension, you can use the corresponding toggle in the bar on the right side of the screen.  In order to produce an overall score, we constrain the dimension weights to sum to 1.  This will happen in the background, so it is not obvious from the slider positions.

SPI.DIM are the 5 SPI dimensions listed above. In the notation, c is a country, t is the date, d is a dimension.  

The nested structure of our index and the summation methods used to build an overall score ensure the axiomatic properties outlined in Cameron et al (2019).  These include symmetry, monotonicity, and subgroup decomposability.
