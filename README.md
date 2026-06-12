## Papers

### Yields adjacent to hedge rows declined significantly towards the alley center. Tree rows contributed to stable crop yields under fluctuating water availability in their proximity and up to the alley center on their leeward side while yields significantly varied with changing climatic water balance on the windward side.

<details>
<ins>Source: </ins> 

```
Koch, O., Moore, J., Hörl, J., Cormann, M., Gayler, S., Lewandowski, I., Marhan, S., Munz, S., Pflugfelder, M., Piepho, H.-P., Schneider, J., von Cossel, M., Weinand, T., Winkler, B., & Schweiger, A. H. (2025). Sheltered by trees: Long-term yield dynamics in temperate alley cropping agroforestry with changing water availability. Agronomy for Sustainable Development, 45(3), 27. https://doi.org/10.1007/s13593-025-01022-5
```

Introduction:
- **Microclimate**: The regulating effects of woody components on the microclimate have been observed by various studies, namely the reduction of temperature extremes, wind speed, and evaporation (Schoeneberger et al. 2012; Kanzler et al. 2019; Swieter et al. 2022; Jacobs et al. 2022).
- **Competition**: However, competition for light, water, and soil nutrients between arable crops and woody components has been reported to negatively affect crop yields in agroforestry systems (Arenas-Corraliza et al. 2022).
- The reduction in photosynthetically active radiation caused by the tree stands has been shown to result in yield and quality decline (sometimes up to 50% yield decrease) in field crops such as various cereals and legumes (Reynolds et al. 2007; Dufour et al. 2013; Artru et al. 2017).
- **Gap for temperate climates**: While in a tropical, sub-tropical, and Mediterranean context the benefit of agroforestry for agricultural yields has been established for several production systems (Gomes et al. 2020; Arenas-Corraliza et al. 2022) and is projected to be of increasing importance in the face of climate change (Cardinael et al. 2021), there is still a large research gap for temperate climates.
- **Windbreak**: Additionally, the windbreak and shade provided by the tree rows can reduce **water loss** and improve **soil moisture retention** due to the lower radiation intensity, **reduced wind speed**,** decreased surface run-off**, and more **moderate soil temperatures** (Bird 1998; Quinkenstein et al. 2009; Jacobs et al. 2022).
- **Variation**: Variation among results (yield, water use, microclimate effects, soil effects, etc.) in agroforestry studies is often attributed to differences in tree stand and management (Luedeling et al. 2016; Jacobs et al. 2022).
- **Tree Differences**: Water competition has been shown to vary depending on the tree selection, crops, canopy structure, and of course rainfall patterns (Wang et al. 2021).
- **Distance to trees**: Furthermore, the spatial distance between the tree rows may play a key role in crop yield and performance due to its influence on competition factors (Quinkenstein et al. 2009; Ivezić et al. 2021).
- **Available models**: Although models have been developed to predict tree–crop interactions, yields, ecosystem services, and profitability of agroforestry systems, additional empirical research is needed to improve the accuracy of these models through better parameterization (Van Noordwijk and Lusiana 1999; van der Werf et al. 2007; Graves et al. 2011; Luedeling et al. 2016; Dupraz et al. 2019).

Methods:
- **Size**: 7.1 ha
-  Willow short rotation coppice (SRC): willow hybrid clone “Tordis” (Salix schwerinii × viminalis), planted in three double rows spaced 2 m between each other. Within one double row, single rows are spaced 0.75 m apart, spacing along the row is 0.6 m (Figure 2 A). Distance of tree rows to the crop alley is ~0.9 m. Willows are harvested every 3 years.
-  Walnuts: productive nut-bearing Juglans regia variety “Weinheimer (No. 139),” planted in double rows with rows spaced 4 m apart and trees at 7.5 m distance to each other. Distance of tree rows to the crop alley is 2 m (Figure 2B).
-  Autochthonous hedge: features a diverse mix of woody species, typical for their occurrence in the agricultural landscape. The hedge was planted in three parallel rows with a spacing of 1.5 m between the rows and the plants within each row. Distance of hedge rows to the crop alley is 2.5 m (Figure 2C).
- **Climatic variable**: Incorporating water–energy interactions, water-balance-based variables are proposed as meaningful estimates of the hydrologic and energetic environment experienced by plants (Stephenson 1990; Fisher et al. 2011). For this purpose, CWB was calculated as the difference between precipitation and potential evapotranspiration from late spring to summer (between May and August) (Figure 3). The time period was selected as relevant for the assessment of tree row effects on crop yields with respect to the timing of tree leaf development in spring and sensitivity of crops to drought in summer (Gobin 2012).
- **Model**: Multi-level modelling is proposed as a feasible approach to address spatial and temporal complexity which challenges statistical analysis of agroforestry trials (Golicz et al. 2023).
  - Models were built with the **glmmTMB-R-package** (Brooks et al. 2024).
  - **response**: crop yield
  - **fixed effect**: Blocks (BLOCK), cropping practice, distance to tree rows, and aspect, CWB with linear and quadratic terms as well as their interaction with alley cropping practice, aspect (ASPECT), i.e., crops grown east or west from the tree row, and distance classes (DIST) 0–6, 6–12, 12–18, and 18–24 m from the tree row. Additionally, both the crop species (CROP) and age (AGE) were used as fixed covariates in order to distinguish their effect from that of CWB.
 - **random effects**: plots (PLOT)
  - **Response**: YIELD ~
  - **Fixed**: BLOCK + CROP + TREAT + DIST + ASPECT + TREAT:DIST + TREAT:ASPECT + DIST:ASPECT + CROP:DIST + CROP:ASPECT + TREAT:ASPECT:DIST + AGE + CWB + I(CWB^2) + TREAT:CWB + TREAT:I(CWB^2) + DIST:CWB + DIST:I(CWB^2) + ASPECT:CWB + ASPECT:I(CWB^2) + TREAT:DIST:CWB + ASPECT:DIST:CWB + TREAT:ASPECT:CWB + TREAT:ASPECT:DIST:CWB
  - **Random intercept**:  (1|YEAR) + (1|BLOCK:YEAR) + (1|TREAT:YEAR) + (1|DIST:YEAR) + (1|ASPECT:YEAR) + (1|TREAT:DIST:ASPECT:YEAR) + (1|BLOCK:PLOT) + (1|BLOCK:PLOT:YEAR)
  - **Variance**: The type II Wald chi-squared test was used for analysis of variance.
  - **Pairwise post hoc comparisons**:  emmeans-R-Package (Lenth 2024)
  - **Explained variance**: Moreover, we quantified the explained variance for LMMs via the “r.squaredGLMM” function from the MuMIn-R-package (Bartoń 2023), yielding a marginal R2 (mR2) and a conditional R2 (cR2) based on Nakagawa et al. (2017). While the latter quantifies the variance explained by the fixed effect and the random effect combined, the marginal R2 solely accounts for the variance explained by the fixed effect.

Results: 
- significant differences in yield dynamics among the studied winter crops: With a marginal R2 of 0.68, the fixed effects explained substantial variation in the analyzed yield data. Adding the random effects resulted in a conditional R2 of 0.77.
- Yield dynamics within alley cropping agroforestry systems are shown to be significantly influenced by the proximity of crops to tree rows (Jacobs et al. 2022).
- **No effect of the water availability**,  **tree rows have a stabilizing effect on yields subjected to inter annual weather variability, both at proximity to tree rows on either side and up to the alley center on the leeward side**: CWB influenced yield dynamics in interaction with distance and aspect to trees (DIST:ASPECT:CWB, Table 1), but no significant variation with yield data as a single quadratic predictor. The observed influence of CWB on yields did not significantly vary between tree components: thus, in this section, the alley cropping practices are looked at in composite.

Outlook and Limitations:
- Future studies integrating on-site microclimatic and edaphic measurements will help to further improve our understanding of tree–crop interactions and **differences between different weather conditions and agroforestry practices**. This will be addressed by subsequent research efforts at this study site. Moreover, gaining insights on water balances and water use complementarity between trees and annual crops in temperate agroforestry systems would benefit the optimization of climate resilient agroforestry systems and will be a targeted in follow-up investigations. Additionally, measurements of the tree components will be part of upcoming studies analyzing the productivity of the entire agroforestry variants.

To be taken into the model:
Crop Type,Distance to trees Tree species, Distance to Trees::Water Availability

</details> 




### A comprehensive support strategy should combine one-time investment support and enhanced maintenance support during establishment with mechanisms to secure premium pricing, e.g., through specialised markets or labelling schemes.

<details>
<ins>Source: </ins> 

```
Swatek, Simon, Eike Luedeling, and Prajna Kasargodu Anebagilu. “Agroforestry Adoption in Germany: Using Decision Analysis to Explore the Impact of Funding Mechanisms on System Profitability.” Agroforestry Systems 100, no. 6 (2026): 146. https://doi.org/10.1007/s10457-026-01522-7.
```
Introduction:
- Since 2023, Agroforestry (AF) is recognised as eligible for direct payments in Germany under the Common Agricultural Policy in the form of annual financial support (Eco Schemes) and regionally variable investment support.
- Over longer periods (15–20 years), AF becomes economically viable in all scenarios, although outcome uncertainty increases.
- The societal and environmental benefits AF provides are highlighted multiple times in the German CAP strategic plan (SP). Carbon sequestration, climate change adaptation, reduction of nitrogen leaching and enhancement of agricultural biodiversity are seen as key reasons for the promotion of AF in Germany (BMEL 2024b). This indicates acknowledgement of current scientific opinion on AF and ecosystem services (e.g. Veldkamp et al. 2023), and the intention of German policymakers to foster AF to enhance agricultural sustainability.

Case Study: 
- The AF system was established in 2022 on 10.14 ha of arable land, with loamy-sandy soil and no noticeable slope. 5.6% of the plot (0.57 ha) have been converted into 3 m-wide tree rows, spaced 30 m apart. Due to the asymmetrical shape of the plot, the rows vary between 60 and 180 m in length, adding up to a length of 1,890 m. A total of 473 apple trees of 9 different cultivars were planted. The varieties were chosen to be suited for the intended low-input cultivation method, with minimal use of fungicides, herbicides or insecticides. Plant protection measures are to be limited to using pheromone dispensers for codling moth (Cydia pomonella L.) control. The system is expected to reach maturity after around 9 to 12 years and will contain nearly closed rows of apple trees with wide, low canopies, giving the tree rows a hedge-like appearance. A drip irrigation system has been installed in all tree rows to optimize apple production as well as minimize competition for water between arable crops and fruit trees. The arable area of the AF system is conventionally managed with a crop rotation consisting of maize, winter wheat, winter barley and rape seed. Soil cultivation is minimized strategically, and where possible, no-till methods with the application of a total herbicide are used to prepare the field for sowing (Jan Große-Kleimann, personal communication, 23. January 2024). The analysed silvoarable apple AF system represents a typical temperate alley cropping approach combining annual arable crops with perennial fruit production. In temperate Europe, AF systems commonly take the form of alley cropping systems integrating fruit or nut trees, high-value timber trees or short-rotation coppice species with arable cropping or pasture (Hübner and Günzel 2026). The selected system, therefore, represents one of the main conceptual AF types currently considered in German policy.

National ES programme: 
The national ES programme under the CAP’s first pillar provides an annual payment of 200 € per hectare of wooded area for the maintenance of AF systems, with specific requirements (BMEL 2024c):
- Trees must not be scattered across the plot but arranged in at least 2 rows, spanning up to 25 m in width,
- Tree rows may cover 2 to 40% of the agricultural plot,
- The distance between two tree rows must be 20–100 m.
- Whenever the field borders a forest or specific landscape element, the distance between an AF tree row and the field edge must exceed 20 m.
- Harvesting biomass from the woody vegetation is permitted only in January, February and December.
- Specific tree species are prohibited from being planted in an AF system (negative list).

Mathematical Modelling: 
-  decisionSupport package in R
 

Conclusion: 
- A comprehensive support strategy should combine one-time investment support and enhanced maintenance support during establishment with mechanisms to secure premium pricing, e.g., through specialised markets or labelling schemes. Integrated support is vital to ensure the short- to long-term viability of fruit-based AF systems similar to the one modelled in this study.

</details> 


### Multilevel models are underused and under-described in agroforestry system analysis 
<details>
<ins>Source: </ins> 

```
Golicz, Karolina, Hans-Peter Piepho, Eva-Maria L. Minarsch, et al. “Highlighting the Potential of Multilevel Statistical Models for Analysis of Individual Agroforestry Systems.” Agroforestry Systems 97, no. 8 (2023): 1481–89. https://doi.org/10.1007/s10457-023-00871-x.
```

Introduction: 
- Agroforestry systems are more complex in comparison to other agricultural land-use systems such as monocropping systems or annual monocultures because of biological interactions between their components (Scherr 1991; Jacobs et al. 2022). In addition, each component, i.e., the trees and crops or grassland, requires its own management which accounts for agricultural cycles that take place at different spatiotemporal scales (Scherr 1991). This inherent complexity hinders the application of traditional statistical methods in the analysis of agroforestry experiments (Balandier and Dupraz 1998; Birteeb et al. 2020) as many of the available statistical tools were developed for the analysis of bio-physico-chemical properties in agricultural experiments involving annual crops (Verdooren 2020). 
- Trees have been found to influence neighboring treatments above- and belowground (Somarriba et al. 2001) with microclimate effects, e.g., changes in wind speeds, reaching over distances of up to 30 times the tree height (Böhm et al. 2014). Furthermore, long time periods are required to accommodate management activities (Balandier and Dupraz 1998; Lovell et al. 2018). Considering the limited resources in terms of land, labor, and funds; designing an agroforestry experiment, with enough replications or control treatments to allow for a robust statistical analysis, might not be feasible (Stamps and Linit 1998).
- a point-transect design, where samples are collected in the tree row and at defined distances from the tree trunk in the arable or grassland strip. This approach leads to a hierarchical data structure characterized by a spatial autocorrelation within and between transects and to pseudo-replication if samples collected at different distances are treated as replicates (Stamps and Linit 1998). 

(see Supplementary Material 1 for site description and additional explanations of the model set-up as well as the final conclusions)

Selection and classification of variables:
- Spatial and temporal scales are important in agroforestry research because the effects of trees on and the interactions with their surrounding environment intensify as the system matures (Fig. 1; Step 1 of the R script: Selection and classification of variables).
- In agronomic experiments, Piepho et al. (2003) recommended for each experimental unit to be represented by a **random effect** in the model, i.e., random effects can represent individual plots, which can also be applicable to agroforestry systems, provided multiple measurements per plot were collected.
  
</details> 

### Agroforestry literature in the European Union: a bibliometric review and content analysis of key research areas and developments from 1984 to 2025

<details>
<ins>Source: </ins> 

```
Blake-Rath, R., Seegers, R., Grote, U., & Nguyen, T. T. (2026). Agroforestry literature in the European Union: A bibliometric review and content analysis of key research areas and developments from 1984 to 2025. Agroforestry Systems, 100(2), 55. https://doi.org/10.1007/s10457-026-01437-3
```

(1) How has agroforestry research in the EU evolved over time and space?
- research activity has increased substantially, with 42% of articles published between 2021 and 2025, identifying a geographical focus in Spain, Italy, Portugal, Germany, and France; 

(2) What are the most common types of agroforestry systems and structures studied?
- silvopastoral systems dominate the literature, whereas agrosilvopastoral practices receive less attention, with studies focusing on dehesa landscapes, alley cropping, and orchards as the most frequently investigated agroforestry structures

(3) What are the key research areas covered in the literature?
- research areas are strongly oriented toward ecosystem services, especially regulating and provisioning services, while cultural services, economic dimensions, and stakeholder perspectives are still underrepresented, despite their recognized importance for the wider adoption of agroforestry systems.

- The final dataset for the bibliometric analysis (1984–2025) thus encompasses a total of 902 publications. Details of the articles such as title, country, authors, year of publication, and journal can be found within the supplementary information.
- The average impact factor (2024) across these journals is 4.5, ranging from 1.4 (e.g., Communications in Soil Science and Plant Analysis) up to 8.4 (Journal of Environmental Management). Agroforestry Systems is the primary publication venue, contributing 119 articles (13.2%), followed by Sustainability (3.8%) and Forests (3.1%).

</details>

### European agroforestry has no unequivocal effect on biodiversity: a time-cumulative meta-analysis 
<details>
<ins>Source: </ins> 

```
Mupepele, A.-C., Keller, M., & Dormann, C. F. (2021). European agroforestry has no unequivocal effect on biodiversity: A time-cumulative meta-analysis. BMC Ecology and Evolution, 21(1), 193. https://doi.org/10.1186/s12862-021-01911-9
```
Agroforestry is a production system combining trees with crops or livestock. It has the potential to increase biodiversity in relation to single-use systems, such as pastures or cropland, by providing a higher habitat heterogeneity. 


- Overall, there was no benefit of agroforestry to biodiversity. A time-cumulative meta-analysis demonstrated the robustness of this result between 1991 and 2019. In a more nuanced view silvopastoral systems were not more diverse in relation to forests, pastures or abandoned silvopastures. However, silvoarable systems increased biodiversity compared to cropland by 60%. A subgroup analysis showed that bird and arthropod diversity increased in agroforestry systems, while bats, plants and fungi did not.
- In our study we also found that other **environmental variables** have an influence on the agroforestry-biodiversity relationship.
- Our review suggests weak effects, and we are only moderately confident about these findings, supposing that the main driver for biodiversity cannot be found in agroforestry but may lie at **the landscape scale** or be dependent on land-use history.

</details> 

### A protocol for data exploration to avoid common statistical problems
<details>
<ins>Source: </ins> 

```
Source
```
With this wealth of potential pitfalls, ensuring that the scientist does not discover a false covariate effect (type I error), wrongly dismiss a model with a particular covariate (type II error) or produce results determined by only a few influential observations, requires that detailed data exploration be applied before any statistical analysis.

- Data Exploration can take up to 50% of the time spent on analysis.

1) Step 1: Are there outliers in Y and X?
- outliers may cause overdispersion in a Poisson GLM or binomial GLM when the outcome is not binary
- 

2) Step 2: Do we have homogeneity of variance?

3) Step 3: Are the data normally distributed?

4) Step 4: Are there lots of zeros in the data?

5) Step 5: Is there collinearity among the covariates?

6) Step 6: What are the relationships between Y and X variables?

7) Step 7: Should we consider interactions?

8) Step 8: Are observations of the response variable independent?

</details> 

### A systematic method for hypothesis synthesis and conceptual model development
<details>
<ins>Source: </ins> 

```
Grames, E. M., Schwartz, D., & Elphick, C. S. (2022). A systematic method for hypothesis synthesis and conceptual model development. Methods in Ecology and Evolution, 13(9), 2078–2087. https://doi.org/10.1111/2041-210X.13940

```

- It is critical at the outset of any synthesis to clearly identify the objectives and use explicit frameworks to formally define the scope of the research question for the review. Increasingly, frameworks used in systematic reviews include the population (P), interventions (I) or exposures, comparator groups (C) and outcomes (O), with expansions that include time (T) frames, space (S) or geographic locations of interest and consideration of moderator variables. In our case study, a recent systematic review on the effects of forest fragmentation on birds (Grames, 2021), we used TOPICS+M to define the scope of the question as studies that took place during the breeding season (time), that measured edge or area sensitivity (outcome) in forest songbirds (population) within forest patches of different sizes (intervention) across at least two levels (comparator). Furthermore, we required that studies be conducted in boreal or temperate forests of North America (space) to reduce variation inherent to natural systems, and we identified variables related to taxa, geographic location, forest types and surrounding matrix (moderators), which could explain patterns in mechanisms. Not all research questions will have constraints across all these question components; in many cases, the PICO approach may be sufficient.
- Researchers should conduct a reproducible search of the literature and systematically screen articles to determine if they meet the inclusion criteria defined in the previous stage, rather than doing an ad hoc review of the literature.
- The subsequent stages of our approach could be done with any set of articles identified by the research team; however, we advocate for selecting articles systematically to ensure that the resulting model is as complete as possible.
- the data of interest from each article are the authors' hypotheses.

</details> 

### Finding 
<details>
<ins>Source: </ins> 

```
Scordia, D., Corinzia, S. A., Coello, J., Vilaplana Ventura, R., Jiménez-De-Santiago, D. E., Singla Just, B., Castaño-Sánchez, O., Casas Arcarons, C., Tchamitchian, M., Garreau, L., Emran, M., Mohamed, S. Z., Khedr, M., Rashad, M., Lorilla, R. S., Parizel, A., Mancini, G., Iurato, A., Ponsá, S., … Testa, G. (2023). Are agroforestry systems more productive than monocultures in Mediterranean countries? A meta-analysis. Agronomy for Sustainable Development, 43(6), 73. https://doi.org/10.1007/s13593-023-00927-3
```
- Most of the reviewed studies agree on the **negative effect of tree shading on crop yield**
-  The tree species was also a significant effect to consider: evergreen species are usually more impacting than deciduous ones, but care must be also taken with tree traits and phenology (growth speed, canopy density, branchiness, date of budbreak, and root distribution), as well as the tree age and size, density, diversity (taxonomic and functional), and orientation, among others.
-  Despite the reduction in crop yield in agroforestry compared to sole crops, trees may lead to a diversification and increase in overall productivity, therefore reducing farmers’ vulnerability to markets.
-  trees can mitigate the effect of extreme climate events due to hydraulic lift, as shelter from heat waves and preventing lodging from strong winds, provided that both crop and tree cycles are staggered, root systems explore different soil layers, and there are no effects from a phytosanitary point of view.
-  **Differences between crops** relation to agroforestry: In general, barley and oat seem suitable cereals for agroforestry systems mainly due to their short cycle, drought tolerance, and low-input demand. Forage and pasture are extensively used in agroforestry and have been indicated as species with a less than proportional loss of yield at low levels of shade. However, it is essential to manage through sowing forage mixtures and fertilization plan to balance floristic composition, persistence, and productivity. Grain legumes were very susceptible to agroforestry due to shading, while wheat response mostly depended on the cultivar, seasonal climatic conditions, and tree-type association.

</details> 

### Competition and yield in oil palm agroforestry: examining the ‘yield penalty’ of biodiversity (Master Thesis)
<details>
<ins>Source: </ins> 

```
Source
```
- Pure Oil palm plantation, no crop
- The time series suggests a steady yield decline among all enriched plots throughout the period.
- Yet, plots with no trees planted but natural undergrowth development and suspension of artificial fertilization have shown above-average yield per area since initial palm thinning.

</details> 


### Habitat heterogeneity and socioeconomic factors shape the spatial patterns of ancient trees on Hainan, China
<details>
<ins>Source: </ins> 

```
Li, Q., Yuan, J., Cao, Q., Padullés Cubino, J., Nizamani, M. M., Zhu, M., Wang, G., Bai, Y., & Wang, H. (2026). Habitat heterogeneity and socioeconomic factors shape the spatial patterns of ancient trees on Hainan, China. Journal of Forestry Research, 37(1), 88. https://doi.org/10.1007/s11676-026-02027-w
```
- ancient trees
- Spatially, the trees exhibited significant clustering, with higher densities in western and northern regions and markedly lower concentrations in highly urbanized coastal zones.
- Habitat heterogeneity, particularly the difference between maximum and minimum annual precipitation (MAPR), emerged as the predominant predictor of species richness and abundance across most age classes.
- distribution of the youngest ancient trees (Grade Ⅲ) and overall tree density were primarily associated with socioeconomic factors, especially accessibility.


</details> 

### Crop Yields in European Agroforestry Systems: A Meta-Analysis
<details>
<ins>Source: </ins> 

```
Ivezić, V., Yu, Y., & Werf, W. van der. (2021). Crop Yields in European Agroforestry Systems: A Meta-Analysis. Frontiers in Sustainable Food Systems, 5. https://doi.org/10.3389/fsufs.2021.606631
```

- Crop yields in alley cropping decreased on average with 2.6% per year over the first 21 years of the tree stand, indicating increasing competitive effects of the trees with their age.
- **AF system**: Dehesa and Montadoa re less competitive than alley cropping systems
- **Tree density**: Relative yield decreased with the density of trees whereby an increase in tree density by 100 trees per ha was associated with a decrease of relative yield by 20%
- **Tree age**: There was a negative effect of tree age on relative crop yields for both cereals and fodder crops, but the intercepts were significantly different. An increase of tree age with 1 year would result in a decrease of relative yield by 2.6% for both types of understory crop.
- **Distance to trees**: In alley cropping, **the half distance between tree lines** is a useful proxy for the distance between the crop and the trees. Therefore, model for distance is not reliable. We also analyzed the interaction of distance and tree age and found that there was no interaction.

</details> 

### Landscape heterogeneity analysis using geospatial techniques and a priori knowledge in Sahelian agroforestry systems of Senegal
<details>
<ins>Source: </ins> 

```
Ndao, B., Leroux, L., Gaetano, R., Diouf, A. A., Soti, V., Bégué, A., Mbow, C., & Sambou, B. (2021). Landscape heterogeneity analysis using geospatial techniques and a priori knowledge in Sahelian agroforestry systems of Senegal. Ecological Indicators, 125, 107481. https://doi.org/10.1016/j.ecolind.2021.107481
```

- However, agroforestry systems (AFSs) are particularly heterogeneous in sub-Saharan Africa due to small to very small fields, a large variety of agricultural practices and a diversity of parkland compositions and configurations.

Model Variables: 
- Two ecophysiological variables, namely, vegetation productivity and its dynamics during the 2000–2015 period, were derived from 16-day MODIS NDVI time series (MOD13Q1; spatial resolution: 250 m) (Didan, 2015). The average annual integral of NDVI was computed and used as an indicator of the overall vegetation productivity. 
- An agrometeorological variable, namely the actual evapotranspiration (AET), which allows the soil-air interface and plant functioning (WMO, 2012) to be considered, was extracted from the FAO WaPOR database (https://wapor.apps.fao.org/home/1).
- A woody cover map derived from MODIS FAPAR (Fraction of Absorbed Photosynthetically Active Radiation from MODIS; Brandt et al., 2016) was also used to derive information related to tree density as an ecological and anthropic (related to the cropping practices) variable.
- For soil properties, a soil type map from the Institut National de Pédologie (INP –Senegalese National Institute of Soil Sciences – http://inp-senegal.com/) was used. It is an extract of a 1:500,000 soil type map at the national scale (INP, 2013). Soils were classified according to the CPCS’s classification (CPCS, 1967). In the study area, three main types of soils were distinguished: tropical ferruginous soils which usually correspond to dior and deck-dior soils, hydromorphic soils, and saline hydromorphic soils which are rather deck soils.

Landscape Heterogeneity:
- for agroforestry cites NDVI used as a proxy for productivity 

</details> 

### Finding 
<details>
<ins>Source: </ins> 

```
Source
```
</details> 

### Finding 
<details>
<ins>Source: </ins> 

```
Source
```
</details> 

### Finding 
<details>
<ins>Source: </ins> 

```
Source
```
</details> 

### Finding 
<details>
<ins>Source: </ins> 

```
Source
```
</details> 

### Finding 
<details>
<ins>Source: </ins> 

```
Source
```
</details> 


## Glossary:
- **Alley cropping** — the intentional integration of trees and crops with widely spaced tree rows and in-between crop alleys (Wolz and DeLucia 2018) — is an especially attractive form of agroforestry for temperate climates, as it offers a low-input system (via improved water management and nutrient cycling) for the simultaneous growth of food production and biomass for improved land use (Quinkenstein et al. 2009). [Arenas-Corraliza, María Guadalupe, María Lourdes López-Díaz, Víctor Rolo, Yonatan Cáceres, and Gerardo Moreno. “Phenological, Morphological and Physiological Drivers of Cereal Grain Yield in Mediterranean Agroforestry Systems.” Agriculture, Ecosystems & Environment 340 (December 2022): 108158. https://doi.org/10.1016/j.agee.2022.108158.]
