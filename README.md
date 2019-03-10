# Package for "Reducing Commercial Aviation Fatalities" Kaggle competition

[Relevant Kaggle Competition](https://www.kaggle.com/c/reducing-commercial-aviation-fatalities)

I wanted to write a basic package to get a better understanding of building packages in R. This package will not be published to CRAN as it is extremely specific and will not be maintained going forward.

**This directory is a fully functional package and therefore can be installed as follows.**

```r
install.packages("devtools")
devtools::install_github("zschuster/Kaggle.PKG.Aviation")
```
## Package Contents

The package can be broken down into three main areas. Each area is bulleted with sub bullets for functions within that section.

* Data import
  * `readAndSpit`
* Data preprocessing
  * `processTrainData`
  * `processTestData`
* helper functions (mainly used in above functions)
  * `coerceClass`
  * `multiCodeVars`
  * `formatXgbProbs`
  
## Example

The analysis for the Kaggle competition (linked above) can be found [here](https://github.com/zschuster/kaggle.aviation "another one of my repos")

## Feedback

I welcome any feedback (positive or negative) and suggestions that you may have. Please direct any feedback or suggestions to <ztschuster@gmail.com>
