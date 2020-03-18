# Regression Discontinuity Design

### Abstract
The regression discontinuity (RD) design is a branch of the observational study, which locally resembles the randomized experiment and is used to estimate the local causal effect of a treatment that is assigned fully, or partly by the value of a certain variable and a threshold. As the RD design is the subject of the causal inference, important concepts of the causal inference are covered to properly proceed discussions. Based on those concepts, the fundamental idea and structure of the RD design are explained including two sub types of the design: the sharp and the fuzzy RD designs. Furthermore, the assumptions of the RD design is formulated, which have been slightly different in different fields. In order to accurately estimate the local causal effect without confounding, we introduce the bandwidth and use the data that are within the bandwidth away from a threshold only. Since there is still no settled way of finding a "good" bandwidth, we propose a novel approach for bandwidth selection along with two existing methods. Performances of these bandwidth selection methods are compared with simulated data, and it can be inferred that the newly proposed method may yield better results. At the end, we intentionally violate the unconfoundedness assumption and analyze three potential confounding models with simulated data.

**Keywords:** Regression discontinuity design; Causal inference; Observational study; Randomized Experiment; Bandwidth selection; Sensitivity analysis

#### This repository contains the **R** source codes that were used to generate the figures and results of my thesis. Names of the **R** codes with corresponding figures or algorithms in the paper are listed below.

* RandomExpSolvesConfounding.R : Figure 2.2, 2.3
* AssignmentProbabilityConditionalExpectations.R : Figure 3.1, 3.2, 3.3, 3.4
* LocallyParamApp.R : Figure 4.1
* EconCVFigure.R : Figure 4.2
* BandwidthSelectionResults.R : Figure 4.3, Tabular Results in Appendix A
* EstimationFRD.R : Figure 4.4, 4.5
* XYconfounding.R : Figure 5.3
* XTconfounding.R : Figure 5.5
* EconCV.R : Cross Validation Approach (Ludwig and Miller, 2005; Imbens and Lemieux, 2008)
* mywindow.R : Newly Proposed Method
