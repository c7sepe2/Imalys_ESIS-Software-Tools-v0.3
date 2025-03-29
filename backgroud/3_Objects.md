**[Home](../README.md) «» [Manual](../manual/README.md) «» [Tutorial](../tutorial/README.md) «» [Background](../background/README.md) «» [Source](../source)**

[TOC]

------

## 3 Object Generation

»»» Tight connection between shape and spectral properties

*Execute = fabric* under [mapping](../manual/) combines two classification processes. Pixels are combined to form zones, which are classified by means of their individual features and the resulting zones are combined to form objects according to the spatial combination (pattern) of the different zones.

»»» Classification

During the second step, the →Zone process classifies the characteristics of individual zones and assigns zone classes. In this case the classification only depends on the spectral characteristics of the zones and the normalized texture (→Normal) between zonal pixels. After the second step, zones map the spatial distribution of image features and the zone classes map the spectral distribution. The “fabric” process combines both to a class definition that includes size, shape and typical patterns of visible structures in the image.

### Object Definition

An object consisting of three zones (red-green-blue) with a total of 12 pixels is characterized by 17 contacts (yellow symbols). The object definition is based solely on the nature and frequency of these contacts.
