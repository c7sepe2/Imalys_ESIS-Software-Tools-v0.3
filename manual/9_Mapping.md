**[Home](../README.md) «» [Manual](../manual/README.md) «» [Tutorial](../tutorial/README.md) «» [Background](../background/README.md) «» [Source](../source)**

[TOC]

------

## *Mapping:* Classify image features and create image objects

*Imalys* provides several mapping alternatives. A fully self adjusting classification based on pixel features will be performed if a standard image is *selected*.

```
IMALYS [mapping]
...
mapping
	select = compile
	...
```

The same can be done with [zones](7_Zones.md) and their [features](8_Features.md), if the *index* of the *zones* is *selected* (see [tutorial](../tutorial/5_Mapping.md)). If the *fabric* option is added, classified *zones* will be combined to structured *objects*. 

```
IMALYS [mapping]
...
mapping
	select = index
```

All classifiers determine local focal points in the n-dimensional feature space (clustering). They train themselves and pass on a limited number of typical feature combinations for the transferred data. For image data, the features are spectral combinations. For *zones*, they are spectral, geometric and pattern indicators. The number of extracted classes is controlled by the *classes* parameter.

```
IMALYS [mapping]
...
mapping
	select = index
	runoff = true
	...
```

With the “runoff” parameter, *mapping* interprets the *zones* as micro-catchments from an elevation model (see [zones](7_Zones.md)) and calculates catchment areas and synthetic runoff. No further information is required for this.

Please follow the detailed instructions under the key words below.

------

### *Select* an image or zones to be classified

```
IMALYS [mapping]
...
mapping
	select = compile | index
```

The input data must be specified for each classification process. For pixel-based classification, this is any image in the working directory. Image data located elsewhere must first be transferred to the working directory using [compile](4_Compile.md). Short time series can greatly improve classification. In this case, the different images must be combined into one image using *compile* before classification.

If *zones* are to be classified, the *index* and *topology* of the *zones* must be stored in the working directory. The *index* is the raster version of the *zones* geometry, while the *topology* stores the spatial relationship between the *zones*. The classification uses all *features* of the selected [zones](7_Zones.md) equally. *Zones* and *features* that are suitable for the question should be carefully selected in advance and regenerated to suit the question (see *equalize*).

Combining *zones* into *objects* with the *fabric* parameter requires classified [zones](7_Zones.md). The *index*, *topology*, and *mapping* files must be stored in the working directory. *Zones* and all necessary attributes can be saved using the [export](11_Export.md) command and transferred back to the working directory using the [import](3_Import.md) command.

------

### *Classes:* Specify the number of classes or objects to be created

```
IMALYS [mapping]
...
mapping
	select = index
	classes = 30
```

*The input must be a positive number.*

The number of classes at the result is set by *classes*. The number should not be too large. Overclassification reduces the accuracy. We recommend to use two *classes* per desired land use feature. *Imalys* classifies statistically. Coincidence can confuse a statistical analysis. Sometimes one *class* more or less can (!) significantly improve the quality.

------

### *Samples:* Specify the number of samples to train the classifier

```
IMALYS [mapping]
...
mapping
	select = compile
	classes = 30
	samples = 50000
```

*The input must be a positive number.*

To find clusters in the feature space *mapping* uses *samples* from the image data. They are selected from the image at random places. *Samples* make the classification much faster than when each pixel or zone has to be evaluated individually. We recommend to use at least 1000 *samples* per desired class.

------

### *Fabric:* Combine zones to objects

```
IMALYS [mapping]
...
mapping.
	select = index
	classes = 14
	samples = 50000
	fabric = true
```

The *fabric* parameter extends an existing classification of [zones](7_Zones.md) to composite *objects* from different zones. *Objects* are defined only by the spatial pattern of neighboring zones. If the pattern repeats, objects can become arbitrarily large. Regardless of this, individual *zones* with unspecific surroundings, such as standing water, can be mapped as *objects* (see [Mapping](../background/B_Mapping.md) background). 

------

### *Reach:* Set the collection reach for fabric patterns

**Only together with *fabric***

```
IMALYS [mapping]
...
mapping
	select = index
	classes = 14
	samples = 50000
	fabric = true
	reach = 3
```

*The input must be a natural number.*

Sometimes complex patterns of [zones](7_Zones.md) cannot be described by direct neighbors alone. With *reach*, indirect neighbors can also be included in pattern formation. The value of *reach* corresponds to the number of layers of neighboring *zones* that are included in the pattern analysis. *Reach=1* (immediate neighbors) is the default and can be omitted. Large search areas can also lead to blurred classes. It may be more useful to change the size of the *zones* instead.

------

### *Equalize:* Scale all attribute values to the range [0..1]

**only together with zones!**

```
IMALYS [mapping]
...
mapping
	select = index
	equalize = 0.01
```

*Inputs between 0.0 and 0.5 are permitted*

The [features](8_Features.md) of the [zones](7_Zones.md) can show very different values ranges. If they should be meaningfully compared by a classification process, the values of all features have to be normalized. The *equalize* option scales the values of the current *features* to a value range between 0 and 1. Outliers can be excluded by the value of the *equalize* parameter.

*Equalize* sets the limits of the value range to the specified percentile [0.0 to 0.5]. The input is used equally for the upper and lower thresholds. “*Equalize = 0.01*” scales the values so that 1% of the largest and smallest values are NOT within the limits of 0.0 to 1.0.

*Equalize* does not permanently change the attributes.

------

### *Runoff:* Link zones according to a synthetic runoff

```
IMALYS [runoff]
...
mapping
	select = index
	runoff = true
```

The *runoff* parameter calculates a synthetic runoff between [zones](7_Zones.md) derived from an elevation model using the *elevation* parameter. *Runoff* adds the coordinates of the runoff points (“latitude,” “longitude”), the drained area in hectares ("weight"), and the ID of the next linked *zone* (“link”) to the attribute table of the *zones* in the working directory.

*Runoff* additionally translates the hydrological attributes into a layer of line vectors that schematically mark the runoff between the *zones*. Any point can be expanded into a catchment area. *Runoff* does not require any information about classes or samples. The results can be saved with [Export](11_Export.md) as *zones* or as a vector layer.

------

### *Release:* Allow runoff at image boundaries

```
IMALYS [runoff]
...
mapping
	select = index
	runoff = true
	release = true
```

Every elevation model has boundaries. When calculating synthetic runoff, the boundaries at the edge of the image act like a sheet pile wall. The *release* parameter opens this wall so that water can flow off at the edge of the image. The edges of the elevation model have a significant influence on the result in any case. Ideally, the boundaries of the elevation model should largely correspond to the catchment area. 

NoData areas within the elevation model are ignored. Runoff from or over NoData areas is not generated. It may be useful to bridge NoData areas by interpolation before processing.

------

### *Values:* Show the classification result with “natural” colors

```
IMALYS [mapping]
…
mapping
	select = index
	classes = 30`
	samples = 50000
	values = true
```

The primary classification result is a raster layer (*mapping*) with class IDs as values and random colors given as a color palette. The *values* option will transfer the random colors to “natural” colors derived from the class definition. The default colors are the first three bands of the classified image.

------

[Top](9_Mapping.md)