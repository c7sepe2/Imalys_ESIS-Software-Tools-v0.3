## Mapping

**classify image features and create image objects**

[TOC]

*Imalys* provides three mapping alternatives. A fully self adjusting classification based on pixel features will be performed if a standard image is *selected*. The same can be done with [zones](7_Zones.md), if the result of the *zones* process (*index*) is selected as input. If the *fabric* option is added, the zones will be further combined to objects that are characterized by the patterns of different *zones*.

In all cases the classification result is a raster layer called "mapping" with class IDs as values and random colors given as a color palette. 

With *select = compile* all bands of the image [compile](4_Compile.md) at the working directory are classified. The process depends on the Euclidean distance of the values in the n-dimensional feature space. As all bands are treated as equal important, the value distribution of the different bands should be similar. For images calibrated to reflection ([import](3_Import.md)) this is the case. Other bands like elevation or humidity will not fit. The *equalize* process can be used to scale all bands to the same value range.

With *select = index* the result of the most recent [zones](7_Zones.md) and [features](8_Features.md) process is assigned to the *mapping*. In this case *zones* and their *features* are classified instead of pixels. Spectral classification with *zones* instead of pixels are superior in most cases because *zones* follow natural boundaries and generalize pixel values. The typical “pepper and salt” effect of pixel orientated classes will not appear. Moreover the mapping of *zones* can include size, shape and context features of the zones. As the values of these features differ largely from reflection the image values should be scaled with the *equalize* process in accordance to the expected results.

The *fabric* process uses the classified zones to find and characterize spatial patterns (*objects*) among them (Appendix E: Background). The spatial distribution of *zones* and their features over the whole image is analyzed and most common patterns are extracted as *object* classes. This includes the typical neighborhood of *zones*. Many real objects are characterized more by their internal structure and their environment but by their spectral composition. *Objects* can model his composition. *Objects* are mostly larger than *zones*. The object definition does not restrict the size of *objects*. On the other hand large single *zones* like waterbodies can be classified as *objects*.

------

### Select (Process)

**Mark an image or zones to be classified**

```
IMALYS [mapping]
…
mapping
	select = compile
```

To classify a raster image a source like [compile](4_Compile.md) must be passed. If multiple images should be classified together, they must be stacked beforehand using the [compile](4_Compile.md) command.

```
IMALYS [mapping]
…
mapping
	select = index
```

To classify *zones*, the raster version of the *zones* at the working directory (*index*) must be selected. The process depends completely on the most recent created [zones](7_Zones.md) and their [features](8_Features.md). The resulting "index" and "topology" files must be stored at the working directory. 

```
IMALYS [mapping]
…
mapping
	select = index
	fabric = 1`
```

If the *zones* should be further combined to *objects*, the *fabric* parameter must be passed. *Fabric = 1* combines each *zone* with all neighboring *zones* to detect patterns. Larger numbers include the second and further spatial generations of neighbor *zones*.   

------

### Equalize (Process)

**scales all attribute values to the range [0 … 1]**
**only together with zones**

```
IMALYS [mapping]
…
mapping
	select = index
	equalize = 3
```

The *equalize* process scales all attributes of the most recent [features](8_Features.md) process to the same value range. Attributes with very different values can then be meaningfully compared. The input is given in standard deviations to fill the fixed value range [0 … 1]. *Equalize = 1* (one standard deviation) is the weakest binding, inputs around *3* bind 99.9% of all values to the same interval. *Equalize* does not permanently change the attributes.

------

### Classes (Parameter)

**Specifies the number of classes or objects to be created**

```
IMALYS [mapping]
…
mapping
	select = index
	classes = 30`
	samples = 50000
```

The number of classes at the result is set by *classes*. The number should not be too large. Overclassification reduces the accuracy. We recommend to use two *classes* per desired land use feature. *Imalys* classifies statistically. Coincidence can confuse a statistical analysis. Sometimes one *class* more or less can (!) significantly improve the quality.

------

### Samples (Parameter)

**Specifies the number of samples to train the classifier**

```
IMALYS [mapping]
…
mapping
	select = compile
	classes = 30`
	samples = 50000`
```

To find clusters in the feature space *mapping* uses *samples* from the image data. They are selected from the image at random places. *Samples* make the classification much faster than when each pixel or zone has to be evaluated individually. We recommend to use at least 1000 *samples* per desired class.

------

### Values (Parameter)

**Show classification result with “natural” colors**

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

[Previous](8_Features.md)	–	[Index](README.md)	–	[Next](10_Compare.md)