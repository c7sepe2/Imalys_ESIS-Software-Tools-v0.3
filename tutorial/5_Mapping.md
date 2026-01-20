**[Home](../README.md) «» [Manual](../manual/README.md) «» [Tutorial](../tutorial/README.md) «» [Background](../background/README.md) «» [Source](../source)**

[TOC]

------

				## 5 Classification and pattern analysis

Imalys implements methods to map image features of all kinds into separate groups or clusters ([mapping](../manual/9_Mapping.md)). The result reflects feature combinations that are common in the classified image. *Mapping* can classify images at three levels: 

1. Pixel based using the spectral combination of single pixels
2. Zone based using the combination of [zone](../manual/7_Zones.md) attributes
3. Object based using the connections between classified *zones*

The purely statistical object classification without any training was sufficient to separate roof surfaces, streets, parks, small gardens and other urban elements from two very different images of the city of Leipzig. Fig.5.1 shows the result for a satellite image, Fig.5.5 the result for an areal image of the Gohlis district in Leipzig (Germany). In both cases identical parameters of the *mapping* command were applied.

![](../images/T5_Leipzig.png)

*Fig.5.1: Attributes and spatial patterns of 30 self-calibrating object classes of Leipzig (Germany) were assigned to 16 final classes ⬥ Image data: Sentinel-2 ⬥ Date: 16.9.2018 ⬥ Bands: 8-4-3-2 ⬥ Process: Mapping ⬥ Manually assigned colors*

------

The principle is the same in each case. Features or properties create a multidimensional feature space. Local concentrations of feature combinations in this n-dimensional space are detected and separated into a restricted number of classes. 

------

### 5a Pixel based classification

The unsupervised classification of pixels based on spectral combinations is a standard procedure ([mapping](../manual/9_Mapping.md)). The result mainly depends on the selection and quality of the images (see [reduce](../manual/5_Reduce.md)).

![](../images/T5_Automap.png)

*Fig. 5.2: Result of the pixel based mapping as shown in the tutorial 5a. 14 most common spectral combinations are assigned to 14 classes ⬥ Sensor: Landsat 8 ⬥ Time: May to October 2022 ⬥ Bands 2 – 7 ⬥ Manually assigned colors*

------

Deriving basic land use types from spectral image data can be unreliable because land use types are defined by their purpose and not by their appearance in the image. Machine learning can recognize almost any pattern, but needs to be trained using examples. Only trained patterns can be recognized. The training might even take longer than manual classification.

```
IMALYS [tutorial 5a]
home
	directory  =  ~/.imalys
	clear  =  true
	log  =  ~/ESIS/results
compile
	select  =  ~/ESIS/results/bestof_20220501-20220731.tif
	select  =  ~/ESIS/results/bestof_20220801-20221031.tif
mapping
	select  =  compile
	classes  =  14
	samples  =  30000
export
	select  =  mapping
	target  =  ~/ESIS/results/automap.tif
```

Tutorial 5a shows how to classify a multispectral and multitemporal image for 14 spectral clusters. The images in the *results* folder must exist. The *samples* are used to train the classifier. The number of the *samples* should be at least 1000 times larger than the *classes*. If rare features should be classified, a higher number might be necessary. [Mapping](../manual/9_Mapping.md) stores the result image and the class definition to the working directory. [Export](../manual/11_Export.md) saves both as to the *results* directory of the user.

------

### 5b Zone based classification

[Zones](../manual/7_Zones.md) must be created before the classification but *zones* [features](../manual/8_Features.md) can provide additional properties derived from the size, shape and connections of the *zones*. The spectral features of *zones* are the mean of all pixels that contribute to the *zone*. *Zones* and spectral classes complement each other. *Zones* summarize typical pixel features for a limited area. Classification sorts them into a manageable list of feature combinations. Typical problems with clustering pixels such as “pepper and salt” patterns do not occur anymore. Boundaries are clearly delineated.

![](../images/T5_Mapping.png)

*Fig. 5.3: Result of the zone's based mapping of the tutorial 5b. 16 most common feature combinations are assigned to 12 classes ⬥ Sensor: Landsat 8 ⬥ Time: May to October 2022 ⬥ Bands 2 – 7 ⬥ Manually assigned colors*

------

The spectral differentiation of natural areas becomes more reliable when images from different seasons are used together. Images of equal seasons may originate from different years. A logical combination of very different parameters, including the color, shape, distribution and development of surface characteristics, may provide a robust alternative to machine learning without extensive training (Banzhaf 2018).

[Zones](../manual/7_Zones.md) and their features provide an abstraction of the image that makes classification easier. Both lessons 5a and 5b use the same input data and the same number of classes. The spacial abstraction provided by the *zones* combines with the spectral abstraction by the classification process to return a superior result.   

```
IMALYS [tutorial 5b]
home
	directory = ~/.imalys
	clear = true
	log = ~/ESIS/results
compile
	select  =  ~/ESIS/results/bestof_20220501-20220731.tif
	select  =  ~/ESIS/results/bestof_20220801-20221031.tif
zones
	select = compile
	bonds = low
	size = 30
features
	select = compile
	execute = normal
	execute = entropy
mapping
	select = index
	classes = 16
	samples = 30000
export
	select = mapping
	target =  ~/ESIS/results/mapping_2022.tif
```

Lesson 5b shows how to classify zones instead of pixels. The input (*select*) is identical to the lesson 5a. To classify zones instead if images the raster part (*index* and *index.hdr*) of the *zones* definition must be called ([mapping](../manual/9_Mapping.md)). The classification result *mapping* can be exported as usual. Different classes are visualized by a color palette.

------

### 5c	Object based classification

Classified zones can be combined by a second level classification to form “objects” ([mapping](../manual/9_Mapping.md)). Objects are only defined by the intensity of the connections between the different *zones*. The connection intensities form patterns that can be classified. The result describes typical patterns of different spectral and spatial properties. These classes are hereinafter referred to as “objects”. For details refer to the [background](../manual/Background.md) section.

![](../images/T5_Objects-8.png)

*Fig. 5.4: Result of the tutorial 5c with 8 self adjusting object-classes to generalize the city of Munich and its environment. The process differentiates between settlement, permanent green, forest, agriculture and water ⬥ Sensor: Landsat 8 ⬥ Time: May to October 2022 ⬥ Bands 2 – 7 ⬥ Manually assigned colors*

------

The objects size is not limited. Simple patterns of smaller [zones](../manual/7_Zones.md) can be repeated over a large area to form one *object*. Large *zones* usually form *objects* of one dominant zone and many smaller ligands. In practice the mean *size* of the *zones* can be crucial. The *size* should be selected in such a way that homogeneous *objects* are not reduced to a lot of partitions, whereas heterogeneous *objects* can be represented by a sufficient number of smaller *zones*.

As *objects* can consist from a few to several hundred [zones](../manual/7_Zones.md) the supplied images must be large enough to provide sufficient training *samples* in order to recognize typical patterns. The images in this tutorial are rather small. Many combinations of color and shape remain isolated cases and cannot be trained properly. The tutorial with only eight different classes shows how basic structural types in the urban areas can be separated from each other. For the agricultural area a larger image would be required (see Fig. 5.1).

```
IMALYS [tutorial 5c]
home
	directory = ~/.imalys
	clear = true
	log = ~/ESIS/results
compile
	select  =  ~/ESIS/results/bestof_20220501-20220731.tif
	select  =  ~/ESIS/results/bestof_20220801-20221031.tif
zones
	select = compile
	bonds = low
	size = 30
features
	select = compile
	execute = normal
	execute = entropy
mapping
	select = index
	classes = 16
	samples = 30000
mapping
	select = index
	classes = 8
	samples = 80000
	fabric = true
export
	select = mapping
	target = ~/ESIS/results/objects_2022.tif
```

Tutorial 5c: The call of an *object* based classification is almost identical to a *zones* based even if the internal processes differ considerably. The only difference is the *fabric* option that triggers the search for connected zones. Higher values for the *fabric* parameter extend the area of the pattern detection process. In the above example the zones and then object classification are performed together. 

-----

### 5d Extended object features

Objects are not restricted to certain scales or spectral data. In order to compare the self adjusting *object* classification with a highly developed rule-based classification, the image data used in the study of Elze and Banzhaf (1) were classified as *objects*. 

![](../images/T5_Gohlis.png)

*Fig. 5.5: Object classification of the Gohlis district at the city of Leipzig (Germany). The spatial patterns of 500,000 [zones](../manual/7_Zones.md) were reduced by a self-calibrating analysis to 30 basic classes, which were assigned to 13 named classes ⬥ Image sources: Infrared aerial photos and elevation model of the State of Saxony ⬥ Normalized input values ⬥ Manually assigned colors*

-----

Due to the statistical base of the method, object definitions describe objects that are common in the passed image. Other landscapes or a different season can invalidate the definitions. This means that the results can only be transferred into images with the same structure. On the other hand the processing is extremely quick and does not require any training or prior information. The result is only determined by the image data. The technical implementation is described in detail in the [background]() chapter.

------

### 5e Comparison, class reference

ESIS offers the command [compare](../manual/10_Compare.md), which allows to compare any classification result with references. *Compare* uses a rank correlation to become independent of the data distribution. *Compare* returns two cross reference tables between the most recent classification result and a reference given as a raster image or as polygons.

-----

(1) **Elze, S.; Banzhaf, E:** High-precision monitoring of urban structures to understand changes in multiple ecosystem services. Urban For. Urban Green. 2022, 73, 127616.

[Top](5_Mapping.md)
