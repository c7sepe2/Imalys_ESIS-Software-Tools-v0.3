**[Home](../README.md) «» [Manual](../manual/README.md) «» [Tutorial](../tutorial/README.md) «» [Background](../background/README.md) «» [Source](../source)**

[TOC]

------

## Mapping

### Pixels, Zones and image Objects

Images of the earth’s surface are structured. Pixels with almost the same spectral combination are not randomly distributed but form clusters and sometimes regular patterns. The spectral combination of each image can be classified using the [mapping](../manual/9_Mapping.md) command. 

The [zones](../manual/7_Zones.md) command combines regions with pixels of almost the same spectral combination into *zones*. *Zones* have spectral features like pixels but the class definition can gain information from the size, shape and individual connections of the *zones*. A dark color combination of a small *zone* like a shadow might get a very different meaning in a large *zone* like a lake. *Zones* with additional features can also be classified using [mapping](../manual/9_Mapping.md) command. 

[zones](../manual/7_Zones.md) can be associated in a second step classification to form *objects*. *Objects* are defined by the specific patterns of the associated *zones* and allow to classify real world objects that are characerized by a specific combination of different elements (pattern recognition).

------

### Features from pixel's and zone's properties

In terms of zones two abstractions take place. One of them are spectral features. Zones have a spectral composition like pixels but the value is the mean of all pixels connected to one zone. Local differences or textures can not be expressed as a spectral attribute but can be added as additional feature. Imalys introduces local contrast and the distribution of the pixels within the zones as additional features that can support the pure spectral features (diversity, entropy, normal, texture under →features). 

In addition to spectral features the size, shape and connection to other *zones* might differ considerably für single *zones*. Features derived from the geometry and neighborhood of the *zones* can be added as attributes of the *zones* polygons (cellsize, dendrites, diffusion, proportion, relation under →features). 

------

### Control feature space dimensions

The basis of all [mapping](../manual/9_Mapping.md) processes is the Euclidean distance in the n-dimensional feature space, similar to the IsoClass method. The classification process at Imalys follows Kohonen’s suggestion (Kohonen¹) where each neuron represents a separate class. According to this concept the neurons have individual properties, not only connections as neurons of the perceptron type would have (Kriesel²). One of the individual properties is a receptive field, a section of the feature space in which the neurons can recognize features. As for the rest, they are blind. This property enhances their ability to depict small but common differences. The training methods were strongly influenced by the ROLF (Kriesel²) concept.

All classification processes under [mapping](../manual/9_Mapping.md) use cluster analysis. The processes recognize and separate frequent characteristic combinations of pixels or zones. *Mapping* measures the differences between the characteristics as Euclidean distance in the n-dimensional characteristic space. The values of the different [features](../manual/8_Features.md) can be very different. Reflectances are between 0 and 1, shape attributes can be negative and areas can have multi-digit values. Parameters with large values would dominate the classes if they were not normalized beforehand with *equalize* under [mapping](../manual/9_Mapping.md).

------

### Time as an attribute

------

### Pattern analysis to define "objects"

*Objects* (execute = fabric) are a spatial composition of different *zones*. If *zones* are classified according to their local →features the connection frequency of different *zones* can be used to define pattern classes among them. Add the *execute = fabric* parameter to combine *zone* and *opbject* processing under the →mapping command.

![](../images/B2_PixToObj.png)

*Three steps to define an image object: Pixels of similar spectral combination are combined into zones (1). Zones are classified into clusters with typical (spectral) features (2). Specific zone combinations are aggregated to form objects* (3).

------

The class definition only depends on the frequency of the boundaries between different classes of the *zones*. The frequency is counted as pixel connections. This includes all pixels and even pixels within the *zones*. As a consequence large *objects* will be defined mainly by the (spectral) features of their *zones*, small *objects* by the specific mixture of different *zones*. This principle introduces the necessary plasticity in the *object* definition and allows intermediate states between homogeneous and patterend *objects*.

![](../images/B2_Links.png)

*Objects* can consist of a simple pattern of two or three *zone* classes but continuously distributed over a large area like a forest. *Objects* can also consist of only one large but homogeneous *zone* like a traffic line and possibly accompanied by a few small ligands like a waterbody. In the first instance, the object is defined mainly by the connections between *zones*, in the second instance, the connection between internal pixels dominate the *object* definition and the connections to other *zones* play a minor role (see above). In practice there is a smooth transition between both extremes.

Since the *object* class definition only depends on different frequencies, each *zone* class may occur in each *object* class. Non-specific *zone* classes such as shadows can be defined in almost all *object* classes. They are not characteristic for the *object*, but make it complete.

------

### Citations

(1) Teuvo Kohonen: Analysis of a Simple Self-Organizing Process: Biol. Cybern. 44, 135-140 (1982)

(2) David Kriesel: Neuronale Netze: http://www.dkriesel.com/science/neural_networks

------

[Top](B_Mapping.md)



### Themen

**was wird klassifiziert? ← Spektralkombinationen, Features, Höhe bei DEMs**

**Basis ist der n-nimensioale Merkmalsraum, egal ob Pixel oder Merkmale**

**Features können sehr unterschiedliche Wertebereiche haben → Ausgleich vorsehen**

**Zonen kombinieren Grenzen UND innere Merknmale UND Kontext ← mehr Information als Pixel**

With *select = compile* the spectral combinations of the image [compile](4_Compile.md) are classified. The process depends on the Euclidean distance of the values in the n-dimensional feature space. As all bands are treated as equal important, the value distribution of the different bands should be similar. For images calibrated to reflection or radiation ([import](3_Import.md)) this is the case. Other bands like elevation or humidity will not fit. The *equalize* process can be used to scale all bands to the same value range (see [background·mappgenerations ing](../background/B_Mapping.md)).

With *select = index* the result of the most recent [zones](7_Zones.md) and [features](8_Features.md) process is assigned to the *mapping*. In this case *zones* and their *features* are classified instead of pixels. Spectral classification with *zones* instead of pixels are superior in most cases because *zones* follow natural boundaries and generalize pixel values.

=== boundaries as preclassification ===

The typical “pepper and salt” effect of pixel orientated classes will not appear. Moreover the mapping of *zones* can include size, shape, context and time features of the zones. As the values of these features differ largely from reflection the image values should be scaled with the *equalize* process in accordance to the expected results.~~

~~The *fabric* process uses the classified zones to find and characterize spatial patterns among them (see see [background·objects](../background/B_Objects.md)). The spatial distribution of different *zones* and their vicinity is analyzed and most common patterns are extracted as *object* classes. Many real objects are characterized more by their internal structure and their environment but by their spectral composition. *Objects* can model his composition. The object definition does not restrict the size of *objects*. On the other hand large single *zones* like waterbodies can be classified as a single *object*.
