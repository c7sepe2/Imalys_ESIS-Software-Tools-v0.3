**[Home](../README.md) «» [Manual](../manual/README.md) «» [Tutorial](../tutorial/README.md) «» [Background](../background/README.md) «» [Source](../source)**

[TOC]

------

## *Zones:* Delineate homogeneous image elements

The *zones* command delineates a seamless network of image segments that completely covers the image. The process tries to minimize the pixel diversity within each zone. Zones were introduced to provide a structural basis for landscape diversity and other structural features. They allow an easy transformation of raster images to a vector format like maps. Zones are stored in an internal format to support rapid processing. Use the [Export](11_Export.md) command to get attributed polygons. The implementation of the *zones* follows the GEOBIA paradigm (Blaschke¹). 

The *zones* process can be controlled by the *size* and the *bonds* parameter. Technically the zones are created by an iteration that can be continued until the diversity within the zones can not be reduced further by combining zones. The resulting zones can be rather large. The *size* parameter will terminate the process when the mean of all zones has reached the input. The *bonds* parameter controls the size differences of the resulting zones.

Larger *zones* are always the combination of smaller *zones*. The number of input layers is not restricted. To control the result of the *zone* generation an ESRI shape file *index.shp* is created at the working directory. Zones can be classified and combined to larger *objects* (see [Mapping](9_Mapping.md)). 

------

### *Select* an image as basis of the zones process

```
IMALYS [zones]
…
zones
	select = compile
	…
```

The *zones* module needs an image stored at the working directory. Use *select* to assign the image. The *zones* module can work with classified and scalar images. Scalar images will be segmented in order to minimize the local variance of the pixel values. Classified images (maps) will be converted to a vector format. Each classified area will be translated to one zone.

------

### *Size:* Select the mean size of the zones

```
IMALYS [zones]
…
zones
	select = compile
	size = 30
    bonds = low
```

The density of zones is mainly controlled by the *size* parameter. The input is interpreted as mean size of all resulting zones and can be further qualified by the *bonds* parameter. The *size* of zones is counted in pixels. 

------

### *Bonds:* Select the variety of zones sizes

```
IMALYS [zones]
…
zones
	select = compile
	size = 30
	bonds = low
```

Depending on the landscape structure, individual *zones* of very different sizes can be created. *Bonds = low* will allow a wide range of sizes, *bonds = high* will force the result to almost equal sizes and *bonds = medium* is a compromise between both. *Bonds = accurate* only works properly with classified images and is used to vectorize them.

------

### *Sieve:* Merge small zones with larger ones

```
IMALYS [zones]
…
zones
	select = compile
	size = 30
	bonds = low
	sieve = 2
```

Very small zones like single pixels or short pixel rows may not be desired. The parameter *sieve* allows to merge small zones with larger ones. The passed number is interpreted as accepted pixels within the zone. `[sieve = 1]` will erase single pixels, `[sieve = 2]` will erase pixel pairs and so on. As dot shaped zones show more internal boundaries than a linear arrangement the process prefers narrow shaped zones. This process might need repeated calls.

------

(1) Thomas Blaschke,T. et al: Geographic Object-Based Image Analysis – Towards a new paradigm: ISPRS Journal of Photogrammetry and Remote Sensing 87 (2014) 180–191, http://dx.doi.org/10.1016/j.isprsjprs.2013.09.014

[Top](7_Zones.md)