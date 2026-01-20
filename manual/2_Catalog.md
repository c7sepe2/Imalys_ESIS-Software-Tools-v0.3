**[Home](../README.md) «» [Manual](../manual/README.md) «» [Tutorial](../tutorial/README.md) «» [Background](../background/README.md) «» [Source](../source)**

[TOC]

------

## *Catalog:* Create an image collection database

The [catalog](2_Catalog.md) command creates a database for image archives in the same directory to be used by the [Import](3_Import.md) command. 

The *catalog* saves the bounding box of the images and the time of the data acquisition. [Import](3_Import.md) can use this information to quickly find suitable input images. The *catalog* is formatted as a GIS layer (WKT-format) to obtain an overview of the locally stored data.

***The catalog command is currently disabled***

------

### Source: Set an image catalog mask

```
IMALYS [catalog]
...
catalog
	source = ~/ESIS/archives/*_192023_*.tar
	target = ~/ESIS/archives/bonds
```

A filename or a filename mask (*source*) must be passed to select image archives to be included in the catalog. All system specific wildchars like "?" or "*" will be accepted. The name of the *catalog* (target) can be freely selected. *Catalog* stores the result in the same directory as the images.

In this example the parameter *source* selects all Landsat tiles 192/023 stored as archives (.tar) at the directory "ESIS/archives". All files that share the given mask will be processed. Different catalogs can be used to separate sensor types or specific time periods. Each directory requires its own *catalog*

------

### Target: Select an image catalog name

```
IMALYS [catalog]
...
catalog
	...
	target = ~/ESIS/stock/bonds
```

The “target” parameter takes the name of the catalog. The name can be freely selected. The *catalog* must be stored in the same directory as the image data.

[Top](2_Catalog.md)

