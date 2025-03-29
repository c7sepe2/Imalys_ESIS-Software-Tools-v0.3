**[Home](../README.md) «» [Manual](../manual/README.md) «» [Tutorial](../tutorial/README.md) «» [Background](../background/README.md) «» [Source](../source)**

[TOC]

------

## *Catalog:* Create a position and size database for image archives

The *catalog* command selects and creates an image archives database at the passed directory. The [Import](3_Import.md) command can address uncompressed archives directly but with less comfort. 

The *catalog* saves the usable image area and the time of the data acquisition using the WKT format. [Import](3_Import.md) can use this information to quickly find suitable archives. *Catalog* will create a specific database *bonds.wkt* for each selected directory. The *bonds.wkt* files can be opened as a GIS layer to obtain an overview of the locally stored data.

------

### Archives: Select images from an archives collection

```
IMALYS [catalog]
…
catalog
	archives = ~/ESIS/archives/*_192023_*
```

A filename or a filename mask must be given to select one or several directories. In this case the parameter *archives* selects all Landsat tiles 192/023. All system specific wildchars like "?" or "*" will be accepted. Different directories can be used to separate sensor types or specific time periods. All archives that share the given mask will be accepted. 

[Top](2_Catalog.md)

