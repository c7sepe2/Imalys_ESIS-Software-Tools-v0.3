## Catalog

**Create a position and size database for image archives**

```
IMALYS [catalog]
…
catalog
	archives = ~/ESIS/archives/*_192023_*
```

The *catalog* command was implied to select appropriate archives for an [Import](3_Import.md) process effectively. *Import* can address archives directly with the *select* command but with less comfort. 

The “catalog” saves the usable image area and the time of data acquisition using the WKT format. Import can use this information to quickly find suitable archives. Catalog will create a specific database *bonds.wkt* for each selected directory. The *bonds.wkt* files can be opened as a GIS layer to obtain an overview of the locally stored data.

------

### Archives

**Select a directory and a subset of an image archives collection**

A filename or a filename mask must be given to select one or several directories. In this case the parameter *archives* selects all Landsat tiles 192/023. All system specific wildchars like "?" or "*" will be accepted. Different directories can be used to separate sensor types or specific time periods. All archives that share the given mask will be accepted. 

------

[Previous](1_Home.md)	–	[Index](README.md)	–	[Next](3_Import.md)

