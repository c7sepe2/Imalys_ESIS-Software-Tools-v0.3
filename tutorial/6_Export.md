**[Home](../README.md) «» [Manual](../manual/README.md) «» [Tutorial](../tutorial/README.md) «» [Background](../background/README.md) «» [Source](../source)**

[TOC]

------

## 6 Raster and vector export

[Export](../manual/11_Export.md) allows to save internal results to a selected place. *Imalys* stores the results of all processes at the working directory. Images are stored in the ENVI format, vector data and tables using the WKT format. Both formats support fast and easy processing. During *export* the data can be translated to a large variety of other formats. The format is controlled by the given extension.

```
IMALYS [example 6]
home
	directory = ~/.imalys
	log = ~/ESIS/results
	
...

export
	select = compile
	target = ~/ESIS/bestof_2022.tif
export
	select = zones
	target = ~/ESIS/zones_2022.shp
export
	select = index
	target = ~/ESIS/zones_2022
export
	select = mapping
	target = ~/ESIS/mapping.tif
```

Tutorial 6 shows how to [export](../manual/11_Export.md) data from the working directory. All examples are part of previous tutorials. The first example stores the results of the *bestof* process, the second stores [zones](../manual/7_Zones.md) as a vector file, the third stores the definition files of the *zones* to the passed directory and the forth stores the most recent class definition and the resulting image. The export format is controlled by the extension in the result name. *Export* can be repeated after each command to save new results.

[Top](6_Export.md)

