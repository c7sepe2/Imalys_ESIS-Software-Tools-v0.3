## Export	

**exports layers from the working directory**

[TOC]

Imalys stores each primary result at the working directory. The results can be exported to any other place. During *export* the image or vector format can be selected. Classified images can be exported as attributed polygons. In all cases the extension of the target filename controls the format of the export.

------

### Select

**Select a raster or vector layer to be exported**

```
export
	select = reduce
	target = ~/ESIS/results/NirV_Regression_2000-2023
```

Each raster or vector layer at the working directory can be selected. The export will transform the selected file according to the file name extension of the *target*. A classified raster layer can be exported to a vector layer. Mapping results will be exported together with the binary class definition (BIN format). [Zones](7_Zones.md) must be selected as *index* and will be transformed to attributed polygons according to the selected vector format.

------

### Target

**Selects the filename and the format of the output file(s)**

Raster based results can be exported to 48 different raster formats. Raster export includes results from vector based processes. Without extension the format is ENVI labelled. Vector based results can be exported to 23 different vector formats. Vector export includes automated transformation for classified raster data. The default format is ESRI Shape.

```
export
	select = index
	target = ~/ESIS/results/München.shp
```

In this example the [zone](7_Zones.md) files (boundaries and features) are selected by *index* and exported to the *results* folder using the ESRI shape format. 

-----

[Previous](10_Compare.md)	–	[Index](README.md)	–	[Next](12_Replace.md)