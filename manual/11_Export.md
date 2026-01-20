**[Home](../README.md) «» [Manual](../manual/README.md) «» [Tutorial](../tutorial/README.md) «» [Background](../background/README.md) «» [Source](../source)**

[TOC]

------

## *Export* layers from the working directory

Imalys stores each primary result at the working directory. The results are named as the commands. The results can be exported to any other place. During *export* the image or vector format can be selected. The extension of the target filename controls the format of the export.

------

### *Select* a raster or vector layer to be exported

```
export
	select = reduce
	target = ~/ESIS/results/NirV_Regression_2000-2023
```

Each raster or vector layer at the working directory can be selected. The export will transform the selected file according to the file name extension of the *target*. A classified raster layer can be exported to a raster or a vector layer. 

[Zones](7_Zones.md) and the results of the [mapping](9_Mapping.md) command are described by more than one file. Special rules apply to them. The “index” file in the working directory is the raster version of the *zones* (see [zones](../background/B_Zones.md)). If “**index**” is selected under *export*, the command exports a complete set of all definition files to a separate directory. The name for the directory is selected with *target*. To export *zones* in vector format with all [features](8_Features.md) as attributes, “**zones**” must be selected. The name of the new vector layer is selected with *target*. The results of the ***mapping*** command consist of an image and the class definition “mapping.bit”. The class definition is copied to the same location as the image. Both take on the selected name. The image is saved in the format selected in *target* extension. **Tables** are exported as comma separated values (csv) under the new name. The format can be read by any spreadsheet program.

------

### *Target:* Select the filename and the format of the output file(s)

Raster and vector based results can be exported to all formats that are supported by the GDAL library. Raster export includes results from vector based processes. The extension controls the format of the result. Without extension the format is ENVI labelled. Vector export includes automated transformation for classified raster data.

```
export
	select = index
	target = ~/ESIS/results/München
```

In this example the [zone](7_Zones.md) definition files (boundaries, features, topology, naming) are selected by *index* and exported to the *results* folder.

[Top](11_Export.md)