## Changelog

All Imalys functions are combined in one executable program  [xImalys](./binary). Changes of version 3.1 relevant for users are described below.

### Protocols and log files:

Logs, error messages and process logs are linked via a common date (see [Execute:](manual/0_Execute.md)).

### Variables:

The process chain can contain variables that are defined at the beginning of the process script (see [Replace](manual/12_Replace.md)). The variables can be passed as a comma-separated list in curly brackets. *Imalys* then repeats the process chain for each entry in the list. 

### Image archives:

The collection of locally stored image data [Catalog](manual/2_Catalog.md) is temporarily deactivated.

The **import** and **compile** commands have been reworked. [Import](manual/3_Import.md) extracts compressed data from the provider archives, calibrates it, and supports the search for matching archives with filters for the recording time and the image tile ID. [Compile](manual/4_Compile.md) selects the time and the frame (section) of the image data and transforms formats, projection, pixel size, and image bands. 

### Calibration:

Under [import](manual/3_Import.md), different calibration factors for different bands ´can be passed as a list using **factor** and **offset**.

### Image Quality:

Under [import](manual/3_Import.md), the **quality** parameter generates an image with the number of clear pixels for each image point from all input images. For self-combined images, the *quality* parameter can be called under [reduce](manual/5_Reduce.md).

### Nodata Value:

Under [import](manual/3_Import.md), the NoData value of the raw data can be explicitly set with the **nodata** parameter.

### Image Combination:

If a **frame** has been set under [compile](manual/4_Compile.md), *compile* combines image parts and recordings from different time and places into an intermediate product, which can be merged with [reduce](manual/5_Reduce.md) to create an optimized image of all image parts covering the *frame*. **Merge** under *compile* combines different tiles from the same satellite track into one image.

### Band Names:

Under [compile](manual/4_Compile.md), the **names** parameter can be used to replace the automatically assigned band names with names of your choice.

### Command "intensity":

Under [reduce](manual/5_Reduce.md), the new **intensity** command combines a time series of vegetation indices (NIRV) into an index for reduced vegetation periods, which allows conclusions to be drawn about the intensity of agricultural use.

### Object Classes:

Object classes under [mapping](manual/9_Mapping.md) are created using the parameter **fabric** = true. All other settings are identical to the classification of [zones](manual/7_Zones.md).