**[Home](../README.md) «» [Manual](../manual/README.md) «» [Tutorial](../tutorial/README.md) «» [Background](../background/README.md) «» [Source](../source)**

[TOC]

------

## *Import:* Select, extract, calibrate and clip images

*Import* generates calibrated images with freely selectable bands and a freely selectable image section and saves them in the working directory. 

*Import* was designed to select, extract, calibrate, and format image data from compressed archives provided by providers (NASA/USGS, ESA, etc.) for *Imalys*. The [compile](4_Compile.md) command then combines the imported image tiles into a homogeneous image product. 

Import can quickly search very large collections of archives by location and time to find suitable image data. When selecting image data, *import* uses the file names provided by the providers. These must not be changed. 

```
IMALYS [import]
...
import
	search = ~/ESIS/archives/LC*.tar
	tiles = 193024, 194024
	period = 20220501 - 20220731
	quality = 0.7
	bands = B2, B3, B4, B5, B6, B7
	factor = 2.75e-5
	offset = -0.2
	nodata = 0
	frame = ~/ESIS/frames/Leipzig_b5km_4326.shp
```

The *search*, *tiles*, *frame*, *quality*, *period* and *bands* parameters operate as filters. *Search* selects archives according to a system search, *tiles* accept only the passed tile-IDs, *frame* cuts the result to the passed boundaries. *Quality* rejects any image quality below the passed partition of clear pixels. *period* rejects any image outside of the passed time period and *bands* extracts only the passed bands. *factor* and *offset* transform the raw values to a calibrated value like reflectance or radiation. *Nodata* translates the passed value to the internal NoData definition.

------

### *Bands:* Select bands to be extracted 

```
IMALYS [import]
...
import
	search = ...
	bands = B2, B3, B4, B5, B6, B7
```

*Bands* allows to restrict the extraction to specified bands. For compressed Archives the selection must be given as a comma separated list of band name masks. The band names of the providers are long. The band ID is a short abbreviation around the end of the name. Only these abbreviations have to be specified (see above for all optical bands of the Landsat OLI sensor). 

------

### *Factor, Offset, NoData:* Calibrate raw image data

```
IMALYS [import]
...
import
	search = ...
	factor = 2.75e-5
	offset = -0.2
	nodata = 0
```

Satellite images are provided with values that support easy storage and transportation. For image analysis it is strongly recommended to use reflectance (reflection) or radiation [W/m²] instead of raw values. 

*Import* assumes that zero represents undefined pixels in raw image data. If zero can be a valid value, such as in elevation data or a vegetation index, the value for undefined pixels in the image must be specified. Without a *nodata* value, [compile](4_Compile.md) cannot assemble parts of a larger image properly.

The calibration parameters are part of the metadata of the image archives but sometimes they are difficult to find. Examples for most common sensors are given in the [tutorial](../tutorials). The calibration parameters are sensor dependent and may change between the different product levels of the provider. 

![](../images/M3_factor-offset.png) 

R: Result value; v: Provided value; f: Factor; o: Offset

------

### *Frame:* Cut out parts of the images

```
IMALYS [import]
...
import
	search = ...
	frame = ~/ESIS/frames/Leipzig_b5km_4326.shp
```

*Frame* extracts a defined section from the existing image data. The section does not have to be limited to a tile. The [compile](4_Compile.md) command can combine different image parts into a homogeneous and seamless result. 

The *frame* must be passed as a GIS layer (polygon) and works as a Region Of Interest (ROI). About 20 different GIS formats will be accepted. The coordinate system is automatically adjusted. If the *frame* and the images do not overlap at all the result is empty. 

------

### *Period:* Select a time interval for image acquisition

```
IMALYS [import]
...
import
	search = ...
	period = 20220501 - 20220731
```

*Period* selects year, month and day of the image acquisition. The *period* must be given as "YYYYMMDD - YYYYMMDD" (Y=year, M=month and D=day) as shown above. *Period* relays on the acquisition date in the filenames of the providers.

------

### *Quality:* Apply a quality mask to reject images

```
IMALYS [import]
...
import
	search = ...
	quality = 0.7
```

The *quality* parameter defines the lowest partition of undisturbed pixels in the passed *frame*. Quality must be passed as a figure between 0 and 1. Most of the public remote sensing images are shipped with a quality layer.  *Quality* recognizes it by its name. In Landsat archives, it is of high quality. *Import* uses the quality layer to reject images with more dropouts than the given limit.

If a quality layer and enough images from the same season are available, images with up to 50% errors (*quality = 0.5*) can be combined into a single image (see [reduce](5_Reduce.md)). The result is the most frequent value of each pixel. In many cases the combination is preferable to a single image.

If individual images have to be used, the quality should be close to the maximum (1.0). 

------

### *Search:* Provide a search string

```
IMALYS [import]
...
import
	search = ~/ESIS/archives/LC*.tar
```

The search mask *search* selects a directory in which all image archives can be found. The search can be further restricted using placeholders and parts of the archive name. In this case, only Landsat TAR archives are included. The search mask can use all placeholders like “*” or “?” that are common in the operating system. If the archives are distributed across different directories, *search* must be repeated.

------

### *Tiles:* Select appropriate tiles

```
IMALYS [import]
...
import
	search = ...
	tiles = 193024, 194024
```

Satellite images are provided in partly overlapping tiles. The *tiles* parameter only accepts archives or images with the selected tile IDs in the file name. Different IDs can be passed as a comma-separated list. The *tiles* parameter filters the existing list according to the passed tile IDs.

The names of the tiles vary depending on the provider. In this example, the Landsat version was used. The best way to see which tile ID belongs to which location is to look at a map showing the tile boundaries [Landsat tiles](WRS2_descending) or [Sentinel-2 tiles](sentinel_2_index). A tool that uses the transferred *frame* to find the correct tile is in preparation. 

------

[Top](3_Import.md)
