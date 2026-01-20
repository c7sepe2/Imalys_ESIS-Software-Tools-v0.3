**[Home](../README.md) «» [Manual](../manual/README.md) «» [Tutorial](../tutorial/README.md) «» [Background](../background/README.md) «» [Source](../source)**

[TOC]

------

## *Compile:* Select, translate and stack bands or images

*Compile* is the primary command to organize images and prepare them for further processing.

Most of all *Imalys* commands accept only one image stored at the working directory as an input. The *compile* command translates all selected images to a common format, warps them to a common coordinate system and pixel size, stacks them to a multi layer image, clips them to the given *frame* and stores the result as *compile* at the working directory. The compilation ensures that all selected bands share the same format, projection, pixel size and frame. 


```
IMALYS [compile]
...
compile
	search = ~/ESIS/results/*2022*.tif
	period = 20220501 – 20220731
	crsystem = 32632
	merge = true
	pixel = 30
	bands = B3:B4
	names = red, infrared
	frame = ~/ESIS/frames/c4738.gpkg
	clip = true
	target = Leipzig_B34_2022
	...
```

The images can be selected directly by a filename (*select*) or a *search* string with wild chars ('?','*'). Both can be repeated as often as necessary. Each image format supported by the [gdal](http://www.osgeo.org/) library can be used as an input. If a projection *crsystem* and a *pixel* size is passed, the result is converted to the given system. The selection can be reduced further to a specified time *period* and clipped to a common *frame*. 

For different images, *frame*, *crsystem*, and *pixel* should always be specified. If this information is missing, *compile* will take the parameters from the first image and change all others according to this model. 

If two images share exactly the same acquisition date, they are interpreted as different tiles of the same flight path and can be combined using the *merge* parameter. To do this, the date must be at the end of the file name. The [import](3_Import.md) command saves images with a suitable name.

------

### *Bands*: Select specified band

```
IMALYS [compile]
...
compile
	select | search = ...
	bands = B3:B4
```

Instead of complete images, individual bands from an image can be transferred using the *bands* parameter. Individual bands are selected with “BX,” where “X” stands for the band number to be transferred. The output “BX:BY” selects all bands between number ‘X’ and number “Y”. In the [Import](3_Import.md) command, *bands* are selected by their names. The *compile* command uses the order of the bands in the image for this purpose.
 
------

### *Clip:* Mask the passed *frame*

**only together with *frame***

```
IMALYS [compile]
...
compile
	select | search = ...
	clip = false | true
```

*"clip = false" is default and can be skipped.*

*Clip* restricts the visible image to the transferred *frame*. The option *clip = true* overwrites all pixels outside the *frame* with NoData. The default value is “clip = false” and does not need to be specified.

------

### *Crsystem [EPSG], Pixel [meters]:* Reproject images and define a pixel size

```
IMALYS [compile]
...
compile
	select | search = ...
	crsystem = 32632
	pixel = 30
```

To use different images of the same region or to adopt images to given maps the projection and the pixel size can be changed. *crsystem* expects an EPSG number as new projection. *Pixel* expects a number in meters for the new pixel size. The process depends on the image type. Maps and classified images are reprojected using the nearest neighbor principle. All other images are interpolated by a bicubic convolution process. *Crsystem* and *pixel* can be used also if only the pixel size is to be changed. 

------

### *Frame:* Cut the result to a polygon

```
IMALYS [compile]
...
compile
	select | search = ...
	frame = ~/ESIS/frames/c4738.gpkg
```

If the selected images have different coverages or the result should be restricted to a specified area the parameter *frame* can restrict the result to a specific area. Frame must be passed as a polygon (geometry).

*Frame* spans a rectangle that contains all points of the *frame* and only returns the areas that lie within this rectangle. The *frame* can take any shape. With *clip = true* option, all areas outside the *frame* can be set to NoData. The geometry of the *frame* is automatically reprojected to the coordinate system (*crsystem*) of the images. If no *crsystem* is given all images are adjusted to the first image in the list.

------

### *Merge:* Overwrite images with the same acquisition date

```
IMALYS [compile]
...
compile
	select | search = ...
	merge = false | true
```

*"merge = false" is default and can be skipped*

Providers such as ESA and NASA deliver their raw data in tiles that overlap at the edges. In the direction of flight of the satellite, these tiles contain identical image data. *Compile* can combine such tiles into a homogeneous image during import. The default value is “merge = false” and does not need to be specified.

If the *merge* parameter is set, the *compile* command saves tiles with identical dates as a continuous image. All other tiles are saved as separate layers in a common stack.

------

### *Names:* Assign individual layer names
```
IMALYS [compile]
...
compile
	select | search = ...
	names = blue, green, red, infrared, temperature
```

*In this example 5 names for 5 layers are passed*

Layer names can contain important information, especially when raster and vector layers are mixed. With *names*, a comma-separated list of names can be passed, which is written to the image data in exactly this order. To rename the layers properly, the number of names must match the number of bands. Without manual input, *compile* takes the names of the processes that created the individual channels as far as possible.

------

### *Nodata:* Set an additional NoData value
```
IMALYS [compile]
...
compile
	select | search = ...
	nodata = -32768
```

*In this example a low integer figure is passed. Sometimes it serves as NoData value in elevation models.*

Imalys always uses 1/0 as the value for undefined pixels (NoData). When image data from different sources is mixed, other image values may be defined as Nodata. The *nodata* parameter can be used to define any value in the image data as NoData. Existing NoData values are retained, and the transferred value is converted to NoData. 

------

### *Period:* Select images by a time period

**only together with *search***

```
IMALYS [compile]
...
compile
	select | search = ...
	period = 20220501 – 20220731
```

*Period* reduces the file list of a *search* to images that match the specified period. The acquisition date must be passed as YYYYMMDD - YYYYMMDD (Y=Year, M=Month, D=Day). As not all image formats save an acquisition date, the date must be provided at the end of the file name. Use [import](3_Import.md) to get appropriate names.

-----

### *Select* single images by a filename

```
IMALYS [compile]
...
compile
	select = ~/ESIS/results/BRD_NirV_20230801-20231031.tif
```

*Select* will assign a single image. The *select* parameter can be repeated as often as necessary. Images at the working directory can be called without the directory part. *Select* can be combined with *search* and *period*. 

------

### *Search:* Select images using a search string

```
IMALYS [compile]
...
compile
	search = ~/ESIS/results/*2022*.tif
```

*In this example, all TIFF-formatted images whose file names contain the string “2022” will be selected*

A larger amount of images can be selected with a *search* string using system placeholders ('*','?', …). Variable and fixed parts of the filenames can be combined as needed. 
The *search* parameter can be repeated as often as necessary. *Search* can be combined with *select* and *period*. 

------

### *Target:* Assign a filename for the compiled image stack

```
IMALYS [compile]
...
compile
	select | search = ...
	target = ps_compile
```

*In this example "ps_compile" is chosen as a valid filename at the working directory. The directory part is added by Imalys.*

If the result of the *compile* command should be preserved for further processing the default name "compile" can be changed using the the *target* parameter. *Target* saves the result of the *compile* command at the working directory with the passed name. No path name needs to be specified for the working directory. 

Final results can be stored with the [export](11_Export.md) command at a different place. 

------

[Top](4_Compile.md#Frame:)