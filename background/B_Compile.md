**[Home](../README.md) «» [Manual](../manual/README.md) «» [Tutorial](../tutorial/README.md) «» [Background](../background/README.md) «» [Source](../source)**

[TOC]

------

## Compile: Combine images for further processing

In *Imalys*, [compile](../manual/4_Compile.md) is the central hub for all image data. *Compile* combines all transferred bands or images into one stack. If necessary, format, projection, pixel size and region are adjusted and empty areas are filled with NoData. After *compile*, all images are saved as 32-bit float RAW binary with ENVI header (working format). All *Imalys* commands expect image data in this format. Only classes and zones use an integer format (below).

------

### Common image format

*Imalys* uses the same data format for all processes with image data: 32 bit float raw binary with IDL/ENVI header. Classes are formatted with 8 bit (byte) and zones with 32 bit integer. The headers correspond to the IDL/ENVI conventions, but are extended for *Imalys* by individual entries (e.g. Quality, Period). *Imalys* uses the value 1/0 (+ infinity) for pixels and other values without content (NoData). Deviating NoData values, e.g. for height models, must be adjusted during [import](../manual/3_Import.md) or [compile](../manual/4_Compile.md).

IDL/ENVI as the raster format and WKT (.csv) as the vector format were chosen to make the various processes as simple as possible. The choice is compatible with ESA conventions. *Imalys* processes do NOT check whether the input data uses the common format. Only [import]((../manual/3_Import.md)) and [compile](../manual/4_Compile.md) can read image and vector data in numerous formats. *Imalys* recognizes the formats by the extension in the file name. *Imalys* uses the [GDAL](https://github.com/OSGeo/GDAL) library for transformation.

------

### Prepare images to be stacked

[Compile](../manual/4_Compile.md) is a three step process. In the first processing step *compile* uses filters for band- and file-names, the acquisition date and the image frame, which can be set with the corresponding parameters. *Compile* then only accepts images that meet all criteria. The result can be empty. Then *compile* transforms all image data into the selected coordinate system, adjusts the pixel size and saves the intermediate results in the working memory. *Compile* calls the [GDAL](https://github.com/OSGeo/GDAL) library for this purpose.

------

### Stack, arrange and crop images

In the second step, [compile](../manual/4_Compile.md) copies the results from the first step into a common *frame*. In doing so, *Compile* creates a new layer for each band and crops all images to the common *frame*. *Compile* overwrites images with identical dates. *Compile* interprets such images as parts (tiles) of the same image.

In preparation for the combination, [compile](../manual/4_Compile.md) sorts the image data by name and date, transfers the file names to the band names and registers the image quality and bands per image in the metadata. If no *frame* is specified for the result, *Compile* uses the union of all images. If the *frame* is specified in a different coordinate system than the images, *Compile* uses a bounding box around the new coordinates so that all parts of the images are retained despite rotation.

If all transferred images are of the same size and projected to the same coordinate system, [compile](../manual/4_Compile.md) uses a simple version of the stack algorithm instead of the described process. 

------

### Set NoData and mask images

Some image data uses their own NoData values. With the parameter *nodata* under [import](../manual/3_Import.md), these values can be set to the [imalys NoData value](B_Compile.md). With the *clip* parameter, all pixels that lie outside the transferred *frame* can be set to NoData.

The [reduce](../manual/5_Reduce.md) function offers various options for combining or comparing images.

---

[Top](B_Compile.md)