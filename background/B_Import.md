**[Home](../README.md) «» [Manual](../manual/README.md) «» [Tutorial](../tutorial/README.md) «» [Background](../background/README.md) «» [Source](../source)**

[TOC]

------

## Import

### Archives: Select, extract, crop and calibrate images

In order to store large amounts of data in a clear and space-saving manner, *Imalys* provides routines to read raw data directly from compressed archives as offered by the providers (NASA, ESA etc.). The routines have so far only been tested in detail for Landsat. MODIS, Sentinel-2, Aster and RaidEye are coded but still in the test phase.

The [import](../manual/3_Import.md) command combines the search for suitable locations, acquisition dates and spectral bands in compressed archives, checks the image quality and the proportion of clouds and image deviations, calibrates the raw data, converts to the [Working format](B_Compile.md) and expands the metadata. Additionally *import* creates and saves a QA (Quality Assessment) image, which contains the number of clear bands per pixel [Quality](../manual/4_Compile.md).

[Import](../manual/3_Import.md) übernimmt die Zeit aus dem Dateinamen und den Ort aus den Metadaten der Provider. Als Maß für die Bildqualität dient er Anteil klarer Pixel im gewählten [frame](). *Import* verwendet dazu die Metadaten der Provider.

[Import](../manual/3_Import.md) takes the acquisition date from the file name and the location from the provider metadata. The proportion of clear pixels in the selected [frame](../manual/3_Import.md) serves as a measure of the image quality. *Import* uses the provider metadata to qualify the pixels.

Raw data can also be imported with [compile](../manual/4_Compile.md), but the coding is more complicated.

------

### Create an image archives catalog

The [catalog](../manual/2_Catalog.md) command takes the position (corner points) and acquisition date of all image archives in the selected directory and saves the collection as a geometry (*bonds.csv*) using then WKT format. The WKT format can be opened as a GIS layer to gain an overview of the archived images.

------

[Top](B_Import.md)
