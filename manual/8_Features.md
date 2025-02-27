## Features

**Create zone attributes from size, shape and pixel statistics**

[TOC]

During [zones](7_Zones.md) creation only the boundaries of the individual *zones* are recorded. The *features* process can add an attribute table to characterize the different spectral and morphological features of the *zones*. Each call of the *features* process will reset the whole attribute table. Technically all features are stored in an internal format for rapid processing. Use the [export](11_Export.md) command to get attributed polygons.

*Select* will assign spectral features to the zones. Spectral features will always be the mean of all pixels within one zone. The process allows only one input image. Other images must be stacked beforehand with [compile](4_Compile.md) and passed as a stack. 

*Execute* is used to add a couple of morphological and texture features. *Execute* can be repeated as often as necessary. 

*Entropy* calculates Rao’s Entropy or ß-Diversity using [zones](7_Zones.md) instead of a kernel. Textural features like *texture* or *normal* compare only adjacent pixels and return the “roughness” of the image whereas *entropy* compares all pixels within a given region (3) (5). Regular pattern will show a high roughness but their diversity might be comparably low. Usually the region is given as a moving window or kernel. *Zones* provide an alternative that is defined by regional image properties. The *texture* and the *normal* process for *zones* are also restricted to the boundaries of the *zones*.

*Dendrites*, *proportion* and *relation* add morphological features of the *zones*. *Dendrites* looks at single *zones*, *proportion* compares the size of adjacent *zones* and *relation* returns an indicator for spatial diversity of the *zones*. All of them are designed to be independent of the absolute size of the *zones*, as the absolute size can be selected freely. As *dendrites* depends on perimeter and size of the *zones*, the results can not be compared between different images.

*Diversity* mimics the spectral diversity at pixel scale for *zones*. As *zones* differ in size, shape and shared edges the diversity is not a simple mean but is calculated from the number of shared edges. For multispectral images the deviation of the different bands is calculated independently and the principal component of all deviations is taken as the final value. 

*Diffusion* emphasizes local maxima and minima of all *features*. The algorithm mimics a diffusion through membranes. During the process, *features* “migrate” into the neighboring zone like soluble substances and combine with existing "concentrations". The intensity of the diffusion depends on the length of the common boundaries, the concentration difference and the selected number of iterations. The size of the [zones](7_Zones.md) provides the stock of "soluble substance".

------

### Append (Parameter)

**Add new featurs to an existing attribute table**

```
IMALYS [features]
…
features
	execute = cellsize
	append = true
```

*Append = true* adds the newly selected features to the existing attribute table, in this case the size of the *zones*. The *features* command normally creates a new table. With *append = true* the features can be added individually.

------

### Cellsize (Process)

**Size of the zones given as [ha]**

```
IMALYS [features]
…
features
	execute = cellsize
```

The size of the zones is calculated as the sum of all pixels covering the zone. The values are given as Hectares [ha] (100m × 100m). 

​	![image-20240320120223601](../images/8_sellsize.png)	Sp: pixel size [m]; Sz: pixel per zone; 

Range: [ 0 < cellsize ]

------

### Dendrites (Process)

**Quotient of zone perimeter and cellsize**

```
IMALYS [features]
…
features
	execute = dendrites
```

The *dendrites* process returns the quotient between perimeter and size of single zones. Both values grow with larger zones but the size grows faster. Large zones will show lower values than smaller ones with the same shape.

​	![image-20240320114514035](../images/8_dendrites)	vr: Result Value; pz: Perimeter (zone); sz: Size (zone)

Range: [ 0 < dendrites ≤ 4 ]

------

### Diffusion (Process)

**Emphasize local maxima and minima for all features**

```
IMALYS [features]
…
features
	select = compile
	execute = dendrites
	diffusion = 7
```

The *diffusion* process smoothes small-scale differences and emphasizes outstanding values. The input *diffusion = 7* controls the number of diffusion steps. The process affects the entire attribute table. In the example above the attributes are "diffused" 7 times.

The *diffusion* process is only controlled by the number of iterations (*diffusion = number*). Each iteration enlarges the region of contributing zones. The influence of distant zones on the central zone decreases with distance. Entries over 10 are allowed, but rarely have a visible effect. The process influences small *zones* much more than large ones, as *diffusion* only takes place at the boundaries between the *zones*.

​	![image-20240320120444688](../images/8_diffusion.png)	a: attribute value; s: zone size; c: pixel contacts; i,j: zone indices; t: iterations (time)

Range: [ as parameters above ]

------

### Diversity (Process)

**Diversity of the central zone and all neighbors**

```
IMALYS [features]
…
features
	execute = diversity
```

The diversity between zones is calculated as the statistical deviation of all attributes between the central zone and all its neighbors. As the zones might differ considerably in size and shape the length of the common border was selected as a measure for the contribution of the peripheral zones to the final value. 

​	![image-20240320115214090](../images/8_diversity.png)	vi: Pixel value; vn: Neighbor value; bp: Pixel boundaries

Range: [ 0 ≤ dendrites ]

------

### Entropy (Process)

**Pixel diversity following Rao’s proposal**`

```
IMALYS [features]
…
features
	execute = entropy
```

The *entropy* process returns the spectral diversity of all pixels within one zone. For multispectral images the diversity is calculated independently for each band and the first principal component of all diversities is taken as the final result. The entropy values are calculated using the Gauss's formula for deviation.

​	![image-20240320113611345](../images/8_entropy.png)	dij: Density difference; I,j: neighbor pixels; pi, pj: frequency of pixel values “i” and “j”

Range: [ 0 ≤ entropy ]

------

### Execute (Process)

**Add geometry features as attributes**

```
IMALYS [features]
…
features
	…
	execute = cellsize
	execute = dendrites
	execute = diversity
	execute = entropy
	execute = normal
	execute = proportion
	execute = relation
```

The *execute* process creates morphological and textural *features*. Purely spectral *features* can be created with the *select* process. 

Currently four different *features* are defined to describe the geometry of the *zones* and their surroundings (*cellsize*, *dendrites*, *proportion*, *relation*), two (*entropy*, *normal*) to determine texture *features* within individual *zones* and one (*diversity*) to determine the diversity of entire *zones*. Each process and parameter is explained in detail under its name. Different *features* can be called individually or together.

------

### Normal (Process)

**Normalized pixel texture for individual zones**

```
IMALYS [features]
…
features
	execute = normal
```

As the *texture* process does, *normal* returns the mean difference between all pixel pairs within an individual zone but in this case the difference is normalized by the mean brightness of the compared pixels. 

​	![image-20240320114238721](../images/8_normal.png)	vi: pixel value; vj: neighbor pixel value; b: bands

Range: [ 0 ≤ normal ≤ 1 ]

------

### Proportion (Process)

**Size diversity of the central zone and all neighbors**

```
IMALYS [features]
…
features
	execute = proportion
```

The *proportion* process returns the relation between the size of the central zone and all its neighbors. The result is calculated as relation between the size of the central zone and the mean size of its neighbors. As the size is given in a logarithmic scale, the “mean” is not an arithmetic but a geometric mean. Values around one indicate equally sized neighbor zones. 

​	![image-20240523115837170](../images/8_proportion.png) si: Size, central zone; sj: Size, neighbor zone; n: number of neighbors

Range: [ 0 < proportion ]

------

### Relation (Process)

**Quotient of neighbors and perimeter of one zone**

```
IMALYS [features]
…
features
	execute = relation
```

*Relation* is calculated as the relation between the number of neighbor zones and the perimeter of the central zone. Like *dendrites* also *relation* returns information about the shape and the connection of the zones. Zones with many connections may provide paths for animal travels and enhance diversity.

​	![image-20240320115912653](../images/8_relation.png)	r: relation; c: number of neighbors; p: perimeter

Range: [ relation < 1]

------

### Select (Process)

**Assign spectral features as zone attributes**

```
IMALYS [features]
…
features
	select = compile
```

The *select* process adds all spectral features of the selected image to the attribute table. The band names are preserved as field names if possible. Each image with the same geometry as the *zones* image can be selected. If spectral properties of additional images should be added, the images must be stacked beforehand using the [compile](4_Compile.md) command. 

------

### Texture (Process)

**Pixel texture for individual zones**

```
IMALYS [features]
…
features
	execute = texture
```

The *texture* process returns the mean difference between all pixel pairs within an individual zone. The process thus returns the “roughness” of the image. The *normal* process will return a brightness independent result.

​	![image-20240320114020548](../images/8_texture.png)	v: pixel value; i,j: adjacent pixels; 

Range: [ 0 ≤ texture ]

------

### Values (Process)

**Raster representation of zones with attributes**

```
IMALYS [features]
…
features
	select = compile
	values = true
```

The *values* process creates a multi band raster image from all attributes of the zones polygons. *Values* mainly serves as a control feature. 

-----

[Previous](7_Zones.md)	–	[Index](README.md)	–	[Next](9_Mapping.md)