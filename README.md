## Imalys – ESIS tool for Image Analysis

In the ESIS project we are trying to put environmental indicators on a well-defined and reproducible basis. The *Imalys* software tools are supposed to generate ESIS traits from remote sensing data. *Imalys* was designed to bundle all processes from data selection to the final product in one process. 

![Munich RGB-Normal](images/Munich_RGB-Norm.png)

*City of Munich (Germany), Landsat 8/9, May…July 2022 • Colorized vector images based on zones • Zones boundaries depicted as thin black lines • **Left:** Color composit with bands 7-6-5 • **Right:** Brightness normalized texture within zones, Values 0…0.2 as Blue…Red*

------

The ESIS tools *Imalys* are available as software library and as executable program. Library and program provide tools to select, extract, transform and combine raster and vector data. 

 - Seamless high quality images, spectral indicators and timelines can be created from patchy precursors. 
 - Landscape diversity, trends and landcover types can be analyzed in space and time. 
 - Natural landscape boundaries can be delineated
 - Typical landscape structures can be characterized by a self adjusting classification process.

Most of the methods and analyses implemented in *Imalys* are also available with commercial software and/or open source solutions. The decisive factor for the *Imalys* concept was to bundle all necessary commands and parameters into one command and one process chain that contains all sub-steps and (depending on the application) only requires location, time and result names as input. 

*Imalys* was designed as a collection of building blocks (commands) to extract defined landscape properties (traits) from public available data. The whole process chain is controlled by predefined hooks and runs without manual interaction. The commands are interchangeable and can be rearranged for new tasks. *Imalys* is available as [source code](source/README.md) and as [executable files](executables/README.md) files. *Imalys* is designed to run under a server environment but can also be used on any PC. For detailed information please refer to our [manual](manual/README.md), the [tutorials](tutorial/README.md) or the [background](background/README.md) documents.

### Get Started

The easiest way to learn about *Imalys* is to run one of the [tutorials](tutorial/README.md). Copy and extract the [tutorial data](https://zenodo.org/records/11097359) to a place where you have writing permissions and follow the description. The tutorial includes all *imalys* tools and a stepwise description how to use them. The only thing you have to add is the [GDAL library](https://github.com/OSGeo/GDAL).

### Installation

The *Imalys* [binary files](executables/README.md) need no installation. They can be simply copied to your */usr/local/bin* directory. To run *Imalys* the GDAL library must be available under your */usr/bin/* directory. The GDAL library can be obtained from [GitHub](https://github.com/OSGeo/GDAL). If you run Quantum-Gis the library is already installed. For details please refer to our [manual](manual/README.md).

### Usage

The executabel file *xImalys* must be called with a shell or terminal. All necessary commands and parameters are passed by a single text file. Installation, commands and parameters are described in depth in our [manual](manual/README.md). A [tutorial](tutorial/README.md) provides examples of all processing steps and the combination of the *Imalys* tools. For expert users important algorithms are discussed in a [background](background/README.md) document.

### Development

*Imalys* is under development. The version 0.2 was focused on methods to select and extract appropriate images from large data collections as shipped by the providers and use them for a seamless and high quality product of a freely selectable region. A time series over 40 years for the whole of Germany at [Pangaea](https://doi.pangaea.de/10.1594/PANGAEA.967266) with approx. 50,000 image tiles is an example of this intention. 

In the current version 0.3, the tasks of the *import* and *compile* tools have been changed. *Import* extracts data from archives. *Compile* has been expanded to become the central collection point for all processes. *Compile* transfers data to the internal processing line, harmonizes position, projection and pixel size and can limit the result to a selected ROI. The new version means that process chains from version 0.2 are not always compatible with version 0.3. All noticable changes are documented in the [changelog](CHANGELOG.md). 

### Get involved

The source code is freely available under GNU license. The executable program *xImalys* implements 11 independent [modules](manual/README.md) for the various steps in a data analysis. The entire process chain is controlled by a list of commands and parameters. This list is pure text and do not require any programming knowledge. Examples can be found in [Tutorial](tutorial/README.md)

The sequence of modules can be recombined and linked to form new analyses. The finished process chains are independent of the user, the sensor, the scaling and the location of the analysis, except for the names of the input and output data. They can be exchanged.

We would like to hear of both, new ideas and new demands and how they can be realized!

### Contributing

If you found a bug or want to suggest some interesting features, please refer to our [contributing guidelines](CONTRIBUTING.md) to see how you can contribute to *Imalys*.

### User support

If you need help or have a question, you can use the [Imalys user support](mailto:imalys-support@ufz.de).

### Copyright and License

Copyright(c) 2023, [Helmholtz-Zentrum für Umweltforschung GmbH -- UFZ](https://www.ufz.de). All rights reserved.

- Documentation: [Creative Commons Attribution 4.0 International](https://creativecommons.org/licenses/by/4.0/) <a rel="license" href="http://creativecommons.org/licenses/by/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by/4.0/80x15.png" /></a>

- Source code: [GNU General Public License 3](https://www.gnu.org/licenses/gpl-3.0.html)

For full details, see [LICENSE](https://codebase.helmholtz.cloud/esis/Imalys/-/blob/main/LICENSE.md).

### Acknowledgements

### Publications

Selsam, Peter; Lausch, Angela; Bumberger, Jan (2025): Germany Mosaic: A 40-Year High-Resolution Remote Sensing Time Series of Germany [dataset publication series]. Helmholtz Centre for Environmental Research - UFZ, PANGAEA. https://doi.org/10.1594/PANGAEA.967266

Lausch, A, Selsam, P, Heege, T, von Trentini, F, Almeroth, A, Borg, E, Klenke, R, Bumberger, J: Monitoring and modelling landscape structure, land use intensity and landscape change as drivers of water quality using remote sensing. Science of the Total Environment 960 (2025) 178347: https://doi.org/10.1016/j.scitotenv.2024.178347

Selsam, P.; Bumberger, J.;Wellmann, T.; Pause, M.; Gey, R.; Borg, E.; Lausch, A. Ecosystem Integrity Remote Sensing—Modelling and Service Tool—ESIS/Imalys. Remote Sens. 2024, 16, 1139. https://doi.org/10.3390/rs16071139

Lausch A, Selsam P, Pause M, Bumberger J. 2024 Monitoring vegetation-and geodiversity with remote sensing and traits.Phil.Trans.R.Soc.A382: 20230058.https://doi.org/10.1098/rsta.2023.0058

### How to cite Imalys

If Imalys is advancing your research, please cite as:

>Selsam, P.; Bumberger, J.; Wellmann, T.; Pause, M.; Gey, R.; Borg, E.; Lausch, A. Ecosystem Integrity Remote Sensing—Modelling and Service Tool—ESIS/Imalys. Remote Sens. 2024, 16, 1139. https://doi.org/10.3390/rs16071139

-----------------
<a href="https://www.ufz.de/index.php?en=33573">
    <img src="https://git.ufz.de/rdm-software/saqc/raw/develop/docs/resources/images/representative/UFZLogo.png" width="400"/>
</a>

<a href="https://www.ufz.de/index.php?en=45348">
    <img src="https://git.ufz.de/rdm-software/saqc/raw/develop/docs/resources/images/representative/RDMLogo.png" align="right" width="220"/>
</a>
