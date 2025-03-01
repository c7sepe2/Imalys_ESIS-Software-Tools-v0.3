### Imalys Source Code

The *Imalys* sources are completely provided in this directory. The project *xImalys* includes all *Imalys* [commands](../manual/README.md) and a [parser](../manual/0_Execute.md) to interpret a process chain text file. The code is written in [Free Pascal](https://www.freepascal.org/) and compiled under Linux/Ubuntu.

The *custom* unit reads commands and parameters from the process chain text file, executes the commands and returns a process log and an error log. All other units (*raster, format, mutual, thema, index, vector*) provide the different commands. 

The project file *xImalys.lpr* is designed using [Lazarus](https://www.lazarus-ide.org/). To run under a server environment *Imalys* does not call any graphical objects. As the code is written for Linux, some system calls will not work using Windows. Please be aware that the project files includes my personal paths and must be modified. 
