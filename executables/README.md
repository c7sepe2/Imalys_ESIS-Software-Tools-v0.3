### Get Imalys

The *Imalys* executable file *xImalys* needs no installation. The file can be simply copied to your */usr/local/bin* directory. To run *xImalys* the GDAL library (*gdaltransform* and others) must be available under your */usr/bin/* directory. The GDAL library can be obtained from [GitHub](https://github.com/OSGeo/GDAL). If you run Quantum-Gis the library is already installed. For details please refer to our [manual](../manual/README.md).

### Call Imalys

*xImalys* must be called with the filename of a process chain text file as parameter:

```
x_Imalys path_to_process_chain
```

The call will execute the passed process chain. The *process chain* is a text file (hook) with commands and parameters (see [tutorial](../tutorial/README.md)). For details please refer to the [manual](../manual/README.md).

