# SegButler

### Running SegButler on 3D image stacks in subfolders

```wolframscript -script runSegButler.wls -imgdir ../testdata/test3D/ -objSz 120 -downscale 0.5 -imgstacks```

flags:
```-objSz [Size of objects in image in pixels]```
```-downscale [Downscale factor of original image, useful to speed up analysis]```

Image files need to have the following format:

```*_xy_0-463_z_1-44.*```

with 0.463 being the xy pixelsize and 1.44 being the z pixelsize