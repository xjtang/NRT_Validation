# Validation Protocol  

**This is the validation protocol for evaluating samples for the accuracy assessment of near-real time deforestation detection products.**  

## Classes and Codes

| Class     | Ref_Code | Ref_Class |
|:----------|:--------:|:---------:|
| Forest    | 0        | F         |
| Change    | 1        | C         |
| Non-forest| 4        | NF        |

## Validation Steps

1. Use individual sample shapefile, not master.  
2. Load up 2 QGIS applications, 1 for Landsat and 1 for MODIS. Pick 1 Landsat scene to start with.  
3. Use preview images to see what class it begins as (2012).  
4. Use preview image to see what class it ends as.  
5. Define the class of this sample pixel.  
  i. If stable class, write down Ref_Code, Ref_Class, Note, and Confidence.  
  ii. If change class, find last forest, first non-forest, and first clear in Landsat previews.  
  iii. Use Google Earth with KML to find detail about change if needed.  
6. Look to see if the change expanded, record date if it does.  
7. Use MODIS previews or images to narrow down the change date (if possible).  
8. Write down inclusion based on last image.  
  Full: At the end, all of the pixel is within the change.  
  Edge: Part of the pixel is within the change.  
  Near: if change is within 1 MODIS pixel it is considered near. More than 1 MODIS pixel and it is not considered change.  
10. If change, create folder for change polygon. Create a new shapefile to draw the change event. The shapefile should have the same UTM projection as the Landsat scene that you are working on. In addition to the default "id" column, two additional whole number columns should be added: "pid" which indicate the ID of the sample pixel; "date" which indicate the date of the image that the drawing is based on.
11. Use QGIS to draw polygon of the change. Create different polygons in the same file if the change expands (if it starts with the pixel on edge but ends with full). The "pid" attribute indicates pixel ID, while the "id" attribute indicates the ID of the change polygon (1 = first change, 2 = expandsion, etc). 
12. Write down confidence.  
  1: Not very confident, want to review with full group.  
  2: Not entirely sure, want to review with 1 or 2 others (should be mostly changes.)  
  3: Very confident. No need to review, will be mostly stable classes.  
13. Write down any notes. (e.g. is it agriculture, river, urban? etc.)  

## Special Rules  

1. Any change that is visible in the Landsat images should be considered a change event.  
2. In the case of part forest part non-forest, the assessor will decide which ever class has the majority of the pixel and therefore call it that class. The Includsion for this pixel will be Edge.  
3. In the case of part of the event started changing earlier than the sample pixel, the recording of the dates should be based on the change date of the sample pixel. A new column called D_EVENT is added in order to record the initial change date of the event. And it should be noted in the notes.  
4. In the case of a sample pixel that is part forest and part non-forest, and then later the forest part of the pixel changed during out study time period, this pixel shall be called a change pixel with inclusion of Edge. And the dates shall be recorded based on the new change event. Only the new change event shall appear in the shapefile. The note should indicate that this pixel is already partially changed in the past.  
5. A complete forest pixel near a old change shall be called forest with the inclusion of Near.  
6. Expansion is recorded only if the expansion event is part of the sample pixel.  
7. In the case of a complete clearing of the change event, but part of or the entire sample pixel was not cleared, we should record the clearing date as D_CLEAR and make a note in the notes.  
8. In most cases, the MODIS previews are not helpful. However, if the MODIS previews can suggest any date of change that is more accurate than the date provided from Landsat, it should be recorded as D_CHANGE.  
9. If a sample pixel starts with Edge and ends with Full, it is still considered Full, but make note and draw seperate polygons.  
10. In the case of a change within one pixel distance to the sample pixel, only date of the start of the event and date of the clearing needs to be recorded in D_EVENT and D_CLEAR respectively.  

## Data Details

Landsat Scene: P007R059  
Image Files: /projectnb/landsat/projects/fusion/amz_site/data/landsat/P007R059/images/  
Filename Pattern: L\*stack  
Mask Band: 8  
Cache Folder: /projectnb/landsat/projects/fusion/amz_site/data/landsat/P007R059/cache/.cache  
MODIS Tile: h10v08  
MODIS Data: /projectnb/landsat/datasets/MODIS/h10v08/stacks/  
Filename pattern: C\*tif  
Date Indices: 10, 17  
Mask Band: 6  
Cache Folder: .cache  
Mask Values: 0 (1 = clear)  

Landsat Scene: P232R066  
Image Files: /projectnb/landsat/projects/fusion/amz_site/data/landsat/P232R066/images/  
Filename Pattern: L\*stack  
Mask Band: 8  
Cache Folder: /projectnb/landsat/projects/fusion/amz_site/data/landsat/P232R066/cache/.cache  
MODIS Tile: h11v09  
MODIS Data: /projectnb/landsat/datasets/MODIS/h11v09/stacks/  
Filename Pattern: C\*tif  
Date Indices: 10, 17  
Mask Band: 6  
Cache Folder: .cache  
Mask Values: 0 (1 = clear)  

Landsat Scene: P227R065  
Image Files: /projectnb/landsat/projects/fusion/amz_site/data/landsat/P227R065/images/  
Filename Pattern: L\*stack  
Mask Band: 8  
Cache Folder: /projectnb/landsat/projects/fusion/amz_site/data/landsat/P227R065/cache/.cache  
MODIS Tile: h12v08  
MODIS Data: /projectnb/landsat/datasets/MODIS/h12v08/stacks/  
Filename Pattern: C\*tif  
Date Indices: 10, 17  
Mask Band: 10  
Cache Folder: .cache  
Mask Values: 0 (1 = clear)  
