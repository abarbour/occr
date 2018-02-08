https://earthquake.usgs.gov/earthquakes/search/

extent(OKcounties)
class       : Extent
xmin        : -103.0025
xmax        : -94.43066
ymin        : 33.61579
ymax        : 37.00231

M>=0
1898-01-01 00:00:00

[33.5, 37.1] Latitude
[-103.1, -94.3] Longitude

NOTE:
        be sure to 
	  * select csv as format of output
	  * check order (oldest first)
	then, after downloading:
	  * strip out non-data/header lines 
	    if they exist (csv output shouldn't need touchups)
	  * relink to comcat_catalog.csv
	  * go back and re-run Dly*.R

[ ] Note that now there is a limit, so will
need to stitch multiple catalogs together (arg...)
