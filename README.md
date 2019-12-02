# examples
Various analysis examples

Directory | Description | Language
------------ | ------------- | ---
annotate_area | Add column with area codes based on columns specifying positions, and shapefiles | R
cruise_series_extraction | Extract cruise series via IMR APIs using RstoX | R
fetch_biotic | Fetching data through NMD APIs | R
gerritsen_plot | Generic functions for making plots with common axes in a tabular layout | R ggplot2
landings | Example of parsing and filtering sales notes data from LSS format | R
landings_api | Example commands for fetching sales notes data throught the landings - API at IMR | wget curl
nssk_2019 | Example scripts for preparing data for NSSK 2019 | R
possion_sampling | Simple illustration of Poisson sampling with unequal probability | R
scanmar_analysis | extract information from NMEA telegrams from SCANMAR sensors and analyse them in R. Example of analysis for depth sensor on trawl doors | R python
station_densities | map plotting examples, including plotting density measures as bubbles on given locations. Examples provied for catch rates at trawl-stations | R ggplot2

## Data analysis at IMR
where to find important data access-points and documentation

### Resources at IMR:

What | Where | Access
---- | ----- | ------
Data format documentations | http://www.imr.no/formats/ | open
Reference table documentation (Norwegian) | https://referenceeditor.hi.no | IMR network
Data set explorer | https://datasetexplorer.hi.no | IMR network
API for survey fish-data (fisheries independent & commercial catches) | https://confluence.imr.no/display/API/Biotic+V3+API+documentation | IMR network
API for sales-notes data | https://confluence.imr.no/display/API/Landing+V2+API+documentation | IMR network
Data portal for plankton data | http://coeus.imr.no/dataportal/ | IMR network

### External resources

#### Spatial data
What | Where | Access
---- | ----- | ------
Shapefiles ICES | http://gis.ices.dk/sf/ | open
Shapefiles Directorate of Fisheries | https://kart.fiskeridir.no/nedlasting | open
Bathymetry data | http://portal.emodnet-bathymetry.eu | open

#### Taxonomy
What | Where | Access
---- | ----- | ------
WORMS API | http://www.marinespecies.org/aphia.php?p=webservice | open
ITIS API | https://www.itis.gov/ws_description.html | open
FAO list | http://www.fao.org/fishery/collection/asfis/en | open
Fishbase | https://www.fishbase.se/search.php | open
Artsdatabanken (Norwegian) | http://www2.artsdatabanken.no/artsnavn/Contentpages/Sok.aspx | open
Maritim ordbok (for norwegian names) | https://www.nhh.no/research-centres/maritim-ordbok/ | open

#### Fisheries-dependent data
What | Where | Access
---- | ----- | ------
ICES vocabulary and codes | https://ices.dk/marine-data/vocabularies/pages/default.aspx | open
Regional database (EU) codes | https://www.ices.dk/marine-data/data-portals/Pages/RDB-FishFrame.aspx | open
Norwegian standard conversion factors | https://www.fiskeridir.no/Yrkesfiske/Tall-og-analyse/Omregningsfaktorer | open
FAO standard gear codes (ISSCFG,1980) | http://www.fao.org/3/a-bt986e.pdf | open
FAO revised gear codes (less common) (ISSCFG, 2013) | http://www.fao.org/3/a-bt988e.pdf | open
Mesh-size definitions (Norwegian) | https://www.fiskeridir.no/Fritidsfiske/Reiskap/Garn/Korleis-maaler-vi-masker-i-garn | open
Metier definitions | https://datacollection.jrc.ec.europa.eu/wordef/fishing-activity-metier | open
Offical Norwegian catch statistics, by species (Norwegian) | https://www.fiskeridir.no/Yrkesfiske/Tall-og-analyse/Fangst-og-kvoter/Fangst/Fangst-fordelt-paa-art | open
Norwegian Fisheries Vessel Registry | https://register.fiskeridir.no/fartoyreg/ | open

#### Fisheries-independent data
What | Where | Access
---- | ----- | ------
DATRAS (ICES swept-area surveys) | http://www.ices.dk/marine-data/data-portals/Pages/DATRAS.aspx | open
Acoustic (ICES acoustic trawl surveys) | http://www.ices.dk/marine-data/data-portals/Pages/acoustic.aspx | open
ICES vocabulary and codes | https://ices.dk/marine-data/vocabularies/pages/default.aspx | open
