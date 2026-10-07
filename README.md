# Putting the Flytrap on the Map: Habitat Characteristics of Venus Flytraps (Dionaea muscipula) in Fire-dominated Longleaf Pine (Pinus palustris) Ecosystems

## Description of the data and file structure

This "README.md" file was generated on 16 September 2026 by William J. Gibson

GENERAL INFORMATION

1\. Title of Dataset: Putting the Flytrap on the Map: Habitat Characteristics of Venus Flytraps (Dionaea muscipula) in Fire-dominated Longleaf Pine (Pinus palustris) Ecosystems

2\. Author Information

```
A. Corresponding Author Contact Information

	Name: William J. Gibson
	Institution: University of Kentucky
	Address: 218 T.P. Cooper Building Lexington, KY 40546-0073
	Email: wjgi222@uky.edu

```

3\. Date of data collection (single date, range, approximate date):

May 2024 through September 2026

4\. Geographic location of data collection:

North Carolina, United States of America

5\. Information about funding sources that supported the collection of the data:

This project was supported by McIntire-Stennis Capacity Grant #KY009043

SHARING/ACCESS INFORMATION

1\. Licenses/restrictions placed on the data:

None to report

2\. Links to publications that cite or use the data:

Authors: Gibson, W. J., T. M. Terhune II and D. J. McNeil. In Review. Putting the Flytrap on the Map: Habitat Characteristics of Venus Flytraps (Dionaea muscipula) in Fire-dominated Longleaf Pine (Pinus palustris) Ecosystems.

3\. Links to other publicly accessible locations of the data:

None to report

4\. Links/relationships to ancillary data sets:

None to report

5\. Was data derived from another source? yes/no

```
A. If yes, list source(s): 
```

No

6\. Recommended citation for this dataset:

William, Gibson et al. (In Review), Title: Putting the Flytrap on the Map: Habitat Characteristics of Venus Flytraps (Dionaea muscipula) in Fire-dominated Longleaf Pine (Pinus palustris) Ecosystems, Dryad, Dataset, [DOI]

DATA & FILE OVERVIEW

1\. File List:

File: LandscapeSUBMISSION.R
File: vftdataframe.csv

Description: This file contains all data needed to replicate analyses presented in Gibson et al., In Review

2\. Relationship between files, if important:

Not applicable

3\. Additional related data collected that was not included in the current data package:

None

4\. Are there multiple versions of the dataset? No

```
A. If yes, name of file(s) that was updated: 

	i. Why was the file updated? Not applicable
	ii. When was the file updated? Not applicable
```

METHODOLOGICAL INFORMATION

1\. Description of methods used for collection/generation of data:

See Gibson et al. (in review), Methods.

2\. Methods for processing the data:

See Gibson et al. (in review), Methods.

3\. Instrument- or software-specific information needed to interpret the data:

See Gibson et al. (in review), Methods.

4\. Standards and calibration information, if appropriate:

None to report

5\. Environmental/experimental conditions:

See Gibson et al. (in review), Methods.

6\. Describe any quality-assurance procedures performed on the data:

See Gibson et al. (in review), Methods.

7\. People involved with sample collection, processing, analysis and/or submission:

See author contributions within manuscript.

DATA-SPECIFIC INFORMATION FOR: vftdataframe.csv

### Files and variables

#### File: vftdataframe.csv

**Description: Unscaled extracted environmental data from venus flytrap locations and background locations used for analyses**

##### Variables

* name: point; description: ID of venus flytrap location or background location, used only for identification
* name: ptvalue; description: describes if location is a presence or background point, with "1" equalling presence and "0" equalling background  rey description represents the lowest taxonomic level each prey item. "No_prey" indicates no evidence of prey. Value of "?" indicates that prey was present but no level of identification could be made. "Avian", "Mammalian", or "Reptilian" refer to a class-level identification only. For birds, we use the American Ornithological Society [AOS] alpha codes [see: https://www.pwrc.usgs.gov/BBL/Bander_Portal/login/speclist.php]. For example, Killdeer would be abbreviated as "KILL", Carolina Chickadee would be "CACH", Orange-crowned Warbler would be "OCWA" and Great Black-backed Gull would be "GBBG". For records where 2 or 3 options are all possible, they are all listed and partitioned by a slash. For example, a record that could be either a Eurasian Collared-Dove or Mourning Dove would read "ECDO/MODO". Some records could only be identified to genus; in that case, it would be depicted as (using a cottontail rabbit as an example: "*Sylvilagus* sp.". Some records are presumably scavenging and those are followed by "carcass" (e.g., white-tailed deer carcass"). Finally, birds that could not be identified to species but could be assigned a size class were listed as "large avian", "medium avian", or "small avian". See Methods for full description of how these size classes were assigned.
* name: moist; description: soil moisture measurement extracted from each location
* name: elev; description: the elevation above sea level in meters extracted from each location
* name: slope; description: the terrain's slope, in degrees, extracted from each location
* name: NDVI; description: the normalized difference vegetation index extracted from each location. This variable measures greeness of the landscape, with "0" having no green and "1" being entirely green
* name: DTNdecid; decription: distance to nearest deciduous wetland in meters extracted from each location
* name: DTNgrass; description: distance to nearest grassy field in meters extracted from each location
* name: DTNmatpine; description: distance to nearest mature pine forest in meters extracted from each location
* name: DTNwater; decription: distance to nearest permenant body of water in meters extracted from each location
* name: aspect; description: the terrain's aspect, in degrees, extracted from each location
* name: pttype; description: a duplicate of ptvalue, where "1" describes a venus flytrap location and "0" describes a background location.

#### File: LandscapeSUBMISSION.R

**Description: R code to use the extracted environmental data from Venus flytrap and background locations to create generalized linear models and model validation** 

## Code/software
### File: LandscapeSUBMISSION.R

**Description: R code to use the extracted environmental data from Venus flytrap and background locations to create generalized linear models and model validation** 


## Access information

Other publicly accessible locations of the data:

* No other locations

Data was derived from the following sources:

* See Methods

# vftLandscapeDataRepository
