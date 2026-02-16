---
title: "Here Today, Gone Tomorrow: Dynamics of Peacekeeper Entry and Exit on Violence Against Civilians"
subtitle: "Codebook"
format: 
  typst:
    margin:
      top: 1cm
---

**Feb. 16, 2026**

**Authors:** **Sky Kunkel**^[Postdoctoral Research Associate, Gender and Security Sector Lab, Cornell University. Email: [sk3386@cornell.edu](mailto:sk3386@cornell.edu), web: [www.skytheacademic.com](http://www.skytheacademic.com)] \ \ \ \ **Douglas B. Atkinson**^[Assistant Professor of Political Science, Brigham Young University. Email: [atkinsond@byu.edu](mailto:atkinsond@byu.edu), web: [dougbatkinson.wordpress.com](https://dougbatkinson.wordpress.com)] \ \ \ \ **Rebecca Dudley**^[Assistant Professor of Political Science, Brigham Young University. Email: [rebecca.dudley@byu.edu](mailto:rebecca.dudley@byu.edu), web: [www.rebeccaedudley.com](https://www.rebeccaedudley.com)] \ \ \ \ **Zach Warner**^[Independent researcher. Email: [zachwarner11@gmail.com](mailto:zachwarner11@gmail.com), web: [zachwarner.net](http://www.zachwarner.net)]

---

**Replication Materials:** [https://github.com/skytheacademic/when_peacekeepers_leave](https://github.com/skytheacademic/when_peacekeepers_leave)

This codebook describes the variables for "Here Today, Gone Tomorrow" in the *International Political Science Review*. The data used (`Kunkel-Atkinson-Dudley-Warner-final.RDS`) is a merged PRIO-GRID × month-year panel dataset covering Africa from January 2000 through December 2017. The unit of observation is a PRIO-GRID cell–month. Data are drawn from ACLED, RADPKO, and PRIO.

## Dataset Sources

This dataset integrates variables from multiple sources. For detailed information about variables from external datasets, please consult the original codebooks:

### ACLED Variables
All variables prefixed with `acled_` are derived from the Armed Conflict Location & Event Data (ACLED) Project. These variables capture information on political violence events, demonstrations, and strategic developments. For complete definitions, coding rules, and methodology, please refer to the ACLED Codebook (included in this repository as `ACLED_Codebook_v1_January-2021.pdf`).

**Citation**: Raleigh, Clionadh, Andrew Linke, Håvard Hegre and Joakim Karlsen. 2010. "Introducing ACLED-Armed Conflict Location and Event Data." Journal of Peace Research 47(5): 651-660.

### RADPKO Variables
All variables prefixed with `radpko_` are derived from the RADPKO (Robust African Deployments of Peacekeeping Operations) dataset. These variables provide detailed information on UN peacekeeping deployments at the subnational level, including troop counts, personnel types, and contributing countries. For complete definitions and methodology, please refer to the RADPKO documentation (included in this repository as `radpko_appendix.pdf`).

**Citation**: Hunnicutt, Patrick and William G. Nomikos. 2020. "Nationality, Gender, and Deployments at the Local Level: Introducing the RADPKO Dataset." International Peacekeeping 27(4): 645-672.

### PRIO-GRID Variables
All variables prefixed with `prio_` (or `priogrid_`) as well as core spatial identifiers (`gid`, `row`, `col`, `gwno`) are derived from the PRIO-GRID dataset. PRIO-GRID provides a standardized spatial grid structure with global coverage at 0.5 x 0.5 decimal degree resolution, integrating data on geography, climate, population, and resources. For complete definitions and methodology, please refer to the PRIO-GRID Codebook (included in this repository as `PRIO-GRID-Codebook.pdf`).

**Citation**: Tollefsen, Andreas Forø, Håvard Strand & Halvard Buhaug (2012) "PRIO-GRID: A unified spatial data structure." Journal of Peace Research 49(2): 363-374.

## Project-Specific Variables

The following variables were created specifically for this analysis:

### Identifiers & Time

Core identifiers that locate each observation in space and time.

| Variable | Description | Type |
|----------|-------------|------|
| `year` | Calendar year (2000-2017) | Integer |
| `month` | Calendar month (1 = January, 12 = December) | Integer |
| `time` | Unified time index: (year – 2000) × 12 + month. Positive integer required by the did package | Integer |

**Note**: The spatial identifiers `gid` (PRIO-GRID cell identifier), `row` (PRIO-GRID row index), and `col` (PRIO-GRID column index) originate from the PRIO-GRID framework. Please see the PRIO-GRID Codebook for their definitions.

### Treatment Variables

Variables constructed for use with the did package. Two treatments are defined: (1) peacekeepers arrive, and (2) peacekeepers leave.

#### Peacekeeper Arrival

| Variable | Description | Type |
|----------|-------------|------|
| `first_treated` | Time period of first PKO deployment in the cell. 0 = never treated (control) | Integer |
| `treated` | Binary: 1 if PKO ever deployed in the cell, 0 otherwise | Binary |
| `post_treatment` | Binary: 1 from the period of first deployment onward, 0 before | Binary |

#### Peacekeeper Departure

| Variable | Description | Type |
|----------|-------------|------|
| `first_treated_leave` | Time period when PKO first departs (deployed→not deployed). 0 = never departed | Integer |
| `treated_leave` | Binary: 1 if PKO ever departed the cell, 0 otherwise | Binary |
| `post_treatment_leave` | Binary: 1 from the period of first departure onward, 0 before | Binary |

### Neighbor (Spatial Spillover) Variables

Computed via queen contiguity: for each cell–month, these are the sums of the corresponding ACLED/RADPKO variables across all adjacent cells. Prefixed `neighbor_`. Naming follows the same convention as ACLED variables.

| Variable | Description | Type |
|----------|-------------|------|
| `neighbor_vac_gov_death_all` | Sum of gov't VAC fatalities in neighbors | Integer |
| `neighbor_vac_gov_death_any` | Count of neighbors with any gov't VAC fatalities | Integer |
| `neighbor_vac_reb_death_all` | Sum of rebel VAC fatalities in neighbors | Integer |
| `neighbor_vac_reb_death_any` | Count of neighbors with any rebel VAC fatalities | Integer |
| `neighbor_vac_gov_event_all` | Sum of gov't VAC events in neighbors | Integer |
| `neighbor_vac_gov_event_any` | Count of neighbors with any gov't VAC events | Integer |
| `neighbor_vac_reb_event_all` | Sum of rebel VAC events in neighbors | Integer |
| `neighbor_vac_reb_event_any` | Count of neighbors with any rebel VAC events | Integer |
| `neighbor_gov_death_all` | Sum of all gov't fatalities in neighbors | Integer |
| `neighbor_gov_death_any` | Count of neighbors with any gov't fatalities | Integer |
| `neighbor_reb_death_all` | Sum of all rebel fatalities in neighbors | Integer |
| `neighbor_reb_death_any` | Count of neighbors with any rebel fatalities | Integer |
| `neighbor_gov_event_all` | Sum of all gov't events in neighbors | Integer |
| `neighbor_gov_event_any` | Count of neighbors with any gov't events | Integer |
| `neighbor_reb_event_all` | Sum of all rebel events in neighbors | Integer |
| `neighbor_reb_event_any` | Count of neighbors with any rebel events | Integer |
| `neighbor_bat_death_all` | Sum of battle fatalities in neighbors | Integer |
| `neighbor_bat_death_any` | Count of neighbors with any battle fatalities | Integer |
| `neighbor_bat_event_all` | Sum of battle events in neighbors | Integer |
| `neighbor_bat_event_any` | Count of neighbors with any battle events | Integer |
| `neighbor_pko_any` | Count of neighbors with any PKO deployment | Integer |

## Notes

- **Coverage:** Africa, January 2000 – December 2017, at the PRIO-GRID cell–month level.
- **Missing values:** RADPKO missing values within the study area are recoded to 0, as NA in RADPKO reflects non-deployment.
- **Binary variables:** "_any" variables are recoded to {0, 1} after aggregation.
- **Neighbor structure:** Queen contiguity (shared edge or vertex) defines the neighbor structure for spatial spillover variables.
