# Reflex-Map
Converter of the maps (brushes, some items, lights and spawns) from Reflex Arena to Quake1/Trenchbroom format.
![example](https://github.com/fourier/reflex-map/raw/screenshots/screenshot1.png "Example")

It is implemented with SBCL on Linux and compiled for Windows with LispWorks 7.1/32bit
## Copyright

Copyright (c) Alexey Veretennikov, 2018-2019

## Usage
Just run the GUI. The GUI supports drag&n&drop operation of the input file.

The output file is the TrenchBroom-readable file containing geometry from the input file.
All prefabs exported if they were defined as entities with position in the original file.

Additionally some other entities exported:

- Spawns
- Some lights, according to the following mapping:

  | Reflex light name | Trenchbroom light name |
  | --- | --- |
  | ```industrial/lights/light_rnd``` | ```light_globe``` |
  | ```industrial/lights/light_fluorescent``` | ```light_flame_large_yellow``` |
  | ```industrial/lights/light_step_sml``` | ```light``` |

- Some items exported as well. Supported conversions:

  | Reflex item | QW Item |
  | --- | --- |
  | Shotgun | SuperShotGun |
  | Grenade Launcher | Grenade Launcher |
  | Plasmagun | Nailgun |
  | Rocket Launcher | Rocket Launcher |
  | Ion Gun | Super Nail Gun |
  | Bolt Rifle | Lightning Gun |
  | Shotgun shells | Shotgun shells |
  | Grenades | Rockets |
  | Plasma cells | Spikes |
  | Rockets | Rockets |
  | Ion cells | Spikes |
  | Bolt cells | LG cells |
  | 25 HP | 15 HP |
  | 50 HP | 25 HP |
  | Megahealth | Megahealth |
  | Green armor | Green armor |
  | Yellow armor | Yellow armor |
  | Red armor | Red armor |

## Installation
Download Windows executable from [Releases page](https://github.com/fourier/reflex-map/releases)

The archives in releases contain executable.
