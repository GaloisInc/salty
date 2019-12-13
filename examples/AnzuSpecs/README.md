# Anzu GR(1) Specifications

This directory contains example GR(1) specifications pulled from Anzu,
at http://www.ist.tugraz.at/staff/jobstmann/anzu/

- psltosalty.py
   A script for converting Anzu PSL GR(1) specs to Salty format.
- arbiter
   ARM AMBA AHB Arbiter specs.
   See the paper ["Automatic hardware synthesis from specification: A case   study"](http://www.ist.tugraz.at/staff/jobstmann/anzu/DAC07amba.pdf)
- genBufCfg
   IBM GenBuf Controller. 
   See the paper ["Specify, compile, run: Hardware from PSL"](http://www.ist.tugraz.at/staff/jobstmann/anzu/cocv.pdf).
- runningLights
  Line of LEDs that light up in sequence depending on which button is pressed.
  - genRunningLights.py
    A script for creating variable sized specifications for the running lights example.
- cooperativeSearch
  Two UAVs search for a target. 
  See the paper ["Salty-A Domain Specific Language for GR(1) Specifications and Designs"](https://ieeexplore.ieee.org/document/8793722).
