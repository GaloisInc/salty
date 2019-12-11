# Collision Avoidance Example

This is an example of using a Salty synthesized controller to control unmanned air vehicles (UAVs) following the collision avoidance protocol for UAVs in an airspace with multiple altitude layers.

### Dependencies
- Python 2
- Salty
- Enum34
`$ sudo pip install enum34`
- Pygame
`$ sudo pip install pygame`
- Jinja2
`$ sudo pip install jinja2`

## Running the Example

1. Generate the controller
  `$ ./gen_ctrl.sh`
  First, this script will generate `ctrl.salt` by running the script `gen_ltl.py` which uses 
  `ltl_temp.tl` as a template. then will invoke salty on `ctrl.salt` and generate `Controller.py`, 
  a concrete implementation of the controller in python.

2. Run the simulation
  `$ python simulation.py`
  The script will load the controller `Controller.py` and the pygame environment. 
  The simulation should display on the screen.
