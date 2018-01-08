# Collision Avoidance Example

This is an example of using a Salty synthesized controller to control unmanned air vehicles (UAVs) following the collision avoidance protocol for UAVs in an airspace with multiple altitude layers.

## Running the Example

### Dependencies

- Python 2

- Salty

- Pygame

Run `sudo pip install pygame`

- Jinja2

Run `sudo pip install jinja2`

### Steps:

- Generate the controller

Run `./gen_ctrl.sh`

First, this script will generate `ctrl.salt` by running the script `gen_ltl.py` which uses `ltl_temp.tl` as a template. then will invoke salty on `ctrl.salt` and generate `Controller.py`, a concrete implementation of the controller in python.

- Run the simulation

Run `python simulation.py`

The script will load the controller `Controller.py` and the environment `pygame_env.py` and will run the simulation on the screen.
