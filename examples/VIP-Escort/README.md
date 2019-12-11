# VIP Escort Example

This is an example of using a Salty synthesized controller to control unmanned air vehicles 
(UAVs) following the VIP-escort protocol for UAVs in the presence of an adversary.

## Description

In this scenario we have one main UAV we call “VIP”, multiple support UAVs we call “escorts”, 
one adversary UAV we call “enemy”, and multiple pre-defined locations on the map. These 
locations are shown in green in fig 2. Our aim is to automatically synthesize a control protocol 
that guarantees the following four properties. First, The VIP can only fly from one location to 
another if it is being followed by one of the escorts. Second, The VIP cannot visit certain 
locations until at least one of the escorts have previously inspected by flying over the location. 
In order for the UAVs not to pass through the prohibited regions shown in red in fig 2, the UAVs 
must follow certain paths between the locations in green. For example, to go from the bottom right 
location to the upper right, the UAVs must pass through the location in the center in order to not 
fly over the prohibited regions.

## Dependencies
Install the following dependencies:
- Python 3
- Python 3 ZMQ
  `pip install zmq`
- LocalCoords
  `cd libs/localcoords`
  `sudo python3 setup.py install`
   `cd ../..`
- [Salty](https://github.com/GaloisInc/salty)
- [OpenUxAS](https://github.com/afrl-rq/OpenUxAS)
- [OpenAMASE](https://github.com/afrl-rq/OpenAMASE)
## Running the Example

1. Generate the controller:
  `$ salty ctrl/vip.salt -p`
  This will invoke salty on `vip.salt` and generate `Vip.py`, a concrete implementation of the controller in python.

2. Set the path to the AMASE base directory, e.g.:
  `$ export AMASE_PATH="/<mypath_to_AMASE>/OpenAMASE/OpenAMASE"`

3. Initialize OpenAMASE with the `Scenario_VIPEscort.xml` file:
  `$ cd UxasFiles; ./runAMASE_VIPEscort.sh`

4. Set thepath to the UxAS executable, e.g.:
  `$ export UXAS_PATH="/<my_path_to_UxAS>/OpenUxAS/build"`

5. Initialize OpenUxAS with the `cfg_VIPEscort.xml` file:
  `$ ./runAMASE_VIPEscort.sh`

6. Run the simulation
  `$ cd ..; python simulation.py`
  Once the controller is loaded (it may take some time depending on the controller's size) 
  a message will be printed that the controller been successfully loaded.

7. Hit play in OpenAMASE and watch the simulation

8. Or simply play this demo if you just want to see the action:

[![Alt text for your video](https://img.youtube.com/vi/sCLT1iMOpnQ/0.jpg)](https://youtu.be/sCLT1iMOpnQ)
