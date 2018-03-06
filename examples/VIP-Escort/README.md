# VIP Escort Example

This is an example of using a Salty synthesized controller to control unmanned air vehicles (UAVs) following the VIP-escort protocol for UAVs in the presence of an adversary.

## Running the Example

### Dependencies

- Python 2

- Salty

- OpenUxAS

- OpenAMASE

### Steps:

- Generate the controller

`$ salty ctrl/vip.salt -p`

This will invoke salty on `vip.salt` and generate `Vip.py`, a concrete implementation of the controller in python.

- Initialize OpenAMASE with the `Scenario_VIPEscort.xml` file:

`$cd uxasFiles; ./runAMASE_VIPEscort.sh`

- Initialize OpenUxAS with the `cfg_VIPEscort.xml` file:

`$ ./runAMASE_VIPEscort.sh`

- Run the simulation

`$ cd ..; python simulation.py`

Once the controller is loaded (it may take some time depending on the controller's size) a message will be printed that the controller been successfully loaded.

- Hit play in OpenAMASE and watch the simulation

Or you can simply play this demo:

[![Alt text for your video](https://img.youtube.com/vi/sCLT1iMOpnQ/0.jpg)](https://youtu.be/sCLT1iMOpnQ)
