Example of a traffic light controller developed by Galois for the STTR.

When input 'tick' is True, it changes the light from red to yellow to green to red.
When input 'tick' is False, the light remains the same color.
There is a liveness property for input 'tick' and for output 'green'.

There are three different versions that demonstrate Salty features:
* trafficLight.salt -     basic encoding
* trafficLight.salt -     uses aggregation operators 'any' and 'mutex' 
                          and the 'if-then-else' construct
* trafficLightEnum.salt - uses an 'Enum' for the color of the light

