(* File created by Isaac Ayala*)
(* If[DeviceRead["GPIO",4]==0,Print["Hello"],Print["OFF"]] Didn't work*)

DeviceWrite["GPIO",23->1] (* Works *)

state=0;

For[
i=0,
i<4,
i++,
Pause[3];
state=1-state;
DeviceWrite["GPIO",24->state]
]

DeviceWrite["GPIO",{23->0,24->0}]
