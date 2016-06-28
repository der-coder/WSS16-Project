(* File created by Isaac Ayala*)
(* If[DeviceRead["GPIO",4]==0,Print["Hello"],Print["OFF"]] Didn't work*)

DeviceWrite["GPIO",23->0] (* Works *)
pins={4,17,27,22};
state=0;

For[i=0,i<4,i++,Pause[1];state=1-state;DeviceWrite["GPIO",24->state]]

DeviceWrite["GPIO",{23->0,24->0}]

status=DeviceRead["GPIO",pins];
Print[status[4]]
Print["Success"]

If[
status[4]==1,
For[
i=0,
i<4,
i++,
Pause[0.5];
state=1-state;
DeviceWrite["GPIO",{24->state,23->(1-state)}]
],
Print["Dangit"]
]
Pause[1];
DeviceWrite["GPIO",{23->0,24->0}]

lights=Total[{status[4],status[17],status[27],status[22]}];
lights=IntegerDigits[lights,2];
Print[lights]
