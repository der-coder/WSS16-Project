(* File created by Isaac Ayala*)

DeviceWrite["GPIO",{27->0,22->0}]

status=DeviceRead["GPIO",4];
Print[status]

If[status==1,DeviceWrite["GPIO",{27->0,22->1}],Print["Dangit"]]
Pause[3];
DeviceWrite["GPIO",{27->0,22->0}]
state=0;
Do[Pause[1];state=1-state;DeviceWrite["GPIO",{27->state,22->(1-state)}],{i,5}]
DeviceWrite["GPIO",{27->0,22->0}]
