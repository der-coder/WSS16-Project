(* ::Package:: *)

(* ::Title:: *)
(*Simulation attempt #01*)


(* ::Subchapter:: *)
(*Functions*)


updateSensors[pins_List]:=Module[{temporary,sensorVector},temporary=DeviceRead["GPIO",pins];sensorVector=Table[temporary[pins[[i]]],{i,Length[pins]}]]


getTimeDifference[tNow_,tBefore_]:=Module[{},Round[QuantityMagnitude[UnitConvert[tNow-tBefore,"Seconds"]]]]


updatePosition[currentTime_,statusOld_List,speed_List]:=Module[{newPosition},
newPosition=\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{"Round", "[", 
RowBox[{
RowBox[{"statusOld", "[", 
RowBox[{"[", "2", "]"}], "]"}], "+", 
RowBox[{
RowBox[{"{", 
RowBox[{"0", ",", 
RowBox[{"getTimeDifference", "[", 
RowBox[{"currentTime", ",", 
RowBox[{"statusOld", "[", 
RowBox[{"[", "1", "]"}], "]"}]}], "]"}]}], "}"}], "*", "speed"}]}], "]"}], 
RowBox[{
RowBox[{"statusOld", "[", 
RowBox[{"[", "3", "]"}], "]"}], "==", "\"\<Forward\>\""}]},
{
RowBox[{"Round", "[", 
RowBox[{
RowBox[{"statusOld", "[", 
RowBox[{"[", "2", "]"}], "]"}], "-", 
RowBox[{
RowBox[{"{", 
RowBox[{"0", ",", 
RowBox[{"getTimeDifference", "[", 
RowBox[{"currentTime", ",", 
RowBox[{"statusOld", "[", 
RowBox[{"[", "1", "]"}], "]"}]}], "]"}]}], "}"}], "*", "speed"}]}], "]"}], 
RowBox[{
RowBox[{"statusOld", "[", 
RowBox[{"[", "3", "]"}], "]"}], "==", "\"\<Back\>\""}]},
{
RowBox[{"Round", "[", 
RowBox[{
RowBox[{"statusOld", "[", 
RowBox[{"[", "2", "]"}], "]"}], "-", 
RowBox[{
RowBox[{"{", 
RowBox[{
RowBox[{"getTimeDifference", "[", 
RowBox[{"currentTime", ",", 
RowBox[{"statusOld", "[", 
RowBox[{"[", "1", "]"}], "]"}]}], "]"}], ",", "0"}], "}"}], "*", "speed"}]}], "]"}], 
RowBox[{
RowBox[{"statusOld", "[", 
RowBox[{"[", "3", "]"}], "]"}], "==", "\"\<Left\>\""}]},
{
RowBox[{"Round", "[", 
RowBox[{
RowBox[{"statusOld", "[", 
RowBox[{"[", "2", "]"}], "]"}], "+", 
RowBox[{
RowBox[{"{", 
RowBox[{
RowBox[{"getTimeDifference", "[", 
RowBox[{"currentTime", ",", 
RowBox[{"statusOld", "[", 
RowBox[{"[", "1", "]"}], "]"}]}], "]"}], ",", "0"}], "}"}], "*", "speed"}]}], "]"}], 
RowBox[{
RowBox[{"statusOld", "[", 
RowBox[{"[", "3", "]"}], "]"}], "==", "\"\<Right\>\""}]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False]\)
]


updateWalls2[location_,statusOld_List,sensorsNow_List]:=
Module[{newWalls,blockPerimeter},
blockPerimeter=sensorsNow*{{location[[1]],location[[2]]+1},{location[[1]]+1,location[[2]]},{location[[1]],location[[2]]-1},{location[[1]]-1,location[[2]]}};
blockPerimeter=DeleteCases[blockPerimeter,{0,0}];
newWalls=Join[statusOld[[5]],blockPerimeter];
If[Length[newWalls]>1,newWalls=DeleteCases[newWalls,{}],Nothing];

newWalls
]


updateMap[location_List,currentTime_,statusOld_List]:=Module[{newMap=statusOld[[6]],mapSurface},
mapSurface=\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{"Table", "[", 
RowBox[{
RowBox[{"{", 
RowBox[{
RowBox[{"location", "[", 
RowBox[{"[", "1", "]"}], "]"}], ",", 
RowBox[{
RowBox[{"statusOld", "[", 
RowBox[{"[", 
RowBox[{"2", ",", "2"}], "]"}], "]"}], "+", "i"}]}], "}"}], ",", 
RowBox[{"{", 
RowBox[{"i", ",", 
RowBox[{"getTimeDifference", "[", 
RowBox[{"currentTime", ",", 
RowBox[{"statusOld", "[", 
RowBox[{"[", "1", "]"}], "]"}]}], "]"}]}], "}"}]}], "]"}], 
RowBox[{
RowBox[{"statusOld", "[", 
RowBox[{"[", "3", "]"}], "]"}], "==", "\"\<Forward\>\""}]},
{
RowBox[{"Table", "[", 
RowBox[{
RowBox[{"{", 
RowBox[{
RowBox[{"location", "[", 
RowBox[{"[", "1", "]"}], "]"}], ",", 
RowBox[{
RowBox[{"statusOld", "[", 
RowBox[{"[", 
RowBox[{"2", ",", "2"}], "]"}], "]"}], "-", "i"}]}], "}"}], ",", 
RowBox[{"{", 
RowBox[{"i", ",", 
RowBox[{"getTimeDifference", "[", 
RowBox[{"currentTime", ",", 
RowBox[{"statusOld", "[", 
RowBox[{"[", "1", "]"}], "]"}]}], "]"}]}], "}"}]}], "]"}], 
RowBox[{
RowBox[{"statusOld", "[", 
RowBox[{"[", "3", "]"}], "]"}], "==", "\"\<Back\>\""}]},
{
RowBox[{"Table", "[", 
RowBox[{
RowBox[{"{", 
RowBox[{
RowBox[{
RowBox[{"statusOld", "[", 
RowBox[{"[", 
RowBox[{"2", ",", "1"}], "]"}], "]"}], "+", "i"}], ",", 
RowBox[{"location", "[", 
RowBox[{"[", "2", "]"}], "]"}]}], "}"}], ",", 
RowBox[{"{", 
RowBox[{"i", ",", 
RowBox[{"getTimeDifference", "[", 
RowBox[{"currentTime", ",", 
RowBox[{"statusOld", "[", 
RowBox[{"[", "1", "]"}], "]"}]}], "]"}]}], "}"}]}], "]"}], 
RowBox[{
RowBox[{"statusOld", "[", 
RowBox[{"[", "3", "]"}], "]"}], "==", "\"\<Right\>\""}]},
{
RowBox[{"Table", "[", 
RowBox[{
RowBox[{"{", 
RowBox[{
RowBox[{
RowBox[{"statusOld", "[", 
RowBox[{"[", 
RowBox[{"2", ",", "1"}], "]"}], "]"}], "-", "i"}], ",", 
RowBox[{"location", "[", 
RowBox[{"[", "2", "]"}], "]"}]}], "}"}], ",", 
RowBox[{"{", 
RowBox[{"i", ",", 
RowBox[{"getTimeDifference", "[", 
RowBox[{"currentTime", ",", 
RowBox[{"statusOld", "[", 
RowBox[{"[", "1", "]"}], "]"}]}], "]"}]}], "}"}]}], "]"}], 
RowBox[{
RowBox[{"statusOld", "[", 
RowBox[{"[", "3", "]"}], "]"}], "==", "\"\<Left\>\""}]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False]\);
newMap=Join[newMap,mapSurface];
newMap
]


updateDirection[dir_List,sensorsNow_List]:=Module[{possibleDirections=Table[If[sensorsNow[[i]]==1,Nothing,dir[[i]]],{i,Length[sensorsNow]}]},possibleDirections]


updateDirection2[dirs_List,buttons_List,current_String]:=Module[{i,obstacles,placeholder,placeholder2,nextdir},
obstacles=Table[1-buttons[[i]],{i,Length[buttons]}];
placeholder2=DeleteCases[dirs*obstacles,0];
If[
MemberQ[dirs*obstacles,current],
nextdir=current,
nextdir=RandomChoice[placeholder2]
]]


(* ::Subchapter:: *)
(*Initialization*)


pins={4,17,27,22};
sensors=updateSensors[pins];
position={0,0};
velocity={1,1};
map={{0,0}};
walls={{}};
directions={"Forward","Right","Back","Left"};
nextDirection=RandomChoice[directions];
bin=CreateDatabin["Name"->"Robot_SLAM"];
DatabinAdd[bin,{TimeObject[Now],position,nextDirection,sensors,walls,map}];
Print[sensors,nextDirection];
Pause[2];

sensors=updateSensors[pins];
timestampNew=TimeObject[Now];
previousStatus=Last[Values[bin]];
positionNew=updatePosition[timestampNew,previousStatus,velocity];
wallsNew=updateWalls2[positionNew,previousStatus,sensors];
mapNew=updateMap[position,timestampNew,previousStatus];
nextDirection=updateDirection2[directions,sensors,previousStatus[[3]]];
DatabinAdd[bin,{timestampNew,positionNew,nextDirection,sensors,wallsNew,mapNew}];
Print[sensors,nextDirection];
Pause[2];

Do[
sensors=updateSensors[pins];
timestampNew=TimeObject[Now];
previousStatus=Last[Values[bin]];
positionNew=updatePosition[timestampNew,previousStatus,velocity];
wallsNew=updateWalls2[positionNew,previousStatus,sensors];
mapNew=updateMap[position,timestampNew,previousStatus];
nextDirection=updateDirection2[directions,sensors,previousStatus[[3]]];
DatabinAdd[bin,{timestampNew,positionNew,nextDirection,sensors,wallsNew,mapNew}];
Print[sensors,nextDirection]
Pause[1];,{i,20}]
