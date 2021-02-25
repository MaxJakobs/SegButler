#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Section:: *)
(*Load packages*)


dir=DirectoryName@$InputFileName;
If[dir=="",dir=NotebookDirectory[]];


packages=FileNames["*.wl",dir<>"packages"]


Get/@packages;


(* ::Section:: *)
(*Tests*)


(*Tetsing goes here*)


(*Print@"test results:"
report=TestReport@{uniButtest,biButtest,biButpproctest};
Print@Normal@Dataset[report[{"TimeElapsed","TestsFailedCount","TestsSucceededCount"}]]
*)


(* ::Section:: *)
(*Deploy*)


If[(*report["TestsFailedCount"]==0*)True,
Print@"Deploying code...";
contexts=Map[#<>"`"&,FileBaseName/@packages];
DumpSave["dump.mx", contexts];
DeleteObject[CloudObject["packages/SegButler/code64x"]];
obj = CopyFile["dump.mx", CloudObject["packages/SegButler/code64x"]];
SetOptions[obj, Permissions -> "Public"];
DeleteFile["dump.mx"]
]



