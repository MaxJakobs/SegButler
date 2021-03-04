#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Section:: *)
(*Load packages*)


dir=DirectoryName@$InputFileName;
If[dir=="",dir=NotebookDirectory[]];


packages=FileNames["*.wl",FileNameJoin@{dir,"..","packages"}]


Get/@packages;


(* ::Section:: *)
(*Tests*)


tests={};


Print@"loading models..."
SetDirectory[FileNameJoin@{dir,"..","scripts"}];
Run["sh get_models.sh"];


Print@"testing nuclei..."
nucNets=<|"sizeup"->Import[FileNameJoin@{dir,"../models/nucSZUP_1.mx"}]["TrainedNet"],
	15->Import[FileNameJoin@{dir,"../models/nuc15_1.mx"}]["TrainedNet"]|>;


Print@"testing cell..."
cellNets=<|"sizeup"->Import[FileNameJoin@{dir,"../models/cellSZUP_1.mx"}]["TrainedNet"],
	30->Import[FileNameJoin@{dir,"../models/cell30_1.mx"}]["TrainedNet"]|>;


testimg=Import[dir<>"testimg.png"];


testOutput=<|"Cells"->SegmentationButler@<|"image"->testimg,"nets"->cellNets,"smoothOutput"->True,"DEBUG"->True|>,
			"Nuclei"->SegmentationButler@<|"image"->testimg,"nets"->nucNets,"smoothOutput"->True,"DEBUG"->True|>|>;


AppendTo[tests,VerificationTest[Round/@Max/@Image/@testOutput[[;;,"objects"]],<|"Cells"->13,"Nuclei"->12|>]]


(*Tetsing goes here*)


Print@"test results:"
report=TestReport@tests;
Print@Normal@Dataset[report[{"TimeElapsed","TestsFailedCount","TestsSucceededCount"}]]


(* ::Section:: *)
(*Deploy*)


If[report["TestsFailedCount"]==0,
Print@"Deploying code...";
contexts=Map[#<>"`"&,FileBaseName/@packages];
DumpSave["dump.mx", contexts];
DeleteObject[CloudObject["packages/SegButler/code64x"]];
DeleteObject[CloudObject["packages/SegButler/SegButler.wl"]];
obj = CopyFile["dump.mx", CloudObject["packages/SegButler/code64x"]];
SetOptions[obj, Permissions -> "Public"];
obj = CopyFile[packages[[1]], CloudObject["packages/SegButler/SegButler.wl"]];
SetOptions[obj, Permissions -> "Public"];
DeleteFile["dump.mx"]
]
