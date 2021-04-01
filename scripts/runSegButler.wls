#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Subsubsection:: *)
(*Load packages*)


dir=DirectoryName@$InputFileName;
If[dir=="",dir=NotebookDirectory[]];
SetDirectory[dir];


Get@FileNameJoin@{dir,"..","packages","SegButler.wl"}


(* ::Subsubsection:: *)
(*check runtime args*)


If[Length@$ScriptCommandLine>0,
    args=Rest@$ScriptCommandLine;
    If[MemberQ[args,"-imgdir"],
        imgdir=First@Extract[args,Position[args,"-imgdir"]+1],
        Print@"-imgdir runtime argument required for folder to analyse";Abort[]
    ];
    If[MemberQ[args,"-imgstacks"],
        stacks=True,
        Print@"2D not implemented yet. Use -imgstacks to analsye 3D data";Abort[]
    ];
  
    If[MemberQ[args,"-objSz"],
        objSz=ToExpression@First@Extract[args,Position[args,"-objSz"]+1];,
        Print@"script requires objectsize estimate in pixels!";Abort[]
    ];,
    stacks=True;
    imgdir="/Users/MJ 1/Dropbox (Cambridge University)/Documents/Science/CodeRepositories/SegButler/testdata/test3D";
    objSz=120;
];


Print["analysing folder: "<>imgdir];


(* ::Subsubsection:: *)
(*Load config and model*)


config=<|Import[dir<>"config.json"]|>;


If[FileExistsQ@config["model"],
	model=loadSemiSuperNet@config["model"],
	Print@"model not found check config path!";Abort[]];


If[Not[Head@model==NetGraph],Print@"Error loading model!";Abort[]]


(*get model size*)
modelSz=ToExpression@StringTake[First@Select[StringSplit[config["model"],"_"],StringContainsQ[#,"objSz"]&],6;;]
If[NumberQ@modelSz,
	Print["Model Size is: "<>ToString@modelSz],
	Print["Error finding model Sz"];Abort[]
];


(* ::Subsubsection:: *)
(*Apply SegButler*)


clean3DComponents[comp_,objSz_]:=Module[{},
	bd=BorderDimensions@Image3D@comp[[2]];
	SparseArray@Round@ImageData@ImagePad[DeleteSmallComponents[Dilation[Erosion[Closing[FillingTransform@ImagePad[Image3D@comp[[2]],-bd],Round[objSz/10]],Round[objSz/10]],Round[objSz/10]],(objSz^3)/3],bd]*comp[[1]]
]


load3Dstack[file_,orginalSz_,targetSz_]:=Module[{fac},
		fac=ToExpression@StringReplace[Part[StringSplit[StringTake[file,StringPosition[file,"z"~~___~~"."]],{".","_"}],1,2],"-"->"."]/ToExpression@StringReplace[Part[StringSplit[StringTake[file,StringPosition[file,"xy"~~___~~"_"]],{".","_"}],1,2],"-"->"."];
		exif=Import[file,"Exif"];
		channels=ToExpression@First@StringTake[StringTake[exif["ImageDescription"],StringPosition[exif["ImageDescription"],"channels="~~_]],-1];
		Print["channels found: "<>ToString@channels];
		If[NumberQ@fac,
			img3D=Image3D@Map[ColorCombine[#,"RGB"]&,Most/@Partition[Import@file,channels]];
			{ImageResize[img3D,Scaled/@(targetSz/orginalSz*{1,1,fac})],img3D},
			Print@"couldnt calculate zscale from file name!";Abort[]]
	];


analyse3Dstack[file_,objSz_,modelSz_,model_,threshold_:0.5,gpu_:False]:=Module[{},
		Print["Analysing: "<>FileBaseName@file];
		{img3D,originalimg3D}=load3Dstack[file,objSz,modelSz];
		
		Print@"running SegButler...";
		res=SegButler3D@<|"image"->img3D,"objSz"->modelSz,"modelSz"->modelSz,"model"->model,"threshold"->threshold,"targetDevice"->If[gpu,"GPU","CPU"],"zFac"->1|>;
		
		If[res["segmented"]==0,
			Print["Nothing found! Try different net or threshold"];];
		
	
		Print@"cleaning...";
		comps=ComponentMeasurements[DeleteSmallComponents[Normal@res["segmented"],(modelSz^3)/3],"Mask"];
		cleanSegmented=SparseArray@Round@ImageData@ImageApply[Max,Map[Image3D@clean3DComponents[#,modelSz]&,comps]];
		
		Print@"resizing...";
		originalDim=ImageDimensions@originalimg3D;
		cleanSegmented=Round@ArrayResample[cleanSegmented,{originalDim[[3]],originalDim[[1]],originalDim[[2]]},Resampling->"NearestLeft"];

		Print@"making pretty overlay...";
		slices=Image3DSlices[originalimg3D,All];
		cslices=Image3DSlices[Colorize@cleanSegmented,All];
		overlays=MapThread[ImageAssemble@{ImageAdjust@#1,Blend[{ImageAdjust@#1,ImageCompose[SetAlphaChannel@ImageAdjust@#1,RemoveBackground[#2,{Black,.01}]]},.5]}&,{slices,cslices}];
		
		segslices=Image3DSlices[Image3D[Round[cleanSegmented],"Bit16"],All];
		
		outputdir=FileNameJoin@{FileNameJoin[FileNameSplit[file][[;;-2]]],FileBaseName[file]<>"_RESULTS"};
		CreateDirectory[outputdir];
		CreateDirectory[FileNameJoin@{outputdir,"overlays"}];
		CreateDirectory[FileNameJoin@{outputdir,"binary"}];
		
		Table[Export[FileNameJoin@{outputdir,"binary","seg"<>ToString@i<>".png"},segslices[[i]]],{i,Length@segslices}];
		Table[Export[FileNameJoin@{outputdir,"overlays","overlay"<>ToString@i<>".png"},overlays[[i]]],{i,Length@overlays}];
	];


If[stacks,
	imgFiles=If[StringTake[imgdir,1]=="/",
		FileNames["*.tif",imgdir],
		FileNames["*.tif",FileNameJoin@{First@DirectoryStack[],imgdir}]];	
	If[Length@imgFiles>0,
		Map[analyse3Dstack[#,objSz,modelSz,model,0.5,False]&,imgFiles],
		Print["No images found in: "<>FileNameJoin@{First@DirectoryStack[],imgdir}]]
]
