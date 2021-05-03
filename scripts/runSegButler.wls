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
        Print@"2D not implemented yet. Use -imgstacks to analyse 3D data";Abort[]
    ];
    
    If[MemberQ[args,"-gpu"],
        gpu=True;
        Print@"Using GPU! Experimental...",
        gpu=False
    ];
  
    If[MemberQ[args,"-objSz"],
        objSz=ToExpression@First@Extract[args,Position[args,"-objSz"]+1];,
        Print@"script requires objectsize estimate in pixels!";Abort[]
    ];
     If[MemberQ[args,"-downscale"],
        downscalefactor=ToExpression@First@Extract[args,Position[args,"-downscale"]+1];
        ,
        Print@"script requires objectsize estimate in pixels!";Abort[]
    ];
    ,
    downscalefactor=0.5;
    stacks=True;
    imgdir="home/max/deepMirror Dropbox/Data/BasicCells/SophieKrusoe/Test";
    
    (*imgdir="/Users/MJ 1/DeepMirrorDropbox/deepMirror Dropbox/Data/BasicCells/SophieKrusoe/Test";*)
    objSz=120;
    gpu=True;
];


Print["downscaling set to: "<>ToString@downscalefactor];


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


(*clean3DComponents[comp_,objSz_]:=Module[{},
	bd=BorderDimensions@Image3D@comp[[2]];
	SparseArray@Round@ImageData@ImagePad[DeleteSmallComponents[Dilation[Erosion[Closing[FillingTransform@ImagePad[Image3D@comp[[2]],-bd],Ceiling[objSz/50]],DiskMatrix@Table[Ceiling[objSz/50],3]],DiskMatrix@Table[Ceiling[objSz/50],3]],((objSz)^3)/10],bd]*comp[[1]]
]*)


clean3DComponents[comp_,objSz_]:=Module[{},
	rules=Most@ArrayRules@comp[[2]];
	mins=Min/@Transpose[rules[[;;,1]]]-1;
	maxs=Max/@Transpose[rules[[;;,1]]]-1;
	subSA=SparseArray@Map[#[[1]]-mins->#[[2]]&,rules];
	subSA=SparseArray@Round@ImageData@DeleteSmallComponents[Dilation[Erosion[Closing[FillingTransform@Image3D@subSA,Ceiling[objSz/50]],DiskMatrix@Table[Ceiling[objSz/50],3]],DiskMatrix@Table[Ceiling[objSz/50],3]],(objSz^3)/50]*comp[[1]];
	rules=Most@ArrayRules@subSA;
	SparseArray[Map[#[[1]]+mins->#[[2]]&,rules],Dimensions@comp[[2]]]
]


load3Dstack[file_,downscalefactor_]:=Module[{fac},
		fac=ToExpression@StringReplace[Part[StringSplit[StringTake[file,StringPosition[file,"z"~~___~~"."]],{".","_"}],1,2],"-"->"."]/ToExpression@StringReplace[Part[StringSplit[StringTake[file,StringPosition[file,"xy"~~___~~"_"]],{".","_"}],1,2],"-"->"."];
		exif=Import[file,"Exif"];
		Print["z scale= "<>ToString@fac];
		channels=ToExpression@First@StringTake[StringTake[exif["ImageDescription"],StringPosition[exif["ImageDescription"],"channels="~~_]],-1];
		Print["channels found: "<>ToString@channels];
		If[NumberQ@fac,
			img3D=Image3D@Map[ColorCombine[#,"RGB"]&,Most/@Partition[Import@file,channels]];
			ImageResize[img3D,Scaled/@(downscalefactor*{1,1,fac})],
			Print@"couldnt calculate zscale from file name!";Abort[]]
	];


analyse3Dstack[file_,objSzIN_,modelSz_,model_,threshold_:0.5,gpu_:False,downscalefactor_:1]:=Module[{objSz},
		Print["Analysing: "<>FileBaseName@file];
		img3D=load3Dstack[file,downscalefactor];
		Print@ImageDimensions@img3D;
		objSz=downscalefactor*objSzIN;
		Print@"running SegButler...";
		res=SegButler3D@<|"image"->img3D,"objSz"->objSz,"modelSz"->modelSz,"model"->model,"threshold"->threshold,"targetDevice"->If[gpu,"GPU","CPU"],"zFac"->1,"morphoBinFac"->.5|>;
		
		If[res["segmented"]==0,
			Print["Nothing found! Try different net or threshold"];];
		
	
		Print@"cleaning...";
		comps=ComponentMeasurements[DeleteSmallComponents[Normal@res["segmented"],(objSz^3)/50],"Mask"];
		cleanSegmented=SparseArray[Flatten[Map[Most@ArrayRules@clean3DComponents[#,objSz]&,comps],1],Dimensions@res["segmented"]];
		
		Print@"resizing...";
		originalDim=ImageDimensions@img3D;
		cleanSegmented=Round@ArrayResample[cleanSegmented,{originalDim[[3]],originalDim[[2]],originalDim[[1]]},Resampling->"NearestLeft"];

		Print@"making pretty overlay...";
		slices=Image3DSlices[img3D,All];
		cslices=Image3DSlices[Colorize@cleanSegmented,All];
		overlays=MapThread[ImageAssemble@{ImageAdjust@#1,Blend[{ImageAdjust@#1,ImageCompose[SetAlphaChannel@ImageAdjust@#1,RemoveBackground[#2,{Black,.01}]]},.5]}&,{slices,cslices}];
		
		segslices=Image3DSlices[Image3D[Round[cleanSegmented],"Bit16"],All];
		
		outputdir=FileNameJoin@{FileNameJoin[FileNameSplit[file][[;;-2]]],FileBaseName[file]<>"_RESULTS"};
		CreateDirectory[outputdir];
		
		Print@"Caclulating measurements";
		measurements=ComponentMeasurements[cleanSegmented,{"Count","PerimeterCount","Circularity","Length","Width"},All,"Dataset"];
		Export[FileNameJoin@{outputdir,"morphologicalQuantifications.csv"},measurements];
		
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
		Map[analyse3Dstack[#,objSz,modelSz,model,0.5,gpu,downscalefactor]&,imgFiles],
		Print["No images found in: "<>FileNameJoin@{First@DirectoryStack[],imgdir}]]
]


(* ::Subsection:: *)
(*Debugging Section*)


(*imgFiles=If[StringTake[imgdir,1]=="/",
		FileNames["*.tif",imgdir],
		FileNames["*.tif",FileNameJoin@{First@DirectoryStack[],imgdir}]]*)


(*file=imgFiles[[1]]
objSzIN=objSz;
modelSz
model
threshold=0.5
gpu=False
downscalefactor*)


(*Print["Analysing: "<>FileBaseName@file];
		img3D=load3Dstack[file,downscalefactor];
*)



