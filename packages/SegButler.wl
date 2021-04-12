(* ::Package:: *)

BeginPackage["SegButler`"]
netevaluate::usage="evaluate net"
netevaluateGradNet::usage="evaluate gradient net"
segment2DImageGradientMap::usage="segment image with gradient map"
getpmap::usage="get pmap"
segment2DImageGradientMap::usage="segment output from gradeintnetwork"
netevaluateSizeUP::usage="setnetevaluate[set_,net_] Evaluate net over input set of images"
netevaluateSizeUPfcn::usage="setnetevaluate[set_,net_] Evaluate net over input set of images"
segmentPmapWithGradients::usage=".."
segment3DImageGradientMap::usage="segmentents 3D image"

SegButler3D::usage="segments 3D image (experimental)
	Inputs:
	threshold->bin threshold,\[IndentingNewLine]	targetDevice->GPU or CPU\[IndentingNewLine]	objSz->size of object in image\[IndentingNewLine]	modelSz-> size on which the model was trained\[IndentingNewLine]	model->neural network model"


SegmentationButler::usage="segments a 2D image (top level function), Takes an input associaion:
	Required Inputs:
	image->Image to analyse
	imgObjectSize-> size of objects in image
	nets-><|size->net,sizeupt->net,..|>
	Optional (Default):
	smoothOutput-> False upsamples image before gradient ascend
	binaryThreshold-> 0.5
	morphoBinFac-> 1 factor for morphological binarisation, <1 useful if objects are not fully recognised
	unhinged-> False (no limits on size and computation time)"

loadSemiSuperNet::usage="loads semisuper net from file to be used by package"


Begin["`Private`"]


loadSemiSuperNet[mxFile_]:=Module[{tmp=NetExtract[Import[mxFile]["TrainedNet"],"stylenetLabeled"]},NetReplacePart[tmp,
	{"Input"->NetEncoder[{"Image",Rest@NetExtract[tmp,"Input"],If[First@NetExtract[tmp,"Input"]==1,"Grayscale","RGB"]}],"Binary"->Flatten@{Rest@NetExtract[tmp,"Input"],2},"horzGrad"->NetDecoder[{"Image","Grayscale"}],"vertGrad"->NetDecoder[{"Image","Grayscale"}]}]]


removeOutlierpixels[img_,lq_:.001,uq_:.999,rand_:False]:=ImageAdjust[img,0,{Quantile[img,lq],Quantile[img,uq]},{0,1}]


netevaluate[img_,net_,targetDevice_:"CPU"]:=Module[{dim=ImageDimensions@img,fcn},
	fcn=NetReplacePart[net,"Input"->NetEncoder[{"Image",Round[dim/16]*16,"Grayscale"}]];
	ImageResize[Image@Part[fcn[removeOutlierpixels@img,"Probabilities",TargetDevice->targetDevice],2],dim]];


netevaluateSizeUP[imgIN_,net_,padding_:"Quantile",targetDevice_:"CPU",dsfac_:1/5]:=Module[{dim,img,fcn},
	(*downsample*)
	img=ImageResize[ImageAdjust@imgIN,Scaled[dsfac]];
	img=flipNegated@img;	
	img=ImageCrop[img,NetExtract[NetExtract[net,"Input"],"ImageSize"],Padding->If[padding=="Quantile",Quantile[img,.25],padding]];
	1/dsfac*First@net[removeOutlierpixels@img]]


netevaluateSizeUPfcn[imgIN_,net_,targetDevice_:"CPU"]:=Module[{dim,img,fcn,netdim},
	(*downsample*)
	img=ImageResize[imgIN,Scaled[1/5]];
	img=flipNegated@img;
	dim=ImageDimensions@img;
	netdim=Round[dim/16]*16;
	fcn=NetReplacePart[net,"Input"->NetEncoder[{"Image",netdim,"RGB"}]];	
	
	5*First[dim/netdim]*First@fcn[removeOutlierpixels@ImageResize[img,netdim]]]


flipNegated[img_]:=Module[{m=Median@removeOutlierpixels@img},
	m=If[Length@m>0,Mean@m,m];
	If[m<.5,img,ColorNegate@img]]


netevaluateGradNet[img_,net_,targetDevice_:"CPU"]:=Module[{paddedImg,newdim,dim,fcn,out,gradMap,borderpad,style},
	paddedImg=flipNegated@img;
	borderpad=Max@{16,64-Min[ImageDimensions@img]};
	paddedImg=ImagePad[paddedImg,borderpad,Quantile[paddedImg,.25]];
	dim=ImageDimensions@paddedImg;
	(*paddedImg=If[NetExtract[NetExtract[net,"Input"],"ColorSpace"]=="Grayscale",ImageApply[Max,ImageAdjust@paddedImg],paddedImg];*)
	(*reassebleDeepGradNet Needed so that it runs in the cloud can be removed when cloud is updated*)
	newdim=Round[dim/16]*16;
	fcn=NetReplacePart[(*reassebleDeepGradNet@*)net,{"Input"->NetEncoder[{"Image",newdim,NetExtract[NetExtract[net,"Input"],"ColorSpace"]}],"Binary"->Flatten@{Reverse@newdim,2}}];
	out=fcn[removeOutlierpixels@ImageResize[paddedImg,newdim],TargetDevice->targetDevice];
	style=out["style"];
	out=KeyDrop[out,"style"];
	(**)
	<|"pmap"->ImagePad[ImageResize[Image@Part[out["Binary"],;;,;;,2],dim],-borderpad],Map[ImagePad[ImageResize[#,dim],-borderpad]&,out[[2;;]]],
	"style"->style|>]


setnetevaluate[set_,net_]:=netevaluate[#,net]&/@set;


getpmap[img_,net_,prenet_:Missing[],bestobjectSize_:Missing[],targetDevice_:"CPU"]:=Module[{prePmap,objSz,rescIMG,pmap,fcn,prefcn},
	
	If[MissingQ@prenet,
		netevaluate[img,net,targetDevice],
		prePmap=netevaluate[img,prenet,targetDevice];
		objSz=Median[Last/@ComponentMeasurements[Binarize[prePmap,.5],"AuthalicRadius"]];
		rescIMG=ImageResize[img,Scaled[bestobjectSize/objSz]];
		ImageResize[netevaluate[rescIMG,net,targetDevice],Scaled[objSz/bestobjectSize]]
	]
]


watershedSeparation2DWithPmap[bin_,pmap_,minSz_,maxThreshold_:.1,maxdilation_:1]:=Module[{dt,max},
	dt=pmap;
	max=FillingTransform@Dilation[MaxDetect[bin*dt,maxThreshold],maxdilation,Padding->0];
	{max,DeleteSmallComponents[Binarize[Image@WatershedComponents[ColorNegate@dt,max]*bin,.5],minSz]}
]


watershedSeparation2D[bin_,maxThreshold_:.1,maxdilation_:0]:=Module[{dt,max},
	dt=ImageAdjust@DistanceTransform[bin,Padding->0];
	max=Dilation[MaxDetect[dt,maxThreshold],maxdilation,Padding->0];
	{max,Binarize[Image@WatershedComponents[ImageAdjust@ColorNegate@dt,max]*bin,.5]}
]


watershedSeparation3D[bin_,maxThreshold_:.1]:=Module[{dt,max},
	dt=ImageAdjust@DistanceTransform[bin,Padding->0];
	max=MaxDetect[dt,maxThreshold];
	{max,Binarize[Image3D@WatershedComponents[ImageAdjust@ColorNegate@dt,max]*bin,.5]}
]


(*watershedSeparation3DWithPmap[bin_,pmap_,maxThreshold_:.1,maxdilation_:0]:=Module[{dt,max},
	dt=pmap;
	max=Dilation[MaxDetect[bin*dt,maxThreshold],maxdilation,Padding->0];
	{max,Binarize[Image3D@WatershedComponents[ColorNegate@dt,max]*bin,.5]}
]*)


segmentpmap3D[pmap_,binThreshold_,minimumSize3D_,maxThreshold_]:=Module[{bin,pmaps,bins},
	pmaps=Image3DSlices@pmap;
	(**********************binarise pmap*************************)
	bin=FillingTransform@Binarize[pmap,binThreshold];
	bin=DeleteSmallComponents[bin,minimumSize3D];
	bin=clean3D[cleanSlicesYZ@cleanSlicesXZ@cleanSlicesXY[bin,minimumSize3D^(2/3)],minimumSize3D];
	(**********************2D Watershed binary image with help of pmap**********************)
	bins=Image3DSlices@bin;
	bins=MapThread[Last@watershedSeparation2DWithPmap[#1,#2]&,{bins,pmaps}];
	bins=DeleteSmallComponents[#,minimumSize3D^(2/3)]&/@bins;
	(**********************3D Watershed binary image*************************)
	bin=Last@watershedSeparation3D[Image3D@bins,maxThreshold];
	DeleteSmallComponents[bin,minimumSize3D];]


segmentpmap[img_,pmap_,binThreshold_,minimumSize_,maxThreshold_,debug_:False]:=Module[{bin,maxima,segmented,overlay,labels,overlaylabeled},
	bin=Dilation[Erosion[FillingTransform@MorphologicalBinarize[pmap,{0.1*binThreshold,binThreshold}],1,Padding->0],1,Padding->0];
	bin=DeleteSmallComponents[bin,minimumSize];
	(**********************Watershed binary image*************************)
	{maxima,segmented}=watershedSeparation2DWithPmap[bin,pmap,minimumSize,maxThreshold];
	(**********************generate overlays*************************)
	overlay=Blend@{ImageAdjust@img,ImageCompose[ImageAdjust@img,RemoveBackground[Colorize@MorphologicalComponents@segmented,{Black,.01}]]};
	
	(*get labels and label overlay*)
	labels=ComponentMeasurements[segmented,"Centroid"];
	overlaylabeled=HighlightImage[overlay,Map[ImageMarker[labels[[#,2]]+{0,5},Graphics[{White,Text[Style[ToString@#,FontSize->Scaled@.04]]}]]&,Range[Length@labels]]];
	
	
	Which[
		Total@segmented<500, <|"img"->img,"pmap"->pmap,"maxima"->maxima,
			"color"->Colorize@MorphologicalComponents@segmented,
			"overlay"->overlay,
			"overlaylabeled"->overlaylabeled,
			"objects"->segmented,"error"->True,"messages"->"Hardly any objects found"|>,
		debug,<|"img"->img,"pmap"->pmap,"maxima"->maxima,
			"color"->Colorize@MorphologicalComponents@segmented,
			"overlay"->overlay,
			"overlaylabeled"->overlaylabeled,
			"objects"->segmented,"error"->False,"messages"->"Success!"|>,
		Not@debug,<|
			"img"->img,
			"overlay"->overlay,
			"overlaylabeled"->overlaylabeled,
			"objects"->segmented,"error"->False,"messages"->"Success!"|>
	]
]


(*compileGradientAscentAlgo[gradMap_,mxiter_]:=Module[{getstep},
	getstep[pixels_]:=Extract[gradMap,pixels];
	Compile[{{pixels,_Integer,2}},
		Nest[(#+getstep[#])&,pixels,mxiter](*,
		CompilationTarget->"C"*)]]*)


renumberSA[sa_]:=Module[{tmp=Last/@ComponentMeasurements[sa,"Mask"]},
				Total[tmp*Range@Length@tmp]]



compileGradientAscentAlgo[gradMap_,mxiter_]:=Module[{getstep},
	getstep[pixels_]:=Extract[gradMap,Round@pixels];
	Compile[{{pixels,_Real,2}},
		Nest[(#+getstep[#])&,pixels,mxiter](*,
		CompilationTarget->"C"*)]]


(*upsampleLabelMatrix[lm_,dim_]:=Module[{tmp},
	tmp=Map[Binarize[ImageResize[#,dim,Resampling->"Linear"],.8]&,Image/@Last/@ComponentMeasurements[lm,"Mask"]];
	tmp=Round[tmp*Range@Length@tmp];
	SparseArray@Round@Map[Max,Transpose[ImageData/@tmp,{3,1,2}],{2}]]*)


segmentPmapWithGradients[img_,out_,objectsize_,binThreshold_:0.5,borderObjects_:True,debug_:False,upsample_:True,morphoBinfac_:1]:=Module[{getstep,imgOriginalScale,mxiter,clusters,getcluster,findCenters,bin,lines,endpoints,gradMap,segmented,overlay,pixels,labels,overlaylabeled,dostep,step,return},
	
	bin=ImagePad[MorphologicalBinarize[out["pmap"],{morphoBinfac*binThreshold,binThreshold}],2,Padding->0];
	
	gradMap=Transpose[ImageData/@{ImagePad[out["horzGrad"],2,Padding->0],
								ImagePad[out["vertGrad"],2,Padding->0]},{3,1,2}];
	
	Sow@bin;
	(*get pixels*)
	Sow@First@AbsoluteTiming[
	pixels=First/@Most[ArrayRules@ImageData@bin];];
	If[Length@pixels>0,
	(***************GRADIENT ASCEND ALGORITHM******************)
	Sow@First@AbsoluteTiming[
	findCenters=compileGradientAscentAlgo[gradMap,(2*2*objectsize)];
	endpoints=findCenters@pixels;];
	(******************************************************)
	
	(************Cluster gradient ascent endpoints**************)
	clusters=Map[First,
		Most/@ArrayRules/@Last/@ComponentMeasurements[
			Dilation[Image@SparseArray[Thread[Round@endpoints->1],Reverse@ImageDimensions@bin],1,Padding->0],"Mask"]
			,{2}];
	
	Sow@First@AbsoluteTiming[
	getcluster=Nearest[Thread[Round/@Mean/@clusters->Range@Length@clusters]];
	segmented=SparseArray[Thread[pixels->First/@getcluster/@endpoints],Reverse@ImageDimensions@bin];];
	Sow@segmented;
	(**********************************************************)
	(*Delete Padding*)
	segmented=ArrayPad[segmented,-2];
	
	(*Filter Small components*)
	Sow@First@AbsoluteTiming[segmented=SparseArray@DeleteSmallComponents[Normal@segmented,Round[(objectsize/3)^2]];];
	
	(*remove border objects if needed*)
	If[Not@borderObjects,
		segmented=SparseArray@SelectComponents[segmented,#AdjacentBorderCount==0&]];
	
	If[Total@Total@segmented>0,
		
		Sow@First@AbsoluteTiming[segmented=renumberSA@segmented;];
	
		(*backsample*)
		(*Sow@AbsoluteTiming[segmented=upsampleLabelMatrix[segmented,ImageDimensions@img];];*)
		If[upsample,
			Sow@First@AbsoluteTiming[segmented=SparseArray@Round@ImageData@ImageResize[Image@segmented,ImageDimensions@img,Resampling->"Nearest"];]];
		
		
		Sow@First@AbsoluteTiming[
		overlay=Image[Blend[{ImageAdjust@img,ImageCompose[SetAlphaChannel@ImageAdjust@img,RemoveBackground[Colorize@segmented,{Black,.01}]]},.9],ImageSize->ImageDimensions@img];
		(*overlay=Image[ImageCompose[SetAlphaChannel@ImageAdjust@img,RemoveBackground[Colorize@segmented,{Black,.01}]],ImageSize->ImageDimensions@img];*)
		];
		(*get labels and label overlay*)
		Sow@First@AbsoluteTiming[
		labels=ComponentMeasurements[segmented,"Centroid"];
		overlaylabeled=Image[HighlightImage[overlay,Map[ImageMarker[labels[[#,2]],
			Graphics[{White,Text[Style[ToString@#,FontSize->Scaled@.05]]}]]&,Range[Length@labels]]],ImageSize->ImageDimensions@img];
		];
		Which[
			debug,<|
				out,
				"rawimg"->img,
				"objectsize"->objectsize,
				"flows"->ColorCombine[{Ramp@out["horzGrad"],Ramp[-1*out["horzGrad"]],Ramp@out["vertGrad"],Ramp[-1*out["vertGrad"]]}],
				"overlay"->overlay,
				"overlaylabeled"->overlaylabeled,
				"objects"->segmented,"error"->False,"messages"->"Success!"|>,
			Not@debug,<|
				"rawimg"->img,
				"objectsize"->objectsize,
				"overlay"->overlay,
				"overlaylabeled"->overlaylabeled,
				"styles"->out["style"],
				"objects"->segmented,"error"->False,"messages"->"Success!"|>
		]
		,
		<|"rawimg"->img,
			out,
				"objectsize"->objectsize,
				"overlay"->img,
				"overlaylabeled"->img,
				"objects"->SparseArray[{},Reverse@ImageDimensions@img],"error"->False,"messages"->"Success!"|>]
		,
		<|"rawimg"->img,out,
				"objectsize"->objectsize,
				"overlay"->img,
				"overlaylabeled"->img,
				"objects"->SparseArray[{},Reverse@ImageDimensions@img],"error"->False,"messages"->"Success!"|>]
]


(*segmentPmapWithGradients[img_,out_,objectsize_,binThreshold_:0.5,debug_:False]:=Module[{getstep,imgOriginalScale,mxiter,clusters,getcluster,findCenters,bin,lines,endpoints,gradMap,segmented,overlay,pixels,dim,labels,overlaylabeled,dostep,step,return},
	Sow@AbsoluteTiming[
	bin=Binarize[out["pmap"],binThreshold];
	
	gradMap=Transpose[ImageData/@{ImagePad[ImagePad[out["horzGrad"],-1],1,Padding->0],
								ImagePad[ImagePad[out["vertGrad"],-1],1,Padding->0]},{3,1,2}];];
	(*gradMap round entries*)
	Sow@AbsoluteTiming[
	gradMap=Map[Which[#>0.001,1,#<-0.001,-1,True,0]&,gradMap,{3}];];
	(*gradMap=Map[roundGrad,gradMap,{3}];*)
	(*get pixels*)
	Sow@AbsoluteTiming[
	pixels=First/@Most[ArrayRules@Round@ImageData@bin];];
	(***************GRADIENT ASCEND ALGORITHM******************)
	Sow@AbsoluteTiming[
	findCenters=compileGradientAscentAlgo[gradMap,(2*objectsize)];
	endpoints=findCenters@pixels;];
	(******************************************************)
	(*HighlightImage[bin,Image@SparseArray[Thread[findCenter@#(*NestList[dostep,#,30]*)->1],Reverse@ImageDimensions@bin]&/@RandomSample[pixels,10]]*)
	(*endpoints=Last/@lines;*)
	
	clusters=Map[First,
		Most/@ArrayRules/@Last/@ComponentMeasurements[
			Dilation[Image@SparseArray[Thread[endpoints->1],Reverse@ImageDimensions@bin],1,Padding->0],"Mask"]
			,{2}];
	
	Sow@AbsoluteTiming[
	getcluster=Nearest[Thread[Round/@Mean/@clusters->Range@Length@clusters]];
	segmented=SparseArray[Thread[pixels->First/@getcluster/@endpoints],Reverse@ImageDimensions@bin];];
	
	(*Filter*)
	Sow@AbsoluteTiming[segmented=SparseArray@DeleteSmallComponents[Normal@segmented,Round[(objectsize)^2/50]];];
	
	(*renumber*)
	Sow@AbsoluteTiming[segmented=renumberSA@segmented;];
	
	(*resize to original dim*)
	Sow@AbsoluteTiming[
	segmented=ArrayResample[segmented,Reverse@ImageDimensions@img,Resampling->"Nearest"];
	];
	
	Sow@AbsoluteTiming[
	overlay=Image[Blend[{ImageAdjust@img,ImageCompose[SetAlphaChannel@ImageAdjust@img,RemoveBackground[Colorize@segmented,{Black,.01}]]},.9],ImageSize->ImageDimensions@img];
	(*overlay=Image[ImageCompose[SetAlphaChannel@ImageAdjust@img,RemoveBackground[Colorize@segmented,{Black,.01}]],ImageSize->ImageDimensions@img];*)
	];
	(*get labels and label overlay*)
	Sow@AbsoluteTiming[
	labels=ComponentMeasurements[segmented,"Centroid"];
	overlaylabeled=Image[HighlightImage[overlay,Map[ImageMarker[labels[[#,2]]+{0,5},
		Graphics[{White,Text[Style[ToString@#,FontSize->Scaled@.1]]}]]&,Range[Length@labels]]],ImageSize->ImageDimensions@img];
	];
	Which[
		debug,<|
			out,
			"objectsize"->objectsize,
			"overlay"->overlay,
			"overlaylabeled"->overlaylabeled,
			"objects"->segmented,"error"->False,"messages"->"Success!"|>,
		Not@debug,<|
			"rawimg"->img,
			"objectsize"->objectsize,
			"overlay"->overlay,
			"overlaylabeled"->overlaylabeled,
			"objects"->segmented,"error"->False,"messages"->"Success!"|>
	]
]*)


segment2DImage[img_,net_,minimumSize_,binThreshold_:.5,maxThreshold:.1,debug_:False]:=Module[{pmap,bin,segmented,overlay,labels,overlaylabeled,maxima},
	(**********************evaluate net*************************)
	pmap=getpmap[img,net];
	(**********************segment pmap*************************)
	segmentpmap[img,pmap,binThreshold,minimumSize,maxThreshold,debug]
]




(*segment2DImageGradientMap[img_,net_,netObjectSize_,imgObjectSize_:Missing[],sizenet_:Missing[],binThreshold_:0.5,smooth_:False,debug_:False,borderObjects_:True]:=Module[{out,imgRescaled,estimatedObjSize,bin,originalDim},
	originalDim=ImageDimensions@img;
	Sow@{"sizeestimate",First@AbsoluteTiming[imgRescaled=
		If[MissingQ@imgObjectSize,
			If[Not@MissingQ@sizenet,
				estimatedObjSize=netevaluateSizeUP[img,sizenet,"Periodic"];
				ImageResize[img,Scaled[netObjectSize/estimatedObjSize]]
				,
				img	
			],
			ImageResize[img,Scaled[netObjectSize/imgObjectSize]]
		];]};
	Sow@{"netevaluate",First@AbsoluteTiming[out=netevaluateGradNet[imgRescaled,net];]};
	(*resize to original dimensions to get smooth flows*)
	If[smooth,
		out=Map[ImageResize[#,originalDim,Padding->0]&,out];];
	(*segment*)
	Sow@{"gradientascent",First@AbsoluteTiming[out=<|"estimatedObjectSize"->If[MissingQ@imgObjectSize,estimatedObjSize,imgObjectSize],
		segmentPmapWithGradients[img,out,netObjectSize,binThreshold,borderObjects,debug]|>]};
	out
]
*)


SegmentationButler[assoc_]:=Module[{originalDim,objSize,preferredObjectSize,imgRescaled,out,netsizes,netpixelsize},
	(*check for minimal keys needed*)
	If[KeyExistsQ[assoc,"image"]&&KeyExistsQ[assoc,"nets"],
		(*retain original img dimensions*)
		
		originalDim=ImageDimensions@assoc["image"];
		netsizes=Select[Keys@assoc["nets"],NumberQ];
		Sow@{"sizeEstim",First@AbsoluteTiming[
		objSize=
				If[KeyExistsQ[assoc,"imgObjectSize"]&&Not@MissingQ@assoc["imgObjectSize"],
					assoc["imgObjectSize"],
					If[KeyExistsQ[assoc["nets"],"sizeup"],				
						netevaluateSizeUP[assoc["image"],assoc["nets"]["sizeup"],"Periodic"]
						,
						Missing[]
					]
		];]};
		(*Resize image if pixelsize is known*)
		imgRescaled=If[MissingQ@objSize,
			netpixelsize=Max@netsizes;
			assoc["image"],
			netpixelsize=If[Max@netsizes>objSize,Min@netsizes,Max@netsizes];
			ImageResize[assoc["image"],Scaled[netpixelsize/objSize]]];
		(******Emergency brake if image got rescaled too large*******)
		If[MissingQ@assoc["unhinged"]&&Times@@ImageDimensions@imgRescaled>1200*1200,Missing[],
				
		(**********Eval Net***********)
		Sow@{"neteval",First@AbsoluteTiming[
			out=netevaluateGradNet[imgRescaled,assoc["nets"][netpixelsize],If[KeyExistsQ[assoc,"GPU"]&&assoc["GPU"],"GPU","CPU"]];]};
		
		(*resize to original dimensions to get smooth flows*)
		If[KeyExistsQ[assoc,"smoothOutput"]&&assoc["smoothOutput"],
			out=<|Map[ImageResize[#,originalDim,Padding->0]&,KeyDrop[out,"style"]],"style"->out["style"]|>;
			];
		
		(*segment*)
		Sow@{
			If[KeyExistsQ[assoc,"flows"]&&Not@assoc["flows"],
			(*only binarize pmap if flows off*)
			First@AbsoluteTiming[out=<|"estimatedObjectSize"->objSize,
				out,
				"objects"->SparseArray@MorphologicalComponents@Binarize[out["pmap"],.5],"error"->False,"messages"->"Success!"|>],
			
			First@AbsoluteTiming[out=<|"estimatedObjectSize"->objSize,
			segmentPmapWithGradients[If[KeyExistsQ[assoc,"upsample"]&&Not@assoc["upsample"],imgRescaled,assoc["image"]],
								out,
								If[KeyExistsQ[assoc,"smoothOutput"]&&assoc["smoothOutput"],objSize,netpixelsize],
								If[KeyExistsQ[assoc,"binaryThreshold"],assoc["binaryThreshold"],.5],
								If[KeyExistsQ[assoc,"includeBorderObjects"],assoc["includeBorderObjects"],True],
								If[KeyExistsQ[assoc,"DEBUG"],assoc["DEBUG"],False],
								If[KeyExistsQ[assoc,"upsample"],assoc["upsample"],True],
								If[KeyExistsQ[assoc,"morphoBinFac"],assoc["morphoBinFac"],1]]|>]]};
		out]
		,
		Print@"not enough input given!";
		Abort[];
	]

]


SetAttributes[SegmentationButler,{Protected,ReadProtected,Locked}]


(* ::Section:: *)
(*3 D*)


netevaluateGradNet3D[img3Din_,net_,rescale_,td_]:=Module[{xy,xz,yz,i,n,img3D,out},
	img3D=ImageResize[img3Din,Scaled@rescale];
	(*apply net along each dimension and transform back to original dims*)
	Print@ImageDimensions@img3D;
	n=Total@ImageDimensions@img3D;
	i=0;
	Print@"xz slice analysis...";
	xz=Map[ImageReflect[ImageRotate[ImagePad[Image3D[#],1,Padding->0],{Pi/2,{1,0,0}}],Front]&,
			Transpose@Map[(i++;PrintTemporary[ToString[Round[i/n*100,.01]]<>"%"];
			Values[netevaluateGradNet[#,net,td]])&,Image3DSlices[img3D,All,2]]];
	Print@"yz slice analysis...";
	yz=Map[ImageRotate[ImageRotate[ImagePad[Image3D[#],1,Padding->0],{Pi/2,{1,0,0}}],{-Pi/2,{0,0,1}}]&,
		Transpose@Map[(i++;PrintTemporary[ToString[Round[i/n*100,.01]]<>"%"];
		Values[netevaluateGradNet[#,net,td]])&,Image3DSlices[img3D,All,3]]];
	Print@"xy slice analysis...";
	xy=Map[ImagePad[Image3D[#],1,Padding->0]&,
		Transpose@Map[(i++;PrintTemporary[ToString[Round[i/n*100,.01]]<>"%"];
		Values[netevaluateGradNet[#,net,td]])&,Image3DSlices[img3D,All]]];
	
	Print@"Transposing...";
	<|"img"->img3D,
		"pmap"->(xy[[1]]+xz[[1]]+yz[[1]])/3,
		(*"gradMap"->Image3D[(Transpose[{Normal@SparseArray[{},Reverse@ImageDimensions@xy[[3]]],ImageData@xy[[2]],ImageData@xy[[3]]},{4,1,2,3}]+
							Transpose[{ImageData@xz[[2]],Normal@SparseArray[{},Reverse@ImageDimensions@xz[[3]]],ImageData@xz[[3]]},{4,1,2,3}]+
							Transpose[{ImageData@yz[[2]],ImageData@yz[[3]],Normal@SparseArray[{},Reverse@ImageDimensions@yz[[3]]]},{4,1,2,3}])/2]*)
	 "gradMap"->Image3D[(Transpose[{Normal@SparseArray[{},Reverse@ImageDimensions@xy[[3]]],ImageData@xy[[2]],ImageData@xy[[3]]}+
							{ImageData@xz[[2]],Normal@SparseArray[{},Reverse@ImageDimensions@xz[[3]]],ImageData@xz[[3]]}+
							{ImageData@yz[[2]],ImageData@yz[[3]],Normal@SparseArray[{},Reverse@ImageDimensions@yz[[3]]]},{4,1,2,3}])/2]|>
	]


(*compileGradientAscentAlgo3D[gradMap_,mxiter_]:=Module[{getstep},
	getstep[pixels_]:=Flatten@{Extract[gradMap[[1]],Round@pixels],Extract[gradMap[[2]],Round@pixels],Extract[gradMap[[3]],Round@pixels]};
	Compile[{{pixels,_Real,2}},
		Nest[(#+getstep[#])&,pixels,mxiter](*,
		CompilationTarget->"C"*)]]*)


segmentPmapWithGradients3D[img3D_,out_,objectsize_,binthreshold_:.25]:=Module[{bin,pixels,endpoints,clusters,gradMap,getcluster,segmented,findCenters},
	bin=Binarize[out["pmap"],binthreshold];
	
	pixels=First/@Most[ArrayRules@Round@ImageData@bin];
	
	(*compile grad ascent algorithm*)
	findCenters=compileGradientAscentAlgo[ImageData@out["gradMap"],(5*2*3*objectsize)];
	endpoints=findCenters@pixels;
	
	clusters=Map[First,
		Most/@ArrayRules/@Last/@ComponentMeasurements[
			Image3D@SparseArray[Thread[Round@endpoints->1],Reverse@ImageDimensions@bin],"Mask"]
			,{2}];

	getcluster=Nearest[Thread[Round/@Mean/@clusters->Range@Length@clusters]];
	segmented=SparseArray[Thread[pixels->First/@getcluster/@endpoints],Reverse@ImageDimensions@bin];
	
	(*segmented=SparseArray@Round@ImageData@ImageFilter[If[#[[2,2,2]]!=0,Max@#,0]&,Image3D@segmented,1];*)
	segmented=SparseArray@DeleteSmallComponents[Normal@segmented,Round[(objectsize/3)^3]];
	
	(*Delete Padding*)
	segmented=ArrayPad[segmented,-1];
	
	Sow@AbsoluteTiming[segmented=renumberSA@segmented;];
	<|out,"segmented"->segmented|>
]


segment3DImageGradientMap[img3D_,net_,netobjectsize_,imgobjectsize_,threshold_:.25,td_:"GPU"]:=
	segmentPmapWithGradients3D[img3D,
		netevaluateGradNet3D[img3D,net,netobjectsize/imgobjectsize,td],
		netobjectsize,threshold]



SegButler3D[assoc_]:=Module[{},
If[Not@KeyExistsQ[assoc,"threshold"],
Print@"please provide threshold!";Abort[]];
If[Not@KeyExistsQ[assoc,"targetDevice"],
Print@"please provide targetDevice!";Abort[]];
If[Not@KeyExistsQ[assoc,"file"]&&Not@KeyExistsQ[assoc,"image"],
Print@"please provide file or image!";Abort[]];
If[KeyExistsQ[assoc,"file"],
	If[Not@StringQ@assoc["file"],Print@"please provide file name as string!";Abort[]]];
If[Not@KeyExistsQ[assoc,"objSz"],
Print@"please provide objSz!";Abort[]];
If[Not@KeyExistsQ[assoc,"modelSz"],
Print@"please provide modelSz!";Abort[]];
If[Not@KeyExistsQ[assoc,"model"],
Print@"please provide model!";Abort[]];

(*get rescaling factor*)
zFac=Which[
	Not@KeyExistsQ[assoc,"zFac"]&&KeyExistsQ[assoc,"file"],
		Print@"trying to get zScaling from tif metadata...";
		metadata=Import[assoc["file"],"Exif"];
		If[Not@MissingQ@metadata["XResolution"],
			zspacing=ToExpression@StringTake[metadata["ImageDescription"],{Last@Last@StringPosition[metadata["ImageDescription"],"spacing"]+2,Last@Last@StringPosition[metadata["ImageDescription"],"spacing"]+7}];
			xspacing=N[1/metadata["XResolution"]];
			zspacing/xspacing,	
			Print@"Failed Getting scale from metadata!";Abort[]
		],
	Not@KeyExistsQ[assoc,"zFac"],
		Print@"zFrac key needed";Abort[],
	True,
	assoc["zFac"]];
(*resize image so that xy z same scale*)
Print@"Rescaling 3D image...";
img3D=Which[
	KeyExistsQ[assoc,"image"]&&Not@KeyExistsQ[assoc,"file"],
	ImageResize[Image3D[removeOutlierpixels@assoc["image"],"Byte"],Scaled/@{1,1,zFac}],
	Not@KeyExistsQ[assoc,"image"]&&KeyExistsQ[assoc,"file"],
	ImageResize[Image3D[removeOutlierpixels@Image3D@Import@file,"Byte"],Scaled/@{1,1,zFac}]];
Print@"starting segmentation...";
segment3DImageGradientMap[img3D,assoc["model"],assoc["modelSz"],assoc["objSz"],assoc["threshold"],assoc["targetDevice"]]
]


End[]


EndPackage[]
