(* ::Package:: *)

BeginPackage["BuildNeuralNetworks`"]


UNETdswResGradientOutStyle::usage="UNETdswResGradientOut[n_] using depth separable convolutions and residual blocks and 3 outputs"


Begin["`Private`"]


dir=$InputFileName


leayReLU[alpha_]:=ElementwiseLayer[Ramp[#]-alpha*Ramp[-#]&]
basicBlock[channels_,kernelSize_,opts___]:=NetChain[{ConvolutionLayer[channels,kernelSize,"PaddingSize"->(kernelSize-1)/2(*,"Biases"\[Rule]None*),opts],BatchNormalizationLayer[],leayReLU[0.1]}];
convBlock[channels_,kernelSize_,opts___]:=NetChain[{BatchNormalizationLayer[],leayReLU[0.1],ConvolutionLayer[channels,kernelSize,"PaddingSize"->(kernelSize-1)/2(*,"Biases"\[Rule]None*),opts]}];

(*Depth separated convolutional layer*)
DSConvolutionLayer[nin_,nout_,kernelSz_,stride_:1]:=NetChain@{ConvolutionLayer[nin,kernelSz,"Stride"->stride,"ChannelGroups"->nin,"PaddingSize"->1],ConvolutionLayer[nout,1]}
DSDeconvolutionLayer[n_,k_,stride_:1]:=NetChain@{DeconvolutionLayer[n,k,"Stride"->stride,"GroupNumber"->1],ConvolutionLayer[n,1]}
(*Depth separated convolutional block*)
dsconvBlock[channelsin_,channelsout_,kernelSize_,stride_:1]:=NetChain[{DSConvolutionLayer[channelsin,channelsout,kernelSize,stride],BatchNormalizationLayer[],leayReLU[0.1]}];



(* ::Section:: *)
(*UNETs*)


resBlock2D[nin_,nout_,k_]:=NetGraph[<|
"res"->{dsconvBlock[nin,nout,k],dsconvBlock[nout,nout,k]},
"id"->ConvolutionLayer[nout,{1,1}],
"add"->TotalLayer[]|>,
{{"res","id"}->"add"}
]


resBlockDown[n_,k_]:=NetGraph[<|
"res"->{convBlock[n,3],convBlock[n,3]},
"id"->ConvolutionLayer[n,{1,1}],
"add"->TotalLayer[]|>,
{{"res","id"}->"add"}
]


convStyleBlock[n_,k_]:=NetGraph[<|
	"full"->ConvolutionLayer[n,1],
	"add"->TotalLayer[],
	"conv"->convBlock[n,k]|>
,{NetPort["style"]->"full",{"full",NetPort["in"]}->"add"->"conv"}
]


resBlockUp[n_,k_]:=NetGraph[<|
"res1"->convBlock[n,3],
"res2"->convStyleBlock[n,3],
"res3"->convStyleBlock[n,3],
"res4"->convStyleBlock[n,3],
"id"->ConvolutionLayer[n,{1,1}],
"add1"->TotalLayer[],
"add2"->TotalLayer[]|>,
{NetPort["in"]->{"res1","id"},NetPort["style"]->{NetPort["res2","style"],NetPort["res3","style"],NetPort["res4","style"]},
"res1"->NetPort["res2","in"],{"id","res2"}->"add1"->NetPort["res3","in"],"res3"->NetPort["res4","in"],{"res4","add1"}->"add2"}
]


UNETStyleGradOut[n_]:=Module[{},
NetGraph[<|
	"conv1"->{resBlockDown[n,3],resBlockDown[n,3],DropoutLayer[.1]},
	"maxpool1"->{PoolingLayer[{2,2},2]},
	"conv2"->{resBlockDown[2n,3],resBlockDown[2n,3],DropoutLayer[.1]},
	"maxpool2"->{PoolingLayer[2,2]},
	"conv3"->{resBlockDown[4n,3],resBlockDown[4n,3],DropoutLayer[.1]},
	"maxpool3"->{PoolingLayer[2,2]},
	"conv4"->{resBlockDown[8n,3],resBlockDown[8n,3],DropoutLayer[.1]},
	"up1"->DeconvolutionLayer[4n,2,"Stride"->2],
	"style1"->ResizeLayer@{Scaled@2,Scaled@2},
	"cat1"->CatenateLayer[],
	"uconv1"->resBlockUp[4n,3],
	"style2"->ResizeLayer@{Scaled@4,Scaled@4},
	"up2"->DeconvolutionLayer[2n,2,"Stride"->2],
	"cat2"->CatenateLayer[],
	"uconv2"->resBlockUp[2n,3],
	"style3"->ResizeLayer@{Scaled@8,Scaled@8},
	"up3"->DeconvolutionLayer[n,2,"Stride"->2],
	"cat3"->CatenateLayer[],
	"uconv3"->resBlockUp[n,3],
	"classBinary"->{ConvolutionLayer[2,1],TransposeLayer[{1<->2,2<->3}],SoftmaxLayer[-1]},
	"calcHorzGrad"->{ConvolutionLayer[1,1],Tanh},
	"calcVertGrad"->{ConvolutionLayer[1,1],Tanh}|>,{
	"conv1"->"maxpool1"->"conv2"->"maxpool2"->"conv3"->"maxpool3"->"conv4"->"up1",
	"conv4"->{NetPort["style"],"style1","style2","style3"},
	"style1"->NetPort["uconv1","style"],
	"style2"->NetPort["uconv2","style"],
	"style3"->NetPort["uconv3","style"],
	{"up1","conv3"}->"cat1"->NetPort["uconv1","in"],"uconv1"->"up2",
	{"up2","conv2"}->"cat2"->"uconv2"->"up3",
	{"up3","conv1"}->"cat3"->"uconv3",
	"uconv3"->{"classBinary","calcHorzGrad","calcVertGrad"},
	"classBinary"->NetPort["Binary"],
	"calcHorzGrad"->NetPort["horzGrad"],
	"calcVertGrad"->NetPort["vertGrad"]
	}]]


End[]
EndPackage[]

