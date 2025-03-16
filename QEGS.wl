(* ::Package:: *)

Print["Mathematica package dedicated to investigation of Nash equilibria existence in quantum extended games based on EWL scheme. \nThe package offers various functions to support a user to examine these extensions in more detail."];
Print["Package based on the mathematical concept and formulas covered in the following publications: \n \!\(\*TemplateBox[{RowBox[{RowBox[{\"Permissible\", \" \", \"extensions\", \" \", \"of\", \" \", \"classical\", \" \", \"to\", \" \", \"quantum\", \" \", \"games\", \" \", \"combining\"}], RowBox[{\"three\", \" \", \"strategies\"}]}], {URL[\"https://arxiv.org/pdf/2404.06196\"], None}, \"https://arxiv.org/pdf/2404.06196\", \"HyperlinkActionRecycled\", {\"HyperlinkActive\"}, BaseStyle -> {\"Hyperlink\"}, HyperlinkAction -> \"Recycled\"},\n\"HyperlinkTemplate\"]\) and \n \!\(\*TemplateBox[{RowBox[{RowBox[{\"Permissible\", \" \", \"four\"}], \"-\", RowBox[{\"strategy\", \" \", \"quantum\", \" \", \"extensions\", \" \", \"of\", \" \", \"classical\", \" \", \"games\"}]}], {URL[\"https://arxiv.org/pdf/2405.07380\"], None}, \"https://arxiv.org/pdf/2405.07380\", \"HyperlinkActionRecycled\", {\"HyperlinkActive\"}, BaseStyle -> {\"Hyperlink\"}, HyperlinkAction -> \"Recycled\"},\n\"HyperlinkTemplate\"]\)"]
Print["Name: Quantum Extension Game Package"];
Print["by Krzysztof Grzanka, Piotr Fr\:0105ckiewicz, Anna Gorczyca-Goraj, Marek Szopa"];
Print["Please send bug reports, suggestions, questions, etc. to Krzysztof Grzanka, krzysztof.grzanka@uekat.pl"]


BeginPackage["QEGS`"];

(* Gamma matrices *)
GCreate::usage  = "GCreate[matrix,quiet] - Create a classical game payoff matrix \[CapitalGamma] given by a bimatrix in a form of 
matrix={{{p1,p2},{p3,p4}},{{p5,p6},{p7,p8}}}. Argument \[CloseCurlyQuote]quiet\[CloseCurlyQuote] by default is False and prints output matrix in a traditional form";
G1Create::usage = "G1Create[matrix,quiet] - Create payoff bimatrix \!\(\*SubscriptBox[\(\[CapitalGamma]\), \(1\)]\) by swapping rows of the basic game of the
matrix={{{p1,p2},{p3,p4}},{{p5,p6},{p7,p8}}} form. Argument \[CloseCurlyQuote]quiet\[CloseCurlyQuote] by default is False and prints output matrix in a traditional form";
G2Create::usage = "G2Create[matrix,quiet] - Create payoff bimatrix \!\(\*SubscriptBox[\(\[CapitalGamma]\), \(2\)]\) by swapping columns of the basic game of the
matrix={{{p1,p2},{p3,p4}},{{p5,p6},{p7,p8}}} form. Argument \[CloseCurlyQuote]quiet\[CloseCurlyQuote] by default is False and prints output matrix in a traditional form";
G3Create::usage = "G3Create[matrix,quiet] - Create payoff bimatrix \!\(\*SubscriptBox[\(\[CapitalGamma]\), \(3\)]\) by swapping rows and columns of the basic game of the
matrix={{{p1,p2},{p3,p4}},{{p5,p6},{p7,p8}}} form. Argument \[CloseCurlyQuote]quiet\[CloseCurlyQuote] by default is False and prints output matrix in a traditional form";

(* 3x3 Extensions *)
A0ext::usage = "A0ext[matrix] - Create \!\(\*SubscriptBox[\(A\), \(0\)]\) class three-strategy quantum extension for a game given by bimatrix in a form of 
matrix={{{p1,p2},{p3,p4}},{{p5,p6},{p7,p8}}}. It is described by the formula (78) from \n \!\(\*TemplateBox[{RowBox[{RowBox[{\"Permissible\", \" \", \"extensions\", \" \", \"of\", \" \", \"classical\", \" \", \"to\", \" \", \"quantum\", \" \", \"games\", \" \", \"combining\"}],  RowBox[{\"three\", \" \", \"strategies\"}]}], {URL[\"https://arxiv.org/pdf/2404.06196\"], None}, \"https://arxiv.org/pdf/2404.06196\", \"HyperlinkActionRecycled\", {\"HyperlinkActive\"}, BaseStyle -> {\"Hyperlink\"}, HyperlinkAction -> \"Recycled\"},\n\"HyperlinkTemplate\"]\).";
B0ext::usage = "B0ext[matrix] - Create \!\(\*SubscriptBox[\(B\), \(0\)]\) class three-strategy quantum extension for a game given by bimatrix in a form of 
matrix={{{p1,p2},{p3,p4}},{{p5,p6},{p7,p8}}}. It is described by the formula (79) from \n \!\(\*TemplateBox[{RowBox[{RowBox[{\"Permissible\", \" \", \"extensions\", \" \", \"of\", \" \", \"classical\", \" \", \"to\", \" \", \"quantum\", \" \", \"games\", \" \", \"combining\"}],  RowBox[{\"three\", \" \", \"strategies\"}]}], {URL[\"https://arxiv.org/pdf/2404.06196\"], None}, \"https://arxiv.org/pdf/2404.06196\", \"HyperlinkActionRecycled\", {\"HyperlinkActive\"}, BaseStyle -> {\"Hyperlink\"}, HyperlinkAction -> \"Recycled\"},\n\"HyperlinkTemplate\"]\).";
C0ext::usage = "C0ext[matrix] - Create \!\(\*SubscriptBox[\(C\), \(0\)]\) class three-strategy quantum extension for a game given by bimatrix in a form of 
matrix={{{p1,p2},{p3,p4}},{{p5,p6},{p7,p8}}}. It is described by the formula (80) from \n \!\(\*TemplateBox[{RowBox[{RowBox[{\"Permissible\", \" \", \"extensions\", \" \", \"of\", \" \", \"classical\", \" \", \"to\", \" \", \"quantum\", \" \", \"games\", \" \", \"combining\"}],  RowBox[{\"three\", \" \", \"strategies\"}]}], {URL[\"https://arxiv.org/pdf/2404.06196\"], None}, \"https://arxiv.org/pdf/2404.06196\", \"HyperlinkActionRecycled\", {\"HyperlinkActive\"}, BaseStyle -> {\"Hyperlink\"}, HyperlinkAction -> \"Recycled\"},\n\"HyperlinkTemplate\"]\).";

(* 4x4 Extensions *)
A1ext::usage = "A1ext[matrix] - Create \!\(\*SubscriptBox[\(A\), \(1\)]\) class four-strategy quantum extension for bimatrix game in a form of
matrix={{{p1,p2},{p3,p4}},{{p5,p6},{p7,p8}}}. It is described by the formula (74) from \n \!\(\*TemplateBox[{RowBox[{RowBox[{\"Permissible\", \" \", \"four\"}], \"-\", RowBox[{\"strategy\", \" \", \"quantum\", \" \", \"extensions\", \" \", \"of\", \" \", \"classical\", \" \", \"games\"}]}], {URL[\"https://arxiv.org/pdf/2405.07380\"], None}, \"https://arxiv.org/pdf/2405.07380\", \"HyperlinkActionRecycled\", {\"HyperlinkActive\"}, BaseStyle -> {\"Hyperlink\"}, HyperlinkAction -> \"Recycled\"},\n\"HyperlinkTemplate\"]\)";
A2ext::usage = "A2ext[matrix] - Create \!\(\*SubscriptBox[\(A\), \(2\)]\) class four-strategy quantum extension for bimatrix game in a form of
matrix={{{p1,p2},{p3,p4}},{{p5,p6},{p7,p8}}}. It is described by the formula (74) from \n \!\(\*TemplateBox[{RowBox[{RowBox[{\"Permissible\", \" \", \"four\"}], \"-\", RowBox[{\"strategy\", \" \", \"quantum\", \" \", \"extensions\", \" \", \"of\", \" \", \"classical\", \" \", \"games\"}]}], {URL[\"https://arxiv.org/pdf/2405.07380\"], None}, \"https://arxiv.org/pdf/2405.07380\", \"HyperlinkActionRecycled\", {\"HyperlinkActive\"}, BaseStyle -> {\"Hyperlink\"}, HyperlinkAction -> \"Recycled\"},\n\"HyperlinkTemplate\"]\)";
B1ext::usage = "B1ext[matrix] - Create \!\(\*SubscriptBox[\(B\), \(1\)]\) class four-strategy quantum extension for bimatrix game in a form of
matrix={{{p1,p2},{p3,p4}},{{p5,p6},{p7,p8}}}. It is described by the formula (80) from \n \!\(\*TemplateBox[{RowBox[{RowBox[{\"Permissible\", \" \", \"four\"}], \"-\", RowBox[{\"strategy\", \" \", \"quantum\", \" \", \"extensions\", \" \", \"of\", \" \", \"classical\", \" \", \"games\"}]}], {URL[\"https://arxiv.org/pdf/2405.07380\"], None}, \"https://arxiv.org/pdf/2405.07380\", \"HyperlinkActionRecycled\", {\"HyperlinkActive\"}, BaseStyle -> {\"Hyperlink\"}, HyperlinkAction -> \"Recycled\"},\n\"HyperlinkTemplate\"]\)";
C1ext::usage = "C1ext[matrix] - Create \!\(\*SubscriptBox[\(C\), \(1\)]\) class four-strategy quantum extension for bimatrix game in a form of
matrix={{{p1,p2},{p3,p4}},{{p5,p6},{p7,p8}}}. It is described by the formula (83) from \n \!\(\*TemplateBox[{RowBox[{RowBox[{\"Permissible\", \" \", \"four\"}], \"-\", RowBox[{\"strategy\", \" \", \"quantum\", \" \", \"extensions\", \" \", \"of\", \" \", \"classical\", \" \", \"games\"}]}], {URL[\"https://arxiv.org/pdf/2405.07380\"], None}, \"https://arxiv.org/pdf/2405.07380\", \"HyperlinkActionRecycled\", {\"HyperlinkActive\"}, BaseStyle -> {\"Hyperlink\"}, HyperlinkAction -> \"Recycled\"},\n\"HyperlinkTemplate\"]\)";
D1ext::usage = "D1ext[matrix] - Create \!\(\*SubscriptBox[\(D\), \(1\)]\) class four-strategy quantum extension for bimatrix game in a form of
matrix={{{p1,p2},{p3,p4}},{{p5,p6},{p7,p8}}}. It is described by the formula (87) from \n \!\(\*TemplateBox[{RowBox[{RowBox[{\"Permissible\", \" \", \"four\"}], \"-\", RowBox[{\"strategy\", \" \", \"quantum\", \" \", \"extensions\", \" \", \"of\", \" \", \"classical\", \" \", \"games\"}]}], {URL[\"https://arxiv.org/pdf/2405.07380\"], None}, \"https://arxiv.org/pdf/2405.07380\", \"HyperlinkActionRecycled\", {\"HyperlinkActive\"}, BaseStyle -> {\"Hyperlink\"}, HyperlinkAction -> \"Recycled\"},\n\"HyperlinkTemplate\"]\)";
D2ext::usage = "D2ext[matrix] - Create \!\(\*SubscriptBox[\(D\), \(2\)]\) class four-strategy quantum extension for bimatrix game in a form of
matrix={{{p1,p2},{p3,p4}},{{p5,p6},{p7,p8}}}. It is described by the formula (87) from \n \!\(\*TemplateBox[{RowBox[{RowBox[{\"Permissible\", \" \", \"four\"}], \"-\", RowBox[{\"strategy\", \" \", \"quantum\", \" \", \"extensions\", \" \", \"of\", \" \", \"classical\", \" \", \"games\"}]}], {URL[\"https://arxiv.org/pdf/2405.07380\"], None}, \"https://arxiv.org/pdf/2405.07380\", \"HyperlinkActionRecycled\", {\"HyperlinkActive\"}, BaseStyle -> {\"Hyperlink\"}, HyperlinkAction -> \"Recycled\"},\n\"HyperlinkTemplate\"]\)";
E1ext::usage = "E1ext[matrix] - Create \!\(\*SubscriptBox[\(E\), \(1\)]\) class four-strategy quantum extension for bimatrix game in a form of
matrix={{{p1,p2},{p3,p4}},{{p5,p6},{p7,p8}}}. It is described by the formula (90) from \n \!\(\*TemplateBox[{RowBox[{RowBox[{\"Permissible\", \" \", \"four\"}], \"-\", RowBox[{\"strategy\", \" \", \"quantum\", \" \", \"extensions\", \" \", \"of\", \" \", \"classical\", \" \", \"games\"}]}], {URL[\"https://arxiv.org/pdf/2405.07380\"], None}, \"https://arxiv.org/pdf/2405.07380\", \"HyperlinkActionRecycled\", {\"HyperlinkActive\"}, BaseStyle -> {\"Hyperlink\"}, HyperlinkAction -> \"Recycled\"},\n\"HyperlinkTemplate\"]\)";
E2ext::usage = "E2ext[matrix] - Create \!\(\*SubscriptBox[\(E\), \(2\)]\) class four-strategy quantum extension for bimatrix game in a form of
matrix={{{p1,p2},{p3,p4}},{{p5,p6},{p7,p8}}}. It is described by the formula (90) from \n \!\(\*TemplateBox[{RowBox[{RowBox[{\"Permissible\", \" \", \"four\"}], \"-\", RowBox[{\"strategy\", \" \", \"quantum\", \" \", \"extensions\", \" \", \"of\", \" \", \"classical\", \" \", \"games\"}]}], {URL[\"https://arxiv.org/pdf/2405.07380\"], None}, \"https://arxiv.org/pdf/2405.07380\", \"HyperlinkActionRecycled\", {\"HyperlinkActive\"}, BaseStyle -> {\"Hyperlink\"}, HyperlinkAction -> \"Recycled\"},\n\"HyperlinkTemplate\"]\)";

(* Nash Equilibria *)
FindPureNE::usage = "FindPureNE[Nmatrix] - Highlight Nash equilibria in a numerical bimatrix.";

RangeFindPureNE::usage = "RangeFindPureNE[matrix,{x,xmin,xmax}] - Generate the bimatrix with highlighted Nash equilibria of a given bimatrix,
where payoffs can be expressed in terms of a continuous parameter x in range from xmin to xmax. The parameter value can be controlled 
with a dynamic slider.";

(* Maximin *)
Maximin::usage = "Maximin[matrix] Highlight both players' maximin strategies in the input bimatrix";
RangeMaximin::usage = "RangeMaximin[matrix,{x,xmin,xmax}] - Generate the bimatrix with highlighted maximin strategy for both players in the input bimatrix,
where payoffs can be expressed in terms of a continuous parameter x in range from xmin to xmax. The parameter value can be controlled with a dynamic slider.";


(* Dominated strategies *)
DominatedStrategies::usage = "DominatedStrategies[matrix] - Highlight both players' dominated strategies in the input bimatrix.";
RangeDomStrat::usage = "RangeDomStrat[matrix,list] - Generate the bimatrix with highlighted dominated strategies for both players in the input bimatrix,
where payoffs can be expressed in terms of a continuous parameter x in range from xmin to xmax. The parameter value can be controlled with a dynamic slider.";

(* Report generation *)
GenerateReport::usage = "GenerateReport[matrix] - Make a PDF report summarizing features and (if possible) extensions of the input game bimatrix"

(* Warnings and errors *)
Quiet::GCreate="Argument `1` should be True/False. The matrix \[CapitalGamma] is created anyway";
Quiet::G1Create="Argument `1` should be True/False. The matrix \!\(\*SubscriptBox[\(\[CapitalGamma]\), \(1\)]\) is created anyway";
Quiet::G2Create="Argument `1` should be True/False. The matrix \!\(\*SubscriptBox[\(\[CapitalGamma]\), \(2\)]\) is created anyway";
Quiet::G3Create="Argument `1` should be True/False. The matrix \!\(\*SubscriptBox[\(\[CapitalGamma]\), \(3\)]\) is created anyway";

Size::Ext = "Cannot create quantum game extensions for bimatrix of different size than 2x2.";
Size::GenerateReport = "Cannot make quantum game extension of the bimatrix game of different size than 2x2.
Report with Nash equilibria, maximin and dominated strategies was saved to the file";

Input::FindPureNE = "The input `1` should be a numerical bimatrix";
Input::Maximin = "The input `1` should be a numerical bimatrix";
Input::DominatedStrategies = "The input `1` should be a numerical bimatrix";
Input::GenerateReport = "Cannot find Nash Equilibria, maximin and dominated strategies for a nonnumerical bimatrix. 
Report with quantum extensions of the input game was saved to the file";

Error::GenerateReport = "Cannot make quantum game extension nor find Nash equilibria, maximin and dominated strategies 
for a nonnumerical payoff bimatrix of the size different than 2x2. No report was created";


(* Gamma matrices *)
GCreate[matrix_,quiet_:False] := Module[{list,l1,l2,l3,l4,l5,l6,l7,l8},
	list=Flatten[matrix];
	l1=list[[1]];
	l2=list[[2]];
	l3=list[[3]];
	l4=list[[4]];
	l5=list[[5]];
	l6=list[[6]];
	l7=list[[7]];
	l8=list[[8]];
	G={{{l1,l2},{l3,l4}},{{l5,l6},{l7,l8}}};
	If[quiet==True,{},Print["\[CapitalGamma]=", G//TraditionalForm],Message[Quiet::GCreate,quiet]];
	Return[G]];
	
G1Create[matrix_,quiet_:False] := Module[{G},
	G=GCreate[matrix,True];
	G1={G[[2]],G[[1]]};
	If[quiet==True,{},Print["\!\(\*SubscriptBox[\(\[CapitalGamma]\), \(1\)]\)=", G1//TraditionalForm],Message[Quiet::G1Create,quiet]];
	Return[G1]];
	
G2Create[matrix_,quiet_:False] := Module[{G},
	G=GCreate[matrix,True];
	G2={};
	G2=G2[[All,{1,2}]]=G[[All,{2,1}]];
	If[quiet==True,{},Print["\!\(\*SubscriptBox[\(\[CapitalGamma]\), \(2\)]\)=", G2//TraditionalForm],Message[Quiet::G2Create,quiet]];
	Return[G2]];
	
G3Create[matrix_,quiet_:False] := Module[{G,G2},
	G=GCreate[matrix,True];
	G2=G2Create[matrix,True];
	G3={G2[[2]],G2[[1]]};
	If[quiet==True,{},Print["\!\(\*SubscriptBox[\(\[CapitalGamma]\), \(3\)]\)=", G3//TraditionalForm],Message[Quiet::G3Create,quiet]];
	Return[G3];];
	


(* 3x3 Extensions *)
A0ext[matrix_] := Module[{G,col,row},
If[Length[Flatten[matrix]]!=8,Message[Size::Ext],
G=GCreate[matrix,True];
col={(G[[1,1]]+G[[1,2]])/2,(G[[2,1]]+G[[2,2]])/2};
row={(G[[1,1]]+G[[2,1]])/2,(G[[1,2]]+G[[2,2]])/2,(G[[1,1]]+G[[1,2]]+G[[2,1]]+G[[2,2]])/4};

A0=Join[Join[G,List/@col,2],{row}];
Print["\!\(\*SubscriptBox[\(A\), \(0\)]\)=",A0//TraditionalForm];
Return[A0];]
];


B0ext[matrix_] := Module[{G,col,row},
If[Length[Flatten[matrix]]!=8,Message[Size::Ext],
G=GCreate[matrix,True];
col={(G[[2,1]]+G[[2,2]])/2,(G[[1,1]]+G[[1,2]])/2};
row={(G[[1,2]]+G[[2,2]])/2,(G[[1,1]]+G[[2,1]])/2,(G[[1,1]]+G[[1,2]]+G[[2,1]]+G[[2,2]])/4};

B0=Join[Join[G,List/@col,2],{row}];
Print["\!\(\*SubscriptBox[\(B\), \(0\)]\)=",B0//TraditionalForm];
Return[B0];]
];


C0ext[matrix_] := Module[{G,col,row},
If[Length[Flatten[matrix]]!=8,Message[Size::Ext],
G=GCreate[matrix,True];
col={(G[[1,1]]+G[[1,2]]+G[[2,1]]+G[[2,2]])/4,(G[[1,1]]+G[[1,2]]+G[[2,1]]+G[[2,2]])/4};
row={(G[[1,1]]+G[[1,2]]+G[[2,1]]+G[[2,2]])/4,(G[[1,1]]+G[[1,2]]+G[[2,1]]+G[[2,2]])/4,(G[[1,1]]+G[[1,2]]+G[[2,1]]+G[[2,2]])/4};

C0=Join[Join[G,List/@col,2],{row}];
Print["\!\(\*SubscriptBox[\(C\), \(0\)]\)=",C0//TraditionalForm];
Return[C0];]
];


(*4x4 Extensions*) 
A1ext[matrix_] := Module[{G,G1,G2,G3},
If[Length[Flatten[matrix]]!=8,Message[Size::Ext],
G=GCreate[matrix,True];
G1=G1Create[matrix,True];
G2=G2Create[matrix,True];
G3=G3Create[matrix,True];

A1={{G,a1*G+(1-a1)*G3},{a1*G+(1-a1)*G3,(1-2a1)^2*G+4a1(1-a1)*G3}}//ArrayFlatten;
Print["\!\(\*SubscriptBox[\(A\), \(1\)]\)=",A1//TraditionalForm];
Return[A1];
];]

A2ext[matrix_] := Module[{G,G1,G2,G3},
If[Length[Flatten[matrix]]!=8,Message[Size::Ext],
G=GCreate[matrix,True];
G1=G1Create[matrix,True];
G2=G2Create[matrix,True];
G3=G3Create[matrix,True];

A2={{G,a2*G2+(1-a2)*G1},{a2*G1+(1-a2)*G2,(1-2a2)^2*G3+4a2(1-a2)*G}}//ArrayFlatten;
Print["\!\(\*SubscriptBox[\(A\), \(2\)]\)=",A2//TraditionalForm];
Return[A2];
];]

B1ext[matrix_] := Module[{G,G1,G2,G3},
If[Length[Flatten[matrix]]!=8,Message[Size::Ext],
G=GCreate[matrix,True];
G1=G1Create[matrix,True];
G2=G2Create[matrix,True];
G3=G3Create[matrix,True];

B1={{G,(G+G1+G2+G3)/4},{(G+G1+G2+G3)/4,(G+G1+G2+G3)/4}}//ArrayFlatten;
Print["\!\(\*SubscriptBox[\(B\), \(1\)]\)=",B1//TraditionalForm];
Return[B1];
];]

C1ext[matrix_] := Module[{G,G1,G2,G3},
If[Length[Flatten[matrix]]!=8,Message[Size::Ext],
G=GCreate[matrix,True];
G1=G1Create[matrix,True];
G2=G2Create[matrix,True];
G3=G3Create[matrix,True];

C1={{G,t*(G+G3)/2 + (1-t)(G1+G2)/2},{t*(G+G3)/2 + (1-t)(G1+G2)/2,(1-t)^2 *G + t*(1-t)*(G1+G2)+t^2*G3 }}//ArrayFlatten;
Print["\!\(\*SubscriptBox[\(C\), \(1\)]\)=",C1//TraditionalForm];
Return[C1];
];]

D1ext[matrix_] := Module[{G,G1,G2,G3},
If[Length[Flatten[matrix]]!=8,Message[Size::Ext],
G=GCreate[matrix,True];
G1=G1Create[matrix,True];
G2=G2Create[matrix,True];
G3=G3Create[matrix,True];

D1={{G,t*G + (1-t)G2},{t*G + (1-t)G1,t^2 *G + t*(1-t)*(G1+G2)+(1-t)^2*G3 }}//ArrayFlatten;
Print["\!\(\*SubscriptBox[\(D\), \(1\)]\)=",D1//TraditionalForm];
Return[D1];
];]

D2ext[matrix_] := Module[{G,G1,G2,G3},
If[Length[Flatten[matrix]]!=8,Message[Size::Ext],
G=GCreate[matrix,True];
G1=G1Create[matrix,True];
G2=G2Create[matrix,True];
G3=G3Create[matrix,True];

D2={{G,t*G3 + (1-t)G1},{t*G3 + (1-t)G2,t^2 *G + t*(1-t)*(G1+G2)+(1-t)^2*G3 }}//ArrayFlatten;
Print["\!\(\*SubscriptBox[\(D\), \(2\)]\)=",D2//TraditionalForm];
Return[D2];
];]

E1ext[matrix_] := Module[{G,G1,G2,G3},
If[Length[Flatten[matrix]]!=8,Message[Size::Ext],
G=GCreate[matrix,True];
G1=G1Create[matrix,True];
G2=G2Create[matrix,True];
G3=G3Create[matrix,True];

E1={{G,t*G + (1-t)G1},{t*G + (1-t)G2,t^2 *G + t*(1-t)*(G1+G2)+(1-t)^2*G3 }}//ArrayFlatten;
Print["\!\(\*SubscriptBox[\(E\), \(1\)]\)=",E1//TraditionalForm];
Return[E1];
];]

E2ext[matrix_] := Module[{G,G1,G2,G3},
If[Length[Flatten[matrix]]!=8,Message[Size::Ext],
G=GCreate[matrix,True];
G1=G1Create[matrix,True];
G2=G2Create[matrix,True];
G3=G3Create[matrix,True];

E2={{G,t*G3 + (1-t)G2},{t*G3 + (1-t)G1,t^2 *G + t*(1-t)*(G1+G2)+(1-t)^2*G3 }}//ArrayFlatten;
Print["\!\(\*SubscriptBox[\(E\), \(2\)]\)=",E2//TraditionalForm];
Return[E2];
];]


(* Nash Equilibria *)
FindPureNE[matrix_]:=Module[{mat2},
If[AllTrue[Flatten@matrix,NumberQ],
mat2=matrix;
For[i=1,i< Length[matrix]+1,i++,
For[j=1,j<Length[matrix[[1]]]+1,j++,
If[matrix[[i,j,2]]== Max[matrix[[i,;;,2]]]&&matrix[[i,j,1]]== Max[matrix[[;;,j,1]]],
mat2[[i,j]]=Style[Highlighted[mat2[[i,j]],Background->LightBlue,Frame->True,FrameStyle->Black],Bold];,{},{}]]];Return[NEmatrix=TraditionalForm@mat2];,Message[Input::FindPureNE,matrix]];]


RangeFindPureNE[matrix_,list_]:=DynamicModule[{xmin,xmax,mat},
x=list[[1]];
xmin=list[[2]];
xmax=list[[3]];
Manipulate[mat=matrix/.x->param;FindPureNE[mat];TraditionalForm@NEmatrix,{{param,0.24,ToString[x]},xmin,xmax,0.01},
FrameLabel->{"","",Style["Nash Equilibrium finder",Bold,FontSize->16],""},ContentSize->{550,175},Alignment->Center]];


(* Maximin *)
Maximin[matrix_]:=Module[{rowp,columnp,mins1,mins2,mat2},
If[AllTrue[Flatten@matrix,NumberQ],
rowp=matrix[[;;,;;,1]];
columnp=matrix[[;;,;;,2]] //Transpose;
mins1={};
mins2={};
mat2=matrix;
Do[AppendTo[mins1,Min[rowp[[i]]]],{i,1,Length[rowp]}];
Do[AppendTo[mins2,Min[columnp[[j]]]],{j,1,Length[columnp]}];
maximinp1=Max[mins1];
maximinp2=Max[mins2];
Do[mat2[[Position[mins1,maximinp1][[j]],i]]=Highlighted[mat2[[Position[mins1,maximinp1][[j]],i]],Background->LightGreen,Frame->True,FrameStyle->Black],{i,1,Length[columnp]},{j,1,Length[Position[mins1,maximinp1]]}];
Do[mat2[[i,Position[mins2,maximinp2][[j]]]]=Highlighted[mat2[[i,Position[mins2,maximinp2][[j]]]],Background->LightGreen,Frame->True,FrameStyle->Black],{i,1,Length[rowp]},{j,1,Length[Position[mins2,maximinp2]]}];
Return[Maximat=mat2//.{Highlighted[List[List[d___]],ee___]->Highlighted[List[d],Background->LightGreen,Frame->True,FrameStyle->Black],
Highlighted[List[Highlighted[List[a___],b___]],c___]->Highlighted[a,Background->LightGreen,Frame->True,FrameStyle->Black]} //TraditionalForm];
,Message[Input::Maximin,matrix],Message[Input::Maximin,matrix]]]


RangeMaximin[matrix_,list_]:=DynamicModule[{xmin,xmax,mat},
x=list[[1]];
xmin=list[[2]];
xmax=list[[3]];
Manipulate[mat=matrix/.x->param;Maximin[mat];TraditionalForm@Maximat,{{param,0.24,ToString[x]},xmin,xmax,0.01},
FrameLabel->{"","",Style["Maximin",Bold,FontSize->16],""},ContentSize->{550,175},Alignment->Center]];


(* Dominated strategies *)
DominatedStrategies[matrix_]:=Module[{rowp,columnp,testlist1,testlist2,difr,difc,mat},
If[AllTrue[Flatten@matrix,NumberQ],
rowp=matrix[[;;,;;,1]];
columnp=matrix[[;;,;;,2]] //Transpose;
mat=matrix;
testlist1={};
testlist2={};

Do[{difc=columnp[[i]]-columnp[[j]];
	difr=rowp[[l]]-rowp[[m]];
	If[i!=j && Length[Union[Positive[difc]]]==1&&Union[Negative[difc]][[1]]==True,
	Do[mat[[k,i]]=Highlighted[matrix[[k,i]],Frame->True,FrameStyle->Black,Background->RGBColor[1,0,0,0.55]],{k,1,Length[columnp[[1]]]}],
	{},
	Print["U column"]
	];
	If[l!=m && Length[Union[Positive[difr]]]==1&&Union[Negative[difr]][[1]]==True,
	Do[mat[[l,k]]=Highlighted[matrix[[l,k]],Frame->True,FrameStyle->Black,Background->RGBColor[1,0,0,0.55]],{k,1,Length[rowp[[1]]]}],
	{},
	Print["U row"]
	];
	},
	{i,1,Length[columnp]},{j,1,Length[columnp]},{l,1,Length[rowp]},{m,1,Length[rowp]}
	];
	DomMat=mat//TraditionalForm;
Return[DomMat];,Message[Input::DominatedStrategies,matrix]];]


RangeDomStrat[matrix_,list_]:=DynamicModule[{xmin,xmax,mat},
x=list[[1]];
xmin=list[[2]];
xmax=list[[3]];
Manipulate[mat=matrix/.x->param;DominatedStrategies[mat];TraditionalForm@DomMat,{{param,0.24,ToString[x]},xmin,xmax,0.01},
FrameLabel->{"","",Style["Dominated Strategies",Bold,FontSize->16],""},ContentSize->{550,175},Alignment->Center]];


GenerateReport[matrix_,name_]:= Module[{},Which[
(*t1*)AllTrue[Flatten@matrix,NumberQ]&& Dimensions[matrix]=={2,2,2},
						(*v1*)      A0ext[matrix];
									B0ext[matrix];
									C0ext[matrix];
									A1ext[matrix];
									A2ext[matrix];
									B1ext[matrix];
									C1ext[matrix];
									D1ext[matrix];
									D2ext[matrix];
									E1ext[matrix];
									E2ext[matrix];
									Print[Style["Pure Nash equilibria: \t",Bold],FindPureNE[matrix]];
									Print[Style["Maximin strategies: \t",Bold],Maximin[matrix]];
									Print[Style["Dominated strategies: \t",Bold],DominatedStrategies[matrix]];
									Export["Report_"<>ToString[name]<>".pdf",Grid[{
									{Style["\!\(\*SubscriptBox[\(A\), \(0\)]\)= \t",Bold],TraditionalForm@A0},{Style["\!\(\*SubscriptBox[\(A\), \(1\)]\)= \t",Bold],TraditionalForm@A1},{Style["\!\(\*SubscriptBox[\(A\), \(2\)]\)= \t",Bold],TraditionalForm@A2},
									{Style["\!\(\*SubscriptBox[\(B\), \(0\)]\)= \t",Bold],TraditionalForm@B0},{Style["\!\(\*SubscriptBox[\(B\), \(1\)]\)= \t",Bold],TraditionalForm@B1},
									{Style["\!\(\*SubscriptBox[\(C\), \(0\)]\)= \t",Bold],TraditionalForm@C0},{Style["\!\(\*SubscriptBox[\(C\), \(1\)]\)= \t",Bold],TraditionalForm@C1},
									{Style["\!\(\*SubscriptBox[\(D\), \(1\)]\)= \t",Bold],TraditionalForm@D1},{Style["\!\(\*SubscriptBox[\(D\), \(2\)]\)= \t",Bold],TraditionalForm@D2},
									{Style["\!\(\*SubscriptBox[\(E\), \(1\)]\)= \t",Bold],TraditionalForm@E1},{Style["\!\(\*SubscriptBox[\(E\), \(2\)]\)= \t",Bold],TraditionalForm@E2},
									{Style["Pure Nash equilibria: \t",Bold],FindPureNE[matrix]},
									{Style["Maximin strategies: \t",Bold],Maximin[matrix]},
									{Style["Dominated strategies: \t",Bold],DominatedStrategies[matrix]}},Alignment->{{Right,Left}}]],
									
(*t2*) Dimensions[matrix]=={2,2,2},
					    (*v2*)      A0ext[matrix];
									B0ext[matrix];
									C0ext[matrix];
									A1ext[matrix];
									A2ext[matrix];
									B1ext[matrix];
									C1ext[matrix];
									D1ext[matrix];
									D2ext[matrix];
									E1ext[matrix];
									E2ext[matrix]; 
					                Export["Report_"<>ToString[name]<>"_extensions"<>".pdf",Grid[{
									{Style["\!\(\*SubscriptBox[\(A\), \(0\)]\)= \t",Bold],TraditionalForm@A0},{Style["\!\(\*SubscriptBox[\(A\), \(1\)]\)= \t",Bold],TraditionalForm@A1},{Style["\!\(\*SubscriptBox[\(A\), \(2\)]\)= \t",Bold],TraditionalForm@A2},
									{Style["\!\(\*SubscriptBox[\(B\), \(0\)]\)= \t",Bold],TraditionalForm@B0},{Style["\!\(\*SubscriptBox[\(B\), \(1\)]\)= \t",Bold],TraditionalForm@B1},
									{Style["\!\(\*SubscriptBox[\(C\), \(0\)]\)= \t",Bold],TraditionalForm@C0},{Style["\!\(\*SubscriptBox[\(C\), \(1\)]\)= \t",Bold],TraditionalForm@C1},
									{Style["\!\(\*SubscriptBox[\(D\), \(1\)]\)= \t",Bold],TraditionalForm@D1},{Style["\!\(\*SubscriptBox[\(D\), \(2\)]\)= \t",Bold],TraditionalForm@D2},
									{Style["\!\(\*SubscriptBox[\(E\), \(1\)]\)= \t",Bold],TraditionalForm@E1},{Style["\!\(\*SubscriptBox[\(E\), \(2\)]\)= \t",Bold],TraditionalForm@E2}
																							},Alignment->{{Right,Left}}]];
									Message[Input::GenerateReport],
									
(*t3*)AllTrue[Flatten@matrix,NumberQ],
					 (*v3*)         Print[Style["Pure Nash equilibria: \t",Bold],FindPureNE[matrix]];
									Print[Style["Maximin strategies: \t",Bold],Maximin[matrix]];
									Print[Style["Dominated strategies: \t",Bold],DominatedStrategies[matrix]];
									Export["Report_"<>ToString[name]<>"_properties"<>".pdf",Grid[{
									{Style["Pure Nash equilibria: \t",Bold],FindPureNE[matrix]},
									{Style["Maximin strategies: \t",Bold],Maximin[matrix]},
									{Style["Dominated strategies: \t",Bold],DominatedStrategies[matrix]}},Alignment->{{Right,Left}}]];
									Message[Size::GenerateReport],
									
(*t4*)Not[AllTrue[Flatten@matrix,NumberQ]&& Dimensions[matrix]=={2,2,2}],
					 (*v4*)       Message[Error::GenerateReport]
]]


EndPackage[];
