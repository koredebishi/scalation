# Question1ref PDF Extracted Reference Map

This file is generated from machine-extracted PDF text. It is for orientation only.
Do not cite this markdown file. Cite the original PDF/paper after checking the PDF.

PDF folder: `Question1ref`
Extracted text folder: `_extracted/Question1ref`

## Papers

### `AgentGuard.pdf`

- Extraction status: extracted
- Working title from extracted text: AgentGuard: Runtime Verification of AI Agents
- PDF source: `AgentGuard.pdf`
- Extracted text: `_extracted/Question1ref/AgentGuard.txt`

```text
===== PAGE 1 =====

AgentGuard: Runtime Verification of AI Agents
Roham Koohestani
JetBrains Research, The Netherlands
roham.koohestani@jetbrains.com
Abstract—The rapid evolution to autonomous, agentic AI from mathematics and formal methods with the existing theory
systems introduces significant risks due to their inherent unpre- of these agentic systems [6].
dictability and emergent behaviors; this also renders traditional
In this work, we present AgentGuard, a framework for
verification methods inadequate and necessitates a shift towards
runtime verfication of Agentic AI systems. We provide a proof-
probabilisticguaranteeswherethequestionisnolongerifasystem
will fail, but the probability of its failure within given constraints. of-concept for the verification process and demonstrate how it
This paper presents AgentGuard, a framework for runtime can easily be integrated into existing systems by integrating it
verification of Agentic AI systems that provides continuous, into an existing agentic system, RepairAgent [7]. We conclude
quantitative assurance through a new paradigm called Dynamic
by providing a roadmap for future work building on this
Probabilistic Assurance. AgentGuard operates as an inspection
framework. In the sections to follow we begin by further
layerthatobservesanagent’srawI/Oandabstractsitintoformal
events corresponding to transitions in a state model. It then expanding on the problem (section II), followed by a section
uses online learning to dynamically build and update a Markov outliningtheexistingworkandtheoryonformalverificationof
DecisionProcess(MDP)thatformallymodelstheagent’semergent AI Agents (section III). In section IV we present our approach
behavior.Usingprobabilisticmodelchecking,theframeworkthen
followed by its application to RapairAgent in . We conclude by
verifies quantitative properties in real-time.
presenting a discussion of the proposed framework (section V).
Index Terms—Agentic AI, Formal Verification, Probabilistic
Model-Checking, Runtime Verification
II. TheProblem
The problem and risk associated with these models are, at
I. Introduction
the same time, the characteristics that make them powerful.
The rapid evolution from generative artificial intelligence Here we discuss these further in detail. At the core of modern
(GenAI) to agentic AI marks a measurable leap in AI; agentic systems, you will most likely find a Large Language
Where previously, AI systems were confined to gathering

[SNIPPET TRUNCATED. RETURN TO THE PDF OR .TXT EXTRACTION FOR MORE CONTEXT.]
```

### `AgentVVVeryStrong.pdf`

- Extraction status: extracted
- Working title from extracted text: Preprint. Underreview.
- PDF source: `AgentVVVeryStrong.pdf`
- Extracted text: `_extracted/Question1ref/AgentVVVeryStrong.txt`

```text
===== PAGE 1 =====

Preprint. Underreview.
AIVV: Neuro-Symbolic LLM Agent-Integrated Verification
and Validation for Trustworthy Autonomous Systems
JiyongKwon1 UjinJeon2 SoojiLee3 GuangLin1,4
1SchoolofMechanicalEngineering,PurdueUniversity
2SchoolofElectricalandComputerEngineering,PurdueUniversity
3DepartmentofComputerScience,PurdueUniversity
4DepartmentofMathematics,PurdueUniversity
WestLafayette,IN47907,USA
{kwon165, ujeon, lee5264, guanglin}@purdue.edu
Abstract
Deeplearningmodelsexcelatdetectinganomalypatternsinnormaldata.
However, they do not provide a direct solution for anomaly classifica-
tion and scalability across diverse control systems, frequently failing to
distinguish genuine faults from nuisance faults caused by noise or the
controlsystem’slargetransientresponse. Consequently,becausealgorith-
mic fault validation remains unscalable, full Verification and Validation
(V&V)operationsarestillmanagedbyHuman-in-the-Loop(HITL)analysis,
resultinginanunsustainablemanualworkload. Toautomatethisessen-
tial oversight, we propose Agent-Integrated Verification and Validation
(AIVV),ahybridframeworkthatdeploysLargeLanguageModels(LLMs)
asadeliberativeouterloop. Becauserigoroussystemverificationstrictly
dependsonaccuratevalidation,AIVVescalatesmathematicallyflagged
anomaliestoarole-specializedLLMcouncil. Thecouncilagentsperform
collaborativevalidationbysemanticallyvalidatingnuisanceandtruefail-
uresbasedonnatural-language(NL)requirementstosecureahigh-fidelity
system-verificationbaseline. Buildingonthisfoundation,thecouncilthen
performssystemverificationbyassessingpost-faultresponsesagainstNL
operationaltolerances,ultimatelygeneratingactionableV&Vartifacts,such
asgain-tuningproposals. Experimentsonatime-seriessimulatorforUn-
mannedUnderwaterVehicles(UUVs)demonstratethatAIVVsuccessfully
digitizestheHITLV&Vprocess,overcomingthelimitationsofrule-based
fault classification and offering a scalable blueprint for LLM-mediated
oversightintime-seriesdatadomains.
1 Introduction
Inmission-criticaldomainssuchasUnmannedUnderwaterVehicles(UUVs),automated
anomaly detection systems must process telemetry that is noisy, highly stochastic, and
sparseingenuinefaultevents(Pangetal.,2021). Contemporaryresearchreliesheavilyon
deeplearningarchitectures(e.g., RNNs, Transformers)coupledwithrobustuncertainty
quantification to establish rigorous residual bounds and control the marginal error rate
(Hundmanetal.,2018;Tulietal.,2022;Angelopoulos&Bates,2021).

[SNIPPET TRUNCATED. RETURN TO THE PDF OR .TXT EXTRACTION FOR MORE CONTEXT.]
```

### `An_introduction_to_verification_and_validation_of_simulation_models_2013.pdf`

- Extraction status: extracted
- Working title from extracted text: Proceedings of the 2013 Winter Simulation Conference
- PDF source: `An_introduction_to_verification_and_validation_of_simulation_models_2013.pdf`
- Extracted text: `_extracted/Question1ref/An_introduction_to_verification_and_validation_of_simulation_models_2013.txt`

```text
===== PAGE 1 =====

Proceedings of the 2013 Winter Simulation Conference
R. Pasupathy, S.-H. Kim, A. Tolk, R. Hill, and M. E. Kuhl, eds
AN INTRODUCTION TO VERIFICATION AND VALIDATION OF SIMULATION MODELS
Robert G. Sargent
Department of Electrical Engineering and Computer Science
L.C. Smith College of Engineering and Computer Science
Syracuse University
Syracuse, NY 13244, USA
ABSTRACT
Model verification and validation are defined, and why model verification and validation are important is
discussed. The three approaches to deciding model validity are described. A graphical paradigm that
shows how verification and validation are related to the model development process and a flowchart that
shows how verification and validation is part of the model development process are presented and dis-
cussed. A recommended procedure for verification and validation is given.
1 INTRODUCTION
An introduction to verification and validation of simulation models is given in this paper. Verification and
validation are concerned with determining whether a model and its results are “correct” for a specific use
or purpose. Formally, model verification is defined as “ensuring that the computer program of the com-
puterized model and its implementation are correct” and model validation is defined as the “substantiation
that a computerized model within its domain of applicability possesses a satisfactory range of accuracy
consistent with the intended application of the model.” Our discussion of verification and validation of
simulation models will be primarily concerned with simulation models that are used to predict system be-
haviors such as systems outputs. Two related topics are model credibility and model usability. Model
credibility is concerned with developing in (potential) users the confidence they require in order to use a
model and in the information derived from that model. Model usability is determining that the model and
its user instructions are easy to use.
It is important that verification and validation of a simulation model be performed for each use or
purpose of a model. If the purpose of a simulation model is to answer a variety of questions, the validity
of the model needs to be determined with respect to each question. The developers and users of simula-
tion models, the decision makers using information obtained from the results of these models, and the in-
dividuals affected by decisions based on such models are all rightly concerned with whether a model

[SNIPPET TRUNCATED. RETURN TO THE PDF OR .TXT EXTRACTION FOR MORE CONTEXT.]
```

### `AutonomousAgentPaper.pdf`

- Extraction status: extracted
- Working title from extracted text: Autonomous Agents for Scientific Discovery:
- PDF source: `AutonomousAgentPaper.pdf`
- Extracted text: `_extracted/Question1ref/AutonomousAgentPaper.txt`

```text
===== PAGE 1 =====

Autonomous Agents for Scientific Discovery:
Orchestrating Scientists, Language, Code, and Physics
LianhaoZhou1 HongyiLing1 CongFu1 YepengHuang2
MichaelSun3 WendiYu1 XiaoxuanWang4 XinerLi1
XingyuSu1 JunkaiZhang4 XiusiChen5 ChenxingLiang1
XiaofengQian6,7,8 HengJi5 WeiWang4 MarinkaZitnik2 ShuiwangJi1,6,9∗
1DepartmentofComputerScienceandEngineering,TexasA&MUniversity
2DepartmentofBiomedicalInformatics,HarvardMedicalSchool
3ComputerScienceandArtificialIntelligenceLaboratory,MassachusettsInstituteofTechnology
4DepartmentofComputerScience,UniversityofCalifornia,LosAngeles
5SiebelSchoolofComputingandDataScience,UniversityofIllinoisUrbanaChampaign
6DepartmentofMaterialsScienceandEngineering,TexasA&MUniversity
7DepartmentofElectricalandComputerEngineering,TexasA&MUniversity
8DepartmentofPhysicsandAstronomy,TexasA&MUniversity
9J.MikeWalker’66DepartmentofMechanicalEngineering,TexasA&MUniversity
Abstract
Computinghaslongservedasacornerstoneofscientificdiscovery. Recently,a
paradigmshifthasemergedwiththeriseoflargelanguagemodels(LLMs),intro-
ducingautonomoussystems,referredtoasagents,thatacceleratediscoveryacross
varyinglevelsofautonomy. Theselanguageagentsprovideaflexibleandversatile
frameworkthatorchestratesinteractionswithhumanscientists,naturallanguage,
computerlanguageandcode,andphysics. Thispaperpresentsourviewandvision
ofLLM-basedscientificagentsandtheirgrowingroleintransformingthescientific
discoverylifecycle,fromhypothesisdiscovery,experimentaldesignandexecution,
toresultanalysisandrefinement. Wecriticallyexaminecurrentmethodologies,
emphasizingkeyinnovations,practicalachievements,andoutstandinglimitations.
Additionally,weidentifyopenresearchchallengesandoutlinepromisingdirections
forbuildingmorerobust,generalizable,andadaptivescientificagents.Ouranalysis
highlightsthetransformativepotentialofautonomousagentstoacceleratescientific
discoveryacrossdiversedomains.
1 Introduction
Scientific discovery is fundamental to advancing human knowledge, driving innovations across
diversefieldssuchasbiology,chemistry,physics,andmaterialsscience,etc.[Zhangetal.,2025a,
Wangetal.,2023a].Thepivotalroleofcomputinginadvancingdiscoveriesacrossthenaturalsciences
haslongbeenrecognized[Dirac,1929],withimpactsrangingfromquantummechanics[Landau
andLifshitz,2013a,Tong,2025a]tofluiddynamics[LandauandLifshitz,2013b,Tong,2025b]. For
manyyears,theprocessofscientificdiscoveryhasbeenpredominantlydependentonhumanintuition,
expertise,

[SNIPPET TRUNCATED. RETURN TO THE PDF OR .TXT EXTRACTION FOR MORE CONTEXT.]
```

### `comp_data_science.pdf`

- Extraction status: extracted
- Working title from extracted text: Introduction to
- PDF source: `comp_data_science.pdf`
- Extracted text: `_extracted/Question1ref/comp_data_science.txt`

```text
===== PAGE 1 =====

. . .
Introduction to
Computational Data Science
Using ScalaTion
...
John A. Miller
Department of Computer Science
University of Georgia
...
December 24, 2024
1

===== PAGE 2 =====

2

===== PAGE 3 =====

Brief Table of Contents
1 Introduction to Data Science 33
1.1 Data Science . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 33
1.2 ScalaTion . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 35
1.3 A Data Science Project . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 42
1.4 Additional Textbooks . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 45
I Foundations 47
2 Linear Algebra 49
2.1 Linear System of Equations . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 49
2.2 Matrix Inversion . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 50
2.3 Vector . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 51
2.4 Vector Calculus . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 54
2.5 Matrix . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 57
2.6 Matrix Factorization . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 60
2.7 Internal Representation . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 61
2.8 Tensor . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 62
2.9 Exercises . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 64
2.10 Further Reading . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 67
3 Probability 69
3.1 Probability Measure . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 69
3.2 Random Variable . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 71
3.3 Probability Distribution . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 72
3.4 Empirical Distribution . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 76
3.5 Expectation . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 77
3.6 Algebra of Random Variables . . . . . . . . . . . . . . . . . . . . .

[SNIPPET TRUNCATED. RETURN TO THE PDF OR .TXT EXTRACTION FOR MORE CONTEXT.]
```

### `DURA-CPS_A_Multi-Role_Orchestrator_for_Dependability_Assurance_in_LLM-Enabled_Cyber-Physical_Systems.pdf`

- Extraction status: extracted
- Working title from extracted text: 2025 55th Annual IEEE/IFIP International Conference on Dependable Systems and Networks Workshops (DSN-W)
- PDF source: `DURA-CPS_A_Multi-Role_Orchestrator_for_Dependability_Assurance_in_LLM-Enabled_Cyber-Physical_Systems.pdf`
- Extracted text: `_extracted/Question1ref/DURA-CPS_A_Multi-Role_Orchestrator_for_Dependability_Assurance_in_LLM-Enabled_Cyber-Physical_Systems.txt`

```text
===== PAGE 1 =====

2025 55th Annual IEEE/IFIP International Conference on Dependable Systems and Networks Workshops (DSN-W)
DURA-CPS: A Multi-Role Orchestrator for
Dependability Assurance in LLM-Enabled
Cyber-Physical Systems
1st Trisanth Srinivasan 1st Santosh Patapati 2nd Himani Musku
Department of AI Deployment and Safety Department of AI Deployment and Safety School of Computer Science
Cyrion Labs Cyrion Labs Carnegie Mellon University
Dallas, USA Dallas, USA Pittsburgh, USA
trisanth@cyrionlabs.org santosh@cyrionlabs.org hmusku@andrew.cmu.edu
3rd Idhant Gode 4th Aditya Arora 5th Samvit Bhattacharya 6th Abubakr Nazriev
College of Engineering ACM AI Department of R&D Sentinel DE
Cornell University University of California San Diego Cyrion Labs University of Montana
Ithaca, USA San Diego, USA San Roman, USA Missoula, USA
iag32@cornell.edu a7arora@ucsd.edu samvitb@cyrionlabs.org abu.nazriev@umconnect.umt.edu
7th Sanika Hirave 8th Zaryab Kanjiani 9th Srinjoy Ghose
Dept. of Computer Science & Engineering School of Applied Economics and Management School of Science
Oakland University Cornell University University of North Texas
Rochester, USA Ithaca, USA Denton, USA
sanikahirave@oakland.edu zk226@cornell.edu srinjoyghose@my.unt.edu
Abstract—Cyber-Physical Systems (CPS) increasingly depend its requirements and behaves as intended. V&V remains a
on advanced AI techniques to operate in critical applications. complex and costly endeavor in most domains [1].
However, traditional verification and validation methods often
The challenges of V&V grow when Artificial Intelligence
struggle to handle the unpredictable and dynamic nature of
(AI) is incorporated into CPS. This difficulty is compounded
AI components. In this paper, we introduce DURA-CPS, a
novel framework that employs multi-role orchestration to au- as increasingly complex AI techniques, such as Deep Neural
tomate the iterative assurance process for AI-powered CPS. Networks (DNNs) and Large Language Models (LLMs) are
By assigning specialized roles (e.g., safety monitoring, security deployed in these systems. The behavior of AI is often
assessment, fault injection, and recovery planning) to dedicated
sensitive to unexpected changes in the environment [2]. This
agentswithinasimulatedenvironment,DURA-CPScontinuously
unpredictability, coupled with a vulnerability to adversarial
evaluatesandrefinesAIbehavioragainstarangeofdependability
requirements. We demonstrate the framework through a case attacks [3],

[SNIPPET TRUNCATED. RETURN TO THE PDF OR .TXT EXTRACTION FOR MORE CONTEXT.]
```

### `Guilding-LLM-To-Fix-Flaws.pdf`

- Extraction status: extracted
- Working title from extracted text: 1v43032.6052:viXra
- PDF source: `Guilding-LLM-To-Fix-Flaws.pdf`
- Extracted text: `_extracted/Question1ref/Guilding-LLM-To-Fix-Flaws.txt`

```text
===== PAGE 1 =====

5202
nuJ
82
]ES.sc[
1v43032.6052:viXra
Guiding AI to Fix Its Own Flaws: An Empirical Study on
LLM-Driven Secure Code Generation
HaoYan SwapneelSuhasVaidya
GeorgeMasonUniversity GeorgeMasonUniversity
Fairfax,USA Fairfax,USA
hyan5@gmu.edu svaidya4@gmu.edu
XiaokuanZhang ZiyuYao
GeorgeMasonUniversity GeorgeMasonUniversity
Fairfax,USA Fairfax,USA
xiaokuan@gmu.edu ziyuyao@gmu.edu
Abstract
Coding Question Coding Question LLM-generated
LargeLanguageModels(LLMs)havebecomepowerfultoolsfor
Vulnerable Code
automatedcodegeneration.However,thesemodelsoftenoverlook
criticalsecuritypractices,whichcanresultinthegenerationof
insecurecodethatcontainsvulnerabilities—weaknessesorflawsin
Self-generated
thecodethatattackerscanexploittocompromiseasystem.How- Vul. Hints
ever,therehasbeenlimitedexplorationofstrategiestoguideLLMs
Explained
ingeneratingsecurecodeandalackofin-depthanalysisofthe CodeQL
effectivenessofLLMsinrepairingcodecontainingvulnerabilities. Feedback
Inthispaper,wepresentacomprehensiveevaluationofstate-of-
the-artLLMsbyexaminingtheirinherenttendenciestoproduce
insecurecode,theircapabilitytogeneratesecurecodewhenguided Assessment of Proactive Vulnerability Post-Hoc
by self-generated vulnerability hints, and their effectiveness in LLMs on Code Prevention w. Self- Vulnerability Repair
Vulnerability generated Hints w. Feedback
repairing vulnerabilities when provided with different levels of
feedback.Ourstudycoversbothproprietaryandopen-weightmod- Figure1:WeperformedacomprehensiveevaluationofLLMs
elsacrossvariousscalesandleveragesestablishedbenchmarksto
insecurecodegenerationandrepairacrossthreedimensions.
assessawiderangeofvulnerabilitytypes.Throughquantitative
andqualitativeanalyses,werevealthatalthoughLLMsareprone
togeneratinginsecurecode,advancedmodelscanbenefitfrom
GitHubCopilotgeneratedvulnerablecodein40%ofcasesacross
vulnerabilityhintsandfine-grainedfeedbacktoavoidorfixvulner-
18differenttypesofvulnerabilities.Thesevulnerabilitiesarecate-
abilities.Wealsoprovideactionablesuggestionstodevelopersto
gorizedundertheCommonWeaknessEnumeration(CWE)[1],a
reducevulnerabilitieswhenusingLLMsforcodegeneration.
well-knownframeworkthatstandardizessoftwareweaknesses.
While existing studies highlight concerns regarding vulnera-
CCSConcepts
bilities in LLM-generated code, a comprehensive evaluation

[SNIPPET TRUNCATED. RETURN TO THE PDF OR .TXT EXTRACTION FOR MORE CONTEXT.]
```

### `LatourLabLife.pdf`

- Extraction status: extracted
- Working title from extracted text: LABORATORY LIFE
- PDF source: `LatourLabLife.pdf`
- Extracted text: `_extracted/Question1ref/LatourLabLife.txt`

```text
===== PAGE 1 =====

===== PAGE 2 =====

LABORATORY LIFE

===== PAGE 3 =====

This page intentionally left blank

===== PAGE 4 =====

LABORATORY LIFE
The Construction of
Scientific Facts
Bruno Latour · Steve Woolgar
Introduction by Jonas Salk
With a new postscript and index by the authors
PRINCETON UNIVERSITY PRESS
PRINCETON, NEW JERSEY

===== PAGE 5 =====

Published by Princeton University Press, 41 William Street, Princeton, New Jersey 08540
In the United Kingdom: Princeton University Press, Chichester, West Sussex
Copyright © 1979 by Sage Publications, Inc.
Copyright © 1986 by Princeton University Press
All rights reserved
LCC 85-43378
ISBN 0-692-09418-7
Princeton University Press books are printed on acid-free paper, and meet the guidelines for
permanence and durability of the Committee on Production Guidelines for Book Longevity
of the Council on Library Resources
Disclaimer:
Some images in the original version of this book are not
available for inclusion in the eBook.

===== PAGE 6 =====

CONTENTS
Preface to Second Edition 7
Acknowledgements 9
Introduction by Jonas Salk 11
1 FROM ORDER TO DISORDER 15
The Observer and the Scientist 19
The Social and the Scientific: A Participant's Resource 21
The Social and the Scientific: The Observer's Dilemma 23
The "Anthropology" of Science 27
The Construction of Order 33
Materials and Methods 39
The Organisation of the Argument 40
Notes 42
2 AN ANTHROPOLOGIST VISITS THE LABORATORY 43
Literary Inscription 45
The Culture of the Laboratory 53
Articles about Neuroendocrinology 54
The "Phenomenotechnique" 63
Documents and Facts 69
The Publication List 72
Statement Types 75
The Transformation of Statement Types 81
Conclusion 86
Notes 88
Photograph File 91
3 THE CONSTRUCTION OF A FACT: THE CASE OF TRF(H) 105
TRF(H) in Its Different Contexts 107
The Delineation of a Subspecialty: The Isolation and
Characterisation of TRF(H) 112
A Choice of Strategies 114
5

===== PAGE 7 =====

6 LABORATORY LIFE
The Elimination of Concurrent Efforts by New Investments 119
The Construction of a New Object 124
The Peptidic Nature of TRF 129
Narrowing Down the Possibilities 142
TRF Moves into Other Networks 148
Notes 149
4 THE MICROPROCESSING OF FACTS 151
The Construction and Dismantling of Facts in Conversation 154
The Sociological Analysis of "Thought Processes" 168
Facts and Artefacts 174
Notes 184
5 CYCLES OF CREDIT 187
Credit: Reward and Credibility
What Motivates Scientists? 189
Limitations of the Notion of Credit as Reward 192
The

[SNIPPET TRUNCATED. RETURN TO THE PDF OR .TXT EXTRACTION FOR MORE CONTEXT.]
```

### `RAG-Paper.pdf`

- Extraction status: extracted
- Working title from extracted text: From Local to Global: A GraphRAG Approach to
- PDF source: `RAG-Paper.pdf`
- Extracted text: `_extracted/Question1ref/RAG-Paper.txt`

```text
===== PAGE 1 =====

From Local to Global: A GraphRAG Approach to
Query-Focused Summarization
DarrenEdge1† HaTrinh1† NewmanCheng2 JoshuaBradley2 AlexChao3
ApurvaMody3 StevenTruitt2 DashaMetropolitansky1 RobertOsazuwaNess1
JonathanLarson1
1MicrosoftResearch
2MicrosoftStrategicMissionsandTechnologies
3MicrosoftOfficeoftheCTO
{daedge,trinhha,newmancheng,joshbradley,achao,moapurva,
steventruitt,dasham,robertness,jolarso}@microsoft.com
†Theseauthorscontributedequallytothiswork
Abstract
The use of retrieval-augmented generation (RAG) to retrieve relevant informa-
tion from an external knowledge source enables large language models (LLMs)
toanswerquestionsoverprivateand/orpreviouslyunseendocumentcollections.
However, RAG fails on global questions directed at an entire text corpus, such
as “What are the main themes in the dataset?”, since this is inherently a query-
focused summarization (QFS) task, rather than an explicit retrieval task. Prior
QFS methods, meanwhile, do not scale to the quantities of text indexed by typ-
ical RAG systems. To combine the strengths of these contrasting methods, we
propose GraphRAG, a graph-based approach to question answering over private
textcorporathatscaleswithboththegeneralityofuserquestionsandthequantity
ofsourcetext. OurapproachusesanLLMtobuildagraphindexintwostages:
first,toderiveanentityknowledgegraphfromthesourcedocuments,thentopre-
generatecommunitysummariesforallgroupsofcloselyrelatedentities. Givena
question,eachcommunitysummaryisusedtogenerateapartialresponse,before
all partial responses are again summarized in a final response to the user. For a
classofglobalsensemakingquestionsoverdatasetsinthe1milliontokenrange,
we show that GraphRAG leads to substantial improvements over a conventional
RAGbaselineforboththecomprehensivenessanddiversityofgeneratedanswers.
1 Introduction
Retrieval augmented generation (RAG) (Lewis et al., 2020) is an established approach to using
LLMs to answer queries based on data that is too large to contain in a language model’s context
window,meaningthemaximumnumberoftokens(unitsoftext)thatcanbeprocessedbytheLLM
atonce (Kuratovetal.,2024;Liuetal.,2023).InthecanonicalRAGsetup,thesystemhasaccessto
alargeexternalcorpusoftextrecordsandretrievesasubsetofrecordsthatareindividuallyrelevant
tothequeryandcollectivelysmallenoughtofitintothecontextwindowoftheLLM.TheLLMthen
Preprint.Underreview.
5202
beF
91
]LC.sc[
2v03161.4042:viXra

===== PAGE 2

[SNIPPET TRUNCATED. RETURN TO THE PDF OR .TXT EXTRACTION FOR MORE CONTEXT.]
```

### `React-Paper.pdf`

- Extraction status: extracted
- Working title from extracted text: PublishedasaconferencepaperatICLR2023
- PDF source: `React-Paper.pdf`
- Extracted text: `_extracted/Question1ref/React-Paper.txt`

```text
===== PAGE 1 =====

PublishedasaconferencepaperatICLR2023
REACT: SYNERGIZING REASONING AND ACTING IN
LANGUAGE MODELS
ShunyuYao∗*,1,JeffreyZhao2,DianYu2,NanDu2,IzhakShafran2,KarthikNarasimhan1,YuanCao2
1DepartmentofComputerScience,PrincetonUniversity
2GoogleResearch,Brainteam
1{shunyuy,karthikn}@princeton.edu
2{jeffreyzhao,dianyu,dunan,izhak,yuancao}@google.com
ABSTRACT
Whilelargelanguagemodels(LLMs)havedemonstratedimpressiveperformance
across tasks in language understanding and interactive decision making, their
abilitiesforreasoning(e.g. chain-of-thoughtprompting)andacting(e.g. action
plangeneration)haveprimarilybeenstudiedasseparatetopics. Inthispaper,we
exploretheuseofLLMstogeneratebothreasoningtracesandtask-specificactions
inaninterleavedmanner,allowingforgreatersynergybetweenthetwo: reasoning
traces help the model induce, track, and update action plans as well as handle
exceptions,whileactionsallowittointerfacewithandgatheradditionalinformation
fromexternalsourcessuchasknowledgebasesorenvironments. Weapplyour
approach,namedReAct,toadiversesetoflanguageanddecisionmakingtasks
and demonstrate its effectiveness over state-of-the-art baselines in addition to
improved human interpretability and trustworthiness. Concretely, on question
answering(HotpotQA)andfactverification(Fever),ReActovercomesprevalent
issues of hallucination and error propagation in chain-of-thought reasoning by
interactingwithasimpleWikipediaAPI,andgeneratinghuman-liketask-solving
trajectories that are more interpretable than baselines without reasoning traces.
Furthermore, on two interactive decision making benchmarks (ALFWorld and
WebShop),ReActoutperformsimitationandreinforcementlearningmethodsby
anabsolutesuccessrateof34%and10%respectively,whilebeingpromptedwith
onlyoneortwoin-contextexamples.
1 INTRODUCTION
Auniquefeatureofhumanintelligenceistheabilitytoseamlesslycombinetask-orientedactionswith
verbalreasoning(orinnerspeech, Alderson-Day&Fernyhough,2015),whichhasbeentheorizedto
playanimportantroleinhumancognitionforenablingself-regulationorstrategization(Vygotsky,
1987;Luria,1965;Fernyhough,2010)andmaintainingaworkingmemory(Baddeley,1992). Con-
sidertheexampleofcookingupadishinthekitchen.

[SNIPPET TRUNCATED. RETURN TO THE PDF OR .TXT EXTRACTION FOR MORE CONTEXT.]
```

### `Survey on Agentic Frameworks.pdf`

- Extraction status: extracted
- Working title from extracted text: LLM-based Agentic Reasoning Frameworks: A Survey from
- PDF source: `Survey on Agentic Frameworks.pdf`
- Extracted text: `_extracted/Question1ref/Survey on Agentic Frameworks.txt`

```text
===== PAGE 1 =====

LLM-based Agentic Reasoning Frameworks: A Survey from
Methods to Scenarios
BINGXIZHAO∗,
BeijingJiaotongUniversity,ChinaandLancasterUniversity,UnitedKingdom
LINGENGFOO∗,
MaxPlanckInstituteforInformatics,SaarlandInformaticsCampus,Germany
PINGHU,
UniversityofElectronicScienceandTechnologyofChina,China
CHRISTIANTHEOBALT,
MaxPlanckInstituteforInformatics,SaarlandInformaticsCampus,Germany
HOSSEINRAHMANI,
LancasterUniversity,UnitedKingdom
JUNLIU†, LancasterUniversity,UnitedKingdom
Recentadvancesintheintrinsicreasoningcapabilitiesoflargelanguagemodels(LLMs)havegivenriseto
LLM-basedagentsystemsthatexhibitnear-humanperformanceonavarietyofautomatedtasks.However,
althoughthesesystemssharesimilaritiesintermsoftheiruseofLLMs,differentreasoningframeworksofthe
agentsystemsteerandorganizethereasoningprocessindifferentways.Inthissurvey,weproposeasystematic
taxonomythatdecomposesagenticreasoningframeworksandanalyzehowtheseframeworksdominate
framework-levelreasoningbycomparingtheirapplicationsacrossdifferentscenarios.Specifically,wepropose
anunifiedformallanguagetofurtherclassifyagenticreasoningsystemsintosingle-agentmethods,tool-based
methods,andmulti-agentmethods.Afterthat,weprovideacomprehensivereviewoftheirkeyapplication
scenariosinscientificdiscovery,healthcare,softwareengineering,socialsimulation,andeconomics.Wealso
analyzethecharacteristicfeaturesofeachframeworkandsummarizedifferentevaluationstrategies.Our
surveyaimstoprovidetheresearchcommunitywithapanoramicviewtofacilitateunderstandingofthe
strengths,suitablescenarios,andevaluationpracticesofdifferentagenticreasoningframeworks.
CCSConcepts:•Generalandreference→Surveysandoverviews;•Computingmethodologies→
Naturallanguageprocessing.
AdditionalKeyWordsandPhrases:AgenticReasoning,LLM-basedAgent,ReasoningFrameworks.
ACMReferenceFormat:
BingxiZhao,LinGengFoo,PingHu,ChristianTheobalt,HosseinRahmani,andJunLiu.2025.LLM-based
AgenticReasoningFrameworks:ASurveyfromMethodstoScenarios.J.ACM37,4,Article111(August2025),
51pages.https://doi.org/XXXXXXX.XXXXXXX
1 Introduction
Large Language Models (LLMs), with their powerful generalization and promising

[SNIPPET TRUNCATED. RETURN TO THE PDF OR .TXT EXTRACTION FOR MORE CONTEXT.]
```

### `SurveyofLLMbaseScientificAgent.pdf`

- Extraction status: extracted
- Working title from extracted text: Towards Scientific Intelligence: A Survey of LLM-based Scientific Agents
- PDF source: `SurveyofLLMbaseScientificAgent.pdf`
- Extracted text: `_extracted/Question1ref/SurveyofLLMbaseScientificAgent.txt`

```text
===== PAGE 1 =====

Towards Scientific Intelligence: A Survey of LLM-based Scientific Agents
ShuoRen*1,2,CanXie*1,3,PuJian1,3,ZhenjiangRen1,3,
ChunlinLeng1,3,JiajunZhang†1,2,3,4
1 StateKeyLaboratoryofMultimodalArtificialIntelligenceSystems,
2 FoundationModelResearchCenter,InstituteofAutomation,CAS.
3 UniversityofChineseAcademyofScience,Beijing,China.
4 WuhanAIResearch,Wuhan,China. †jjzhang@nlpr.ia.ac.cn
*{shuo.ren,xiecan2024,jianpu2023,renzhenjiang2024,lengchunlin2023}@ia.ac.cn
Abstract agentsintegratedomain-specificknowledge,inter-
act through diverse action spaces (including soft-
As scientific research becomes increasingly
ware APIs, simulators, and analytical tools), and
complex,innovativetoolsareneededtoman-
processheterogeneousdatatypesrangingfromnu-
agevastdata,facilitateinterdisciplinarycollab-
mericaldatasetstomolecularstructuresandbiolog-
oration, and accelerate discovery. Large lan-
guagemodels(LLMs)arenowevolvinginto icalsequences. Thisspecializationequipsthemto
LLM-basedscientificagentsthatautomatecrit- managethegrowingcomplexityofmodernscience,
icaltasks—rangingfromhypothesisgeneration facilitateinterdisciplinarydiscovery,andaccelerate
andexperimentdesigntodataanalysisandsim-
thepaceofbreakthroughresearch.
ulation. Unlikegeneral-purposeLLMs,these
AstheadoptionofLLM-basedscientificagents
specialized agents integrate domain-specific
grows, a systematic review of their development,
knowledge,advancedtoolsets,androbustval-
applications, and challenges becomes essential.
idationmechanisms,enablingthemtohandle
complexdatatypes,ensurereproducibility,and While existing surveys provide comprehensive
drivescientificbreakthroughs. Thissurveypro- overviews of general LLM-based agents (Wang
videsafocusedreviewofthearchitectures,de- etal.,2024a;Xietal.,2023;Guoetal.,2024;Hu
sign,benchmarks,applications,andethicalcon- et al., 2024a; Li et al., 2024e; Xie et al., 2024;
siderationssurroundingLLM-basedscientific
Cheng et al., 2024; Shen, 2024; Gridach et al.,
agents. Wehighlightwhytheydifferfromgen-
2025),focusingspecificallyonLLM-basedscien-
eralagentsandthewaysinwhichtheyadvance
tific agents is crucial given their distinctive roles
research across various scientific fields. By
and requirements in the scientific domain. Sev-
examiningtheirdevelopmentandchallenges,
thissurveyoffersacomprehensiveroadmapfor eralrecentsurveyshavebeguntoaddressthisgap
researchersandpractitionerstoharnessthese from different vantage points: Luo et al.

[SNIPPET TRUNCATED. RETURN TO THE PDF OR .TXT EXTRACTION FOR MORE CONTEXT.]
```

### `TowardAScienceofAIReliabilit.pdf`

- Extraction status: extracted
- Working title from extracted text: Towards a Science of AI Agent Reliability
- PDF source: `TowardAScienceofAIReliabilit.pdf`
- Extracted text: `_extracted/Question1ref/TowardAScienceofAIReliabilit.txt`

```text
===== PAGE 1 =====

Towards a Science of AI Agent Reliability
Stephan Rabanser Sayash Kapoor Peter Kirgis Kangheng Liu Saiteja Utpala
Arvind Narayanan
Princeton University
Correspondence to {rabanser, sayashk, arvindn}@princeton.edu
Preprint as of February 24, 2026
A Interactive dashboard available at https://hal.cs.princeton.edu/reliability
Abstract
AI agents are increasingly deployed to execute important tasks. While rising accuracy scores on standard
benchmarkssuggestrapidprogress,manyagentsstillcontinuetofailinpractice. Thisdiscrepancyhighlights
a major limitation of current evaluations: focusing on a single metric is not enough to understand agent
behavior. Notably, it ignores whether agents behave consistently across runs, withstand perturbations,
fail predictably, or have bounded error severity. Grounded in safety-critical engineering, we provide
a holistic performance profile consisting of twelve metrics that decompose agent reliability along four
key dimensions: consistency, robustness, predictability, and safety. Evaluating 14 models across two
complementary benchmarks, we find that recent capability gains have only yielded small improvements in
reliability. By exposing these persistent limitations, our metrics complement traditional evaluations while
offering tools for reasoning about how agents perform, degrade, and fail.
1.0
0.8
0.6
0.4
0.2
0.0
ycaruccA
1.0
0.8
0.6
0.4
0.2
r=0.63
slope=0.21/yr
0.0
)
(
ytilibaileR
R
1.0
0.8
0.6
0.4
0.2
r=0.46
slope=0.03/yr
0.0
)
(
ytilibaileR
R
r=0.82
slope=0.15
1.0
0.8
0.6
0.4
0.2
0.0
2024-05 2024-09 2025-01 2025-05 2025-09 2026-01
Release Date
ycaruccA
1.0
0.8
0.6
0.4
0.2
r=0.73
slope=0.21/yr
0.0
2024-05 2024-09 2025-01 2025-05 2025-09 2026-01
Release Date
)
(
ytilibaileR
R
1.0
0.8
0.6
0.4
0.2
r=0.82
slope=0.10/yr
0.0
0.0 0.2 0.4 0.6 0.8 1.0
Accuracy
)
(
ytilibaileR
R
r=0.92
slope=0.38
AIAG
hcneb-τ
OpenAI: GPT-4 Turbo GPT-4o mini o1 GPT 5.2 GPT 5.2 (medium) GPT 5.2 (xhigh)
Google: Gemini 2.0 Flash Gemini 2.5 Flash Gemini 2.5 Pro Gemini 3.0 Pro
Anthropic: Claude 3.5 Haiku Claude 3.7 Sonnet Claude 4.5 Sonnet Claude 4.5 Opus
Figure 1: Reliability gains lag behind capability progress. Overall reliability shows slow improvement over
time. While accuracy rises steadily across both benchmarks (left), reliability trails behind (center), and the relationship
between the two varies across benchmarks (right), indicating that accuracy gains do not automatically yield

[SNIPPET TRUNCATED. RETURN TO THE PDF OR .TXT EXTRACTION FOR MORE CONTEXT.]
```

### `Verification_and_validation_of_simulation_models.pdf`

- Extraction status: extracted
- Working title from extracted text: Proceedings of the 2010 Winter Simulation Conference
- PDF source: `Verification_and_validation_of_simulation_models.pdf`
- Extracted text: `_extracted/Question1ref/Verification_and_validation_of_simulation_models.txt`

```text
===== PAGE 1 =====

Proceedings of the 2010 Winter Simulation Conference
B. Johansson, S. Jain, J. Montoya-Torres, J. Hugan, and E. Yücesan, eds.
VERIFICATION AND VALIDATION OF SIMULATION MODELS
Robert G. Sargent
Department of Electrical Engineering and Computer Science
L. C. Smith College of Engineering and Computer Science
Syracuse University
Syracuse, NY 13244, USA
ABSTRACT
In this paper we discuss verification and validation of simulation models. Four different approaches to de-
ciding model validity are described; two different paradigms that relate verification and validation to the
model development process are presented; various validation techniques are defined; conceptual model
validity, model verification, operational validity, and data validity are discussed; a way to document re-
sults is given; a recommended procedure for model validation is presented; and model accreditation is
briefly discussed.
1 INTRODUCTION
Simulation models are increasingly being used to solve problems and to aid in decision-making. The de-
velopers and users of these models, the decision makers using information obtained from the results of
these models, and the individuals affected by decisions based on such models are all rightly concerned
with whether a model and its results are “correct”. This concern is addressed through model verification
and validation. Model verification is often defined as “ensuring that the computer program of the compu-
terized model and its implementation are correct” and is the definition adopted here. Model validation is
usually defined to mean “substantiation that a computerized model within its domain of applicability pos-
sesses a satisfactory range of accuracy consistent with the intended application of the model” (Schlesinger
et al. 1979) and is the definition used here. A model sometimes becomes accredited through model accre-
ditation. Model accreditation determines if a model satisfies specified model accreditation criteria accord-
ing to a specified process. A related topic is model credibility. Model credibility is concerned with devel-
oping in (potential) users the confidence they require in order to use a model and in the information
derived from that model.
A model should be developed for a specific purpose (or application) and its validity determined with
respect to that purpose. If the purpose of a model is to answer a variety of questions, the validity of the
model needs to be determined with respect to each question.

[SNIPPET TRUNCATED. RETURN TO THE PDF OR .TXT EXTRACTION FOR MORE CONTEXT.]
```

### `Verification_And_Validation_Of_Simulation_Models_An_Advanced_Tutorial.pdf`

- Extraction status: extracted
- Working title from extracted text: Proceedings of the 2020 Winter Simulation Conference
- PDF source: `Verification_And_Validation_Of_Simulation_Models_An_Advanced_Tutorial.pdf`
- Extracted text: `_extracted/Question1ref/Verification_And_Validation_Of_Simulation_Models_An_Advanced_Tutorial.txt`

```text
===== PAGE 1 =====

Proceedings of the 2020 Winter Simulation Conference
K.-H. Bae, B. Feng, S. Kim, S. Lazarova-Molnar, Z. Zheng, T. Roeder, and R. Thiesing, eds.
VERIFICATION AND VALIDATION OF SIMULATION MODELS:
AN ADVANCED TUTORIAL
Robert G. Sargent
Department of Electrical Engineering and Computer Science
College of Engineering and Computer Science
Syracuse University
Syracuse, NY 13244, USA
ABSTRACT
Verification and validation (V&V) of simulation models are discussed in this paper. Different approaches
to deciding model validity are described and a graphical paradigm that relates V&V to the model
development process is presented and explained. Conceptual model validity, model verification, operational
validity, and data validity are discussed, documentation is briefly covered, and a recommended procedure
for model validation is presented. References for further information are provided when the various aspects
of conducting V&V of simulation models are discussed.
1 INTRODUCTION
This paper discusses verification and validation (V&V) of simulation models. V&V are concerned with
determining whether a model and its results are “correct” for a specific use or purpose. Model verification
is formally defined as “ensuring that the computer program of the computerized model and its
implementation are correct” and model validation is defined as the “substantiation that a computerized
model within its domain of applicability possesses a satisfactory range of accuracy consistent with the
intended application of the model.” Our discussion of V&V will focus primarily on simulation models that
are used to predict system behaviors such as systems outputs. Two related topics are model credibility and
model usability. Model credibility is concerned with developing in (potential) users the confidence they
require in order to use a model and the information derived from that model. Model usability is determining
if the model and its user instructions are easy to use. (See Sargent and Balci (2017) for a history on the
development of V&V of simulation models.)
A model should be developed for a specific purpose and its validity determined with respect to that
purpose. A developed model should usually be a parsimonious model, meaning the model is as simple as
possible yet meets its purpose. Furthermore, the accuracy of a model (sometimes referred to as model
fidelity) should usually be only what is needed to satisfy the model’s use or purpose. If the purpose of a
model is

[SNIPPET TRUNCATED. RETURN TO THE PDF OR .TXT EXTRACTION FOR MORE CONTEXT.]
```
