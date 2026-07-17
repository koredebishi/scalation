# Question3ref PDF Extracted Reference Map

This file is generated from machine-extracted PDF text. It is for orientation only.
Do not cite this markdown file. Cite the original PDF/paper after checking the PDF.

PDF folder: `Question3ref`
Extracted text folder: `_extracted/Question3ref`

## Papers

### `bouha_2015_dynamic_hybrid_traffic_modeling.pdf`

- Extraction status: extracted
- Working title from extracted text: A F S T D H
- PDF source: `bouha_2015_dynamic_hybrid_traffic_modeling.pdf`
- Extracted text: `_extracted/Question3ref/bouha_2015_dynamic_hybrid_traffic_modeling.txt`

```text
===== PAGE 1 =====

A F S T D H
IRST TEP OWARDS YNAMIC YBRID
T M
RAFFIC ODELING
Najia Bouha(cid:63)(cid:63) , Gildas Morvan(cid:63), Hassane Aboua¨ıssa(cid:63), Yoann Kubera(cid:63)
(cid:63) Univ. Lille Nord France, 59000 Lille, France,
U-Artois, LGI2A (EA 3926),
Technoparc Futura, 62400 Be´thune, France
Email: (hassane.abouaissa, gildas.morvan, yoann.kubera)@univ-artois.fr
(cid:63)(cid:63) Universite´ Ibn Zohr, Faculte´ des Sciences,
Agadir, Morocco,
Email: najia.bouha@gmail.com
KEYWORDS and forecasting, the repercussion of the construction of new
parts on infrastructure onto the global behavior of the traffic
Agent-based modeling; Multi-level modeling; Intelligent
flow, ... According to the defined objective, several models
transportation systems; Simulation; Traffic flow
have been developed and can be classified into microscopic,
ABSTRACT mesoscopic and macroscopic models. However, to simulate
Hybridtrafficmodelingandsimulationprovideanimportant large-scale road networks, it can be interesting to integrate
way to represent and evaluate large-scale traffic networks at different representations in the same framework which leads
differentlevels ofdetails. Thefirstlevel, called“microscopic” to the so-called “hybrid modeling” as shown on fig. 1. Note
allows the description of individual vehicles and their inter- that the concept of hybrid modeling has different meanings
actions as well as the study of driver’s individual behavior. accordingtothestudieddomain.Here,hybridmodelingmeans
The second, based on the analogy with fluidic dynamic, is the the coupling of different models.
“macroscopic” one and provides an efficient way to represent In this paper, the first step is devoted to the integration of
trafficflowbehaviorinlargetrafficinfrastructures,usingthree microscopicandmacroscopicmodelsintoasingleframework.
aggregated variables: traffic density, mean speed and traffic Theconceptofhybridmodelinghasbeendevelopedbyseveral
volume. An intermediate level called “mesoscopic” considers authors. Hence, some existing hybrid micro-macro traffic
a group of vehicles sharing common properties such as a models are shown in Table I.
same origin and destination. The work conducted in this
paper presents a first step allowing simulation of wide area simulated entities control strategies
traffic network on the basis of dynamic hybrid modeling,
where the representation associated to a network section can
macro flow of vehicles ramp metering
change at runtime. The proposed approach is implemented in
a simulation platform, called JAM-FREE.
meso group of vehicles variable-message panels
INTRODUCTION
Severe congestion is a daily problem which leads to a con-
micro vehicle vehicle instrumentation
tinuouslygrowthofdirectandindirectcost.Trafficcongestion
which can be recurrent typical to rush hours or non recurrent Fig.1. Hybridtrafficsimulationandcontrolapproach
due to accidents, works, ... represents a major preoccupation
of many transportation institutions and practitioners, calls for
anefficientandintelligentdynamicmanagement.Accordingly, The models presented in Table I share the same limitation:
severalworkswereundertakentostudythetrafficphenomena, connections between levels are fixed a priori and cannot be
to implement effective strategies for an optimal use of the changed at runtime. Therefore, to be able to observe some
existing infrastructure and to minimize the congestion effects emerging phenomena such as congestion formation or to find
as well as a

[SNIPPET TRUNCATED. RETURN TO THE PDF OR .TXT EXTRACTION FOR MORE CONTEXT.]
```

### `burghout_2004_hybrid_microscopic_mesoscopic_thesis.pdf`

- Extraction status: extracted
- Working title from extracted text: Hybrid microscopic-mesoscopic traffic simulation
- PDF source: `burghout_2004_hybrid_microscopic_mesoscopic_thesis.pdf`
- Extracted text: `_extracted/Question3ref/burghout_2004_hybrid_microscopic_mesoscopic_thesis.txt`

```text
===== PAGE 1 =====

Hybrid microscopic-mesoscopic traffic simulation
Wilco Burghout
Doctoral Dissertation
Royal Institute of Technology
Stockholm, Sweden 2004

===== PAGE 2 =====

© Wilco Burghout
Royal Institute of Technology
Department of Infrastructure
Division of Transportation & Logistics
Centre for Traffic Simulation
Teknikringen 72
SE-100 44 Stockholm
Sweden
TRITA-INFRA 04-035
ISSN 1651 - 0216
ISRN KTH/INFRA/--04/035--SE
ISBN 91-7323-099-5

===== PAGE 3 =====

Abstract
Traffic simulation is an important tool for modelling the operations of dynamic
traffic systems and helps analyse the causes and potential solutions of traffic
problems such as congestion and traffic safety. Microscopic simulation models
provide a detailed representation of the traffic process, which makes them most
suitable for evaluation of complicated traffic facilities and Intelligent Transportation
Systems that often consist of complex traffic management, safety and information
systems. Macroscopic and mesoscopic models on the other hand, capture traffic
dynamics in lesser detail, but are faster and easier to apply and calibrate than
microscopic models. Therefore they are most suitable for modelling large networks,
while microscopic models are usually applied to smaller areas.
The objective of this thesis is to combine the strengths of both modelling
approaches and diminish their individual weaknesses by constructing a hybrid
mesoscopic-microscopic model that applies microscopic simulation to areas of
specific interest, while simulating a surrounding network in lesser detail with a
mesoscopic model.
Earlier attempts at hybrid modelling have concentrated on integrating macroscopic
and microscopic models and have proved difficult due to the large difference
between the continuous-flow representation of traffic in macroscopic models and
the detailed vehicle-and driver-behaviour represented in microscopic models. These
problems are solved in this thesis by developing a mesoscopic vehicle-based and
event-based model that avoids the (dis)aggregation problems of traffic flows at the
inter-model boundaries. In addition, this thesis focuses on the general problems of
consistency across the entire hybrid model.
The requirements are identified that are important for a hybrid model to be
consistent across the models at different levels of detail. These requirements vary
from network and route-choice consistency to consistency of traffic dynamics
across the boundaries of the micro- and mesoscopic submodels. An integration
framework is proposed that satisfies these requirements. This integration

===== PAGE 4 =====

framework has been implemented in a prototype hybrid model, MiMe, which is
used to demonstrate the correctness of the solutions to the various integration
issues. The hybrid model integrates MITSIMLab, a microscopic traffic simulation
model, and Mezzo, the newly developed mesoscopic model. Both the hybrid model
and the new Mezzo model are applied in a number of case studies, including a
network in the North of Stockholm, which show their validity and applicability.
The results are promising and support both the proposed integration architecture
and the importance of integrating microscopic and mesoscopic models.
Keywords: Traffic simulation, Traffic models, Mesoscopic, Microscopic, Hybrid

===== PAGE 5 =====

Acknowledgements
This is the place to admit that while there appears only one author on the cover,
this work, just as any other, is a product of the

[SNIPPET TRUNCATED. RETURN TO THE PDF OR .TXT EXTRACTION FOR MORE CONTEXT.]
```

### `burghout_2005_hybrid_mesoscopic_microscopic_traffic_simulation.pdf`

- Extraction status: extracted
- Working title from extracted text: Wilco Burghout, Haris Koutsopoulos, Ingmar Andréasson 1
- PDF source: `burghout_2005_hybrid_mesoscopic_microscopic_traffic_simulation.pdf`
- Extracted text: `_extracted/Question3ref/burghout_2005_hybrid_mesoscopic_microscopic_traffic_simulation.txt`

```text
===== PAGE 1 =====

Wilco Burghout, Haris Koutsopoulos, Ingmar Andréasson 1
Hybrid Mesoscopic-Microscopic Traffic Simulation
Wilco Burghout*1, Haris N. Koutsopoulos2, Ingmar Andréasson1
1 Centre for traffic simulation (CTR), Royal Institute of Technology, SE-10044 Stockholm, Sweden.
{wilco|ingmar}@infra.kth.se
2Department of Civil and Environmental Engineering, Northeastern University
437 Snell Engineering Center, Boston, MA 02115-5000, haris@coe.neu.edu
* Corresponding author: wilco@infra.kth.se
Submitted for publication in Transportation Research Record, March 2005
6092 words (Abstract+Text+References) + 1 table and 4 figures (5 X 250) = 7342 words

===== PAGE 2 =====

Wilco Burghout, Haris Koutsopoulos, Ingmar Andréasson 2
Hybrid Mesoscopic-Microscopic Traffic Simulation
Wilco Burghout, Haris Koutsopoulos, Ingmar Andréasson
Abstract
Traffic simulation is an important tool for modeling the operations of dynamic traffic systems. While microscopic
simulation models provide a detailed representation of the traffic process, macroscopic and mesoscopic models
capture traffic dynamics of large networks, in lesser detail, but without the problems of application and calibration
of microscopic models. In this paper we present a hybrid mesoscopic-microscopic model that applies microscopic
simulation to areas of specific interest, while simulating a large surrounding network in lesser detail with a
mesoscopic model. We identify requirements that are important in order for a hybrid model to be consistent across
the models at different levels of detail. These requirements vary from network and route-choice consistency to
consistency of traffic dynamics at the boundaries of the micro and meso submodels. We propose an integration
framework that satisfies these requirements. A prototype hybrid model is used to demonstrate the application of the
integration framework and the solution of the various integration issues. The hybrid model integrates MITSIMLab,
a microscopic traffic simulation model, and Mezzo, a newly developed mesoscopic model. The hybrid model is
applied in two case studies. The results are very promising and support both the proposed architecture and the
importance of integrating micro and meso models.
Keywords: Traffic simulation, Traffic models, Mesoscopic, Microscopic, Hybrid

===== PAGE 3 =====

Wilco Burghout, Haris Koutsopoulos, Ingmar Andréasson 3
1. INTRODUCTION
Traffic simulation has become very popular for modeling the operations of dynamic traffic systems. Traffic
simulation models are macroscopic, mesoscopic or microscopic. Macroscopic (macro) models (e.g. Strada (1),
Metacor (2)) tend to model traffic as a continuous flow, often using formulations based on hydrodynamic flow
theories. Mesoscopic (meso) models (e.g. DynaMIT (3), DYNASMART (4)) model individual vehicles, but at an
aggregate level, usually by speed-density relationships and queuing theory approaches. Microscopic (micro) models
(e.g. MITSIMLab (5), Vissim (6)) capture the behavior of vehicles and drivers in great detail, including interactions
among vehicles, lane changing, response to incidents, and behavior at merging points. Because of this level of detail
in the representation of traffic dynamics, microscopic models are appropriate for evaluation of ITS systems at the
operational level, since the representation of many dynamic traffic management systems requires such fine-grained
modeling of the traffic process.
However, the application of micro simulation

[SNIPPET TRUNCATED. RETURN TO THE PDF OR .TXT EXTRACTION FOR MORE CONTEXT.]
```

### `burghout_2006_discrete_event_mesoscopic_for_hybrid.pdf`

- Extraction status: extracted
- Working title from extracted text: A Discrete-Event Mesoscopic Traffic Simulation Model for Hybrid
- PDF source: `burghout_2006_discrete_event_mesoscopic_for_hybrid.pdf`
- Extracted text: `_extracted/Question3ref/burghout_2006_discrete_event_mesoscopic_for_hybrid.txt`

```text
===== PAGE 1 =====

A Discrete-Event Mesoscopic Traffic Simulation Model for Hybrid
Traffic simulation
Wilco Burghout, Haris N. Koutsopoulos and Ingmar Andreasson
Abstract—The paper presents a mesoscopic traffic simulation speed/density relationships on the link, use stochastic queue-
model, particularly suited for the development of integrated servers at the nodes to account for delays caused by traffic
meso-micro traffic simulation models. The model combines a signals and the interaction with traffic from other directions
number of the recent advances in simulation modeling, such as
(e.g. FASTLANE [3]). DYNAMEQ [5-7] works in a similar
discrete-event time resolution and combined queue-server and
fashion, but the link dynamics of vehicles are captured by a
speed-density modeling, with a number of new features such as
simplified car-following relationship. Lanes are explicitly
the ability to integrate with microscopic models to create hybrid
traffic simulation. The ability to integrate with microscopic represented including lane-change operations.
models extends the area of use to include evaluation of ITS Macroscopic models [22, 23] are mostly used for planning
systems, which often require the detailed modeling of vehicles in applications, and operations control design, involving large
areas of interest, combined with a more general modeling of
networks and long time periods. Microscopic models (e.g.
large surrounding areas to capture network effects of local
[14]) are suited to model vehicle interactions at the high
phenomena. The paper discusses the structure of the model,
level of detail required for the evaluation of many Intelligent
presents a framework for integration with micro models, and
illustrates its validity through a case study with a congested Transportation Systems (ITS), but are limited to small areas,
network north of Stockholm. It also compares its performance due to the large amounts of input data, and the extensive
with a hybrid model applied to the same network. calibration requirements.
Mesoscopic models provide a middle ground with their
I. INTRODUCTION
ability to model large networks with limited network coding
T R AFFIC simulation models are usually macroscopic, and calibration effort, while providing a better representation
mesoscopic or microscopic. Macroscopic models of the traffic dynamics and individual travel behavior than
represent traffic as a (indivisible) flow, whereas microscopic their macroscopic counterparts. Mesoscopic models are
models describe the behavior of individual drivers, their used for both planning, and real time (online) operations.
vehicles and their interactions. Mesoscopic models maintain They are more flexible than macroscopic models for
individual vehicle representation but with a more aggregate modeling important elements, such as travel behavior (i.e.
representation of traffic dynamics. route choice). However, they are still limited in their ability
A number of mesoscopic models exists in the literature. to represent detailed traffic operations, especially as related
CONTRAM [1], for instance, represents the network by to ITS systems.
nodes and links, and the vehicles on those links are grouped In response to the need for models that can capture both
into packets that travel from origins to destinations (although local traffic phenomena in detail, and effects on a larger
packets may consist of only one vehicle). DYNAMIT [2] surrounding network, hybrid models have recently

[SNIPPET TRUNCATED. RETURN TO THE PDF OR .TXT EXTRACTION FOR MORE CONTEXT.]
```

### `burghout_koutsopoulos_2006_vehicle_loading_boundaries.pdf`

- Extraction status: extracted
- Working title from extracted text: Hybrid Traffic Simulation Models:
- PDF source: `burghout_koutsopoulos_2006_vehicle_loading_boundaries.pdf`
- Extracted text: `_extracted/Question3ref/burghout_koutsopoulos_2006_vehicle_loading_boundaries.txt`

```text
===== PAGE 1 =====

Hybrid Traffic Simulation Models:
Vehicle Loading at Meso – Micro Boundaries
Wilco Burghout
wilco@ctr.kth.se
Assistant Professor
Center for Traffic Simulation (CTR)
Royal Institute of Technology, SE-100 44 Stockholm, Sweden
Haris N. Koutsopoulos
haris@coe.neu.edu
Associate Professor
Department of Civil and Environmental Engineering
Northeastern University, Boston, MA 02115-5000, USA

===== PAGE 2 =====

Hybrid Traffic Simulation Models:
Vehicle Loading at Meso – Micro Boundaries
Wilco Burghout1
Haris N. Koutsopoulos2
Abstract. Traffic simulation models, especially microscopic, are becoming popular and used to
address a wide range of problems, from planning to operations. However, for applications with large
scale networks microscopic models are not practical because of input data and calibration
requirements. Hybrid models, that combine simulation models at different levels of detail have the
potential to address these practical issues. The paper presents a framework for implementing meso-
micro hybrid models that facilitate consistent representation of traffic dynamics. Furthermore, the
paper examines in detail an important element that impacts the consistent representation of traffic
dynamics, the loading of vehicles from the meso to the micro model. A new loading method is
presented that shows superior performance compared to existing approaches. The method is useful
not only in the context of hybrid models, but also for microscopic models on their own. A case study
illustrates the importance of the method in improving the fidelity of both hybrid and pure microscopic
models.
1. Introduction
While microscopic traffic simulation is becoming ever more popular, especially in the evaluation of
advanced traffic management systems and ITS, the amount of effort needed for model calibration and
the preparation of input data often inhibits its use on large networks. Recently, hybrid mesoscopic-
microscopic models have appeared (Burghout, 2004; Burghout, Koutsopoulos, and Andreasson, 2005;
Yang and Morgan, 2006; Shi and Ziliaskopoulos, 2006) that allow for detailed microscopic simulation
of specific areas of interest, while simulating the remaining areas in lesser detail on mesoscopic level.
Since mesoscopic simulation has a more aggregate representation of the roadway and the vehicle
interactions, it requires much less effort in calibration and preparation of input data (especially coding
of the road network). In addition, a number of hybrid macroscopic-microscopic models have appeared
recently (Bourrel and Lesort, 2003; Magne, Rabut and Gabard, 2000; Poschinger, Kates and Meier,
2000; Espie, Gattuso and Galante, 2006; Mammar, Lebaque and Haj-Salem, 2006).
Development and implementation of hybrid models that combine traffic simulation models at different
levels of detail require the resolution of a number of issues, some of them related to the interaction of
the two models at their boundaries. Important among those issues is the consistency in traffic
dynamics at the meso and micro network boundaries.
The objective of this paper is twofold: to a) present a general framework for the implementation of
hybrid simulation models that satisfies the various integration requirements; and b) discuss in detail
one important aspect that has serious implications for the validity of simulation models in general and
hybrid models in particular. This aspect is the mechanism used to load vehicles arriving from the
mesoscopic area into the

[SNIPPET TRUNCATED. RETURN TO THE PDF OR .TXT EXTRACTION FOR MORE CONTEXT.]
```

### `CellTransmissionModelDanganzo2025.pdf`

- Extraction status: extracted
- Working title from extracted text: Transpn. Res.-B. Vol. 298. No. 2. pp. 79-93, 1995
- PDF source: `CellTransmissionModelDanganzo2025.pdf`
- Extracted text: `_extracted/Question3ref/CellTransmissionModelDanganzo2025.txt`

```text
===== PAGE 1 =====

Transpn. Res.-B. Vol. 298. No. 2. pp. 79-93, 1995
Coowieht 0 1995 ElsevierS cience Ltd
Pergamon Printed’& &eat Britain. All rights reserved
0191-2615195 $9.50 + .OO
0191-2615(94)00022-O
THE CELL TRANSMISSION MODEL,
PART II: NETWORK TRAFFIC
CARLOS F. DAGANZO
Department of Civil Engineering and Institute of Transportation Studies,
University of California, Berkeley, CA 94720, U.S.A.
(Received 24 November 1993)
Abstract-This article shows how the evolution of multi-commodity traffic flows over complex
networks can be predicted over time, based on a simple macroscopic computer representation of
traffic flow that is consistent with the kinematic wave theory under all traffic conditions. The
method does not use ad hoc procedures to treat special situations. After a brief review of the basic
model for one link, the article describes how three-legged junctions can be modeled. It then
introduces a numerical procedure for networks, assuming that a time-varying origin-destination
(O-D) table is given and that the proportion of turns at every junction is known. These assump-
tions are reasonable for numerical analysis of disaster evacuation plans. The results are then
extended to the case where, instead of the turning proportions, the best routes to each destination
from every junction are known at all times. For technical reasons explained in the text, the
procedure is more complicated in this case, requiring more computer memory and more time for
execution. The effort is estimated to be about an order of magnitude greater than for the static
traffic assignment problem on a network of the same size. The procedure is ideally suited for
parallel computing. It is hoped that the results in the article will lead to more realistic models of
freeway flow, disaster evacuations and dynamic traffic assignment for the evening commute.
1. INTRODUCTION
Despite all the attention that dynamic assignment is receiving today in the literature
(Transportation Research, 1991), most of the research efforts in that field seem to be
directed at improving the route choice mechanisms of drivers with various levels of
information and/or at refining the algorithms. Little work is aimed at enhancing the
realism of the basic building blocks of the predictions- the underlying traffic perfor-
mance models. A recent overview of this literature is given in Ran ( 1993) and Janson and
Robles (1993).
Some of the works reviewed in these references attempt to predict some form of
system equilibrium, assuming that the travel time on an arc of the network can be
expressed as an increasing function of the flow on the arc at the time. This, however, is a
futile exercise for a rather obvious reason: If a bottleneck causes a queue spanning a
whole arc to form, the result would be high travel times and low flow-an outcome that
cannot be predicted with a simple flow-time relationship.
In an attempt to correct this deficiency, some of the more advanced models define
the arc travel time for an entering vehicle as a function of the current arc occupancy as
well as entering and exiting flows. Unfortunately, such a generalization does not have the
desired effect: Absurd results are still obtained whenever a vehicle’s travel time on an arc
is allowed to depend in any way on the arc entry and/or exit flow at the time of entry
(Daganzo, 1993~). ’
The preceding comments are not meant to be critical of specific works. 2 Rather, they
are meant to illustrate that the state of the art

[SNIPPET TRUNCATED. RETURN TO THE PDF OR .TXT EXTRACTION FOR MORE CONTEXT.]
```

### `Daganzo-cellTransmissionModel.pdf`

- Extraction status: extracted
- Working title from extracted text: Transpn. Res.-B. Vol. 28B, No. 4, pp. 269-287, 1994
- PDF source: `Daganzo-cellTransmissionModel.pdf`
- Extracted text: `_extracted/Question3ref/Daganzo-cellTransmissionModel.txt`

```text
===== PAGE 1 =====

Transpn. Res.-B. Vol. 28B, No. 4, pp. 269-287, 1994
Copyright 0 1994 Elsevier Science Ltd
Pergamon Printed in the UK. All rights reserved
0191-2615194 $6.00 + .OO
0191-2615(93)E0002-3
THE CELL TRANSMISSION MODEL: A DYNAMIC
REPRESENTATION OF HIGHWAY TRAFFIC
CONSISTENT WITH THE HYDRODYNAMIC THEORY
CARLOS F. DAGANZO
Department of Civil Engineering and Institute of Transportation Studies,
University of California, Berkeley CA 94720, U.S.A.
(Received 23 October 1992; in revisedform 13 July 1993)
Abstract-This paper presents a simple representation of traffic on a highway with a single
entrance and exit. The representation can be used to predict traffic’s evolution over time and
space, including transient phenomena such as the building, propagation, and dissipation of
queues. The easy-to-solve difference equations used to predict traffic’s evolution are shown to be
the discrete analog of the differential equations arising from a special case of the hydrodynamic
model of traffic flow. The proposed method automatically generates appropriate changes in
density at locations where the hydrodynamic theory would call for a shockwave; i.e., a jump in
density such as those typically seen at the end of every queue. The complex side calculations
required by classical methods to keep track of shockwaves are thus eliminated. The paper also
shows how the equations can mimic the real-life development of stop-and-go traffic within moving
queues.
1. INTRODUCTION
Accurate descriptions of highway traffic flow over transportation networks, whether at
the planning or operations level, must recognize that the vehicles traveling on any section
of the network must be bound for specific destinations.
Static traffic assignment models used for transportation planning (see Sheffi, 1985,
for example) achieve this goal by describing the flow on a link of the network by its
components by final destination; e.g., by specifying a variable yid that represents the
amount of flow on link i that is ultimately bound for destination d. Unfortunately, this is
much more difficult to do for dynamic network flow problems (with time-dependent
origin-destination (O-D) flows) because the functional dependence of the link flows at
time t, yid(f), on the collection of all past flows is quite complex. This problem manifests
itself both at the planning level, where networks are quite complex, and at the operations
level, where networks are simpler, but more detail is sought about the system’s evolution.
Although dynamic traffic assignment models -planning level models involving large
networks- typically recognize that traffic travels to many destinations, the models are
based on simplistic flow relationships that are not perfectly consistent with the conserva-
tion laws of traffic. A planned sequel to this paper will discuss this in more detail.
Traffic operations models can be microscopic or macroscopic. Microscopic simula-
tions (e.g., Schwerdtfeger, 1984; Cremer and Ludwig, 1986; Nagel and Schreckenberg,
1992) assume that the behavior of an individual vehicle is a function of the traffic condi-
tions in its environment. Although microscopic simulations usually keep track of each
vehicle’s destination, their assumptions are difficult to validate because humans’ behavior
in real traffic (not in contrived “car-following” experiments) is difficult to observe and
measure. This is unfortunate because for a simulation to work the microscopic details
have to be just right.

[SNIPPET TRUNCATED. RETURN TO THE PDF OR .TXT EXTRACTION FOR MORE CONTEXT.]
```

### `gomes_2020_open_traffic_models_hybrid_simulation.pdf`

- Extraction status: extracted
- Working title from extracted text: Open Traffic Models - A framework for hybrid
- PDF source: `gomes_2020_open_traffic_models_hybrid_simulation.pdf`
- Extracted text: `_extracted/Question3ref/gomes_2020_open_traffic_models_hybrid_simulation.txt`

```text
===== PAGE 1 =====

Open Traffic Models - A framework for hybrid
simulation of transportation networks
Gabriel Gomes
August 13, 2019
Abstract
This paper introduces a new approach to hybrid traffic modeling, along with its
implementation in software. The software allows modelers to assign traffic models to
individuallinksinanetwork. Eachmodelimplementsaseriesofmethods, referedtoas
the modeling interface. These methods are used by the program to exchange informa-
tion between adjacent models. Traffic controllers are implemented in a similar manner.
The paper outlines the important components of the method: the network description,
the description of demands, and the modeling and control interfaces. We include tests
demonstratingthepropagationofcongestionbetweenpairsofmacroscpoic,mesoscopic,
andmicroscopicmodels. OpenTrafficModelsisanopensourceimplementationofthese
concepts, and is available at https://github.com/ggomes/otm-sim.
1 Introduction
Simulation tools are an integral part of transportation planning and research. As described
by Lieberman (2004), the history of traffic simulators stretches back to the 1950s, and has
proceeded alongside developments in the theory of traffic. This trend has continued to the
present day: about half of the papers in the Transportation Research Board’s 2018 issue on
Intelligent Transportation Systems involve a simulation model. Similar numbers apply to
recent issues of the IEEE Transactions on Intelligent Transportation Systems.
Transportation models can be classified broadly into macroscopic, mesoscopic, and mi-
croscopic models. We adopt here the definitions of van Wageningen-Kessels et al. (2015) for
these terms. Macropscopic models do not distinguish individual vehicles, but instead view
traffic as a coninuum. This approach originates with the work of Lighthill and Whitham
(1955) and Richards (1956), who coupled the “fundamental diagram” of Greenshields (1935)
with the law of conservation of vehicles. In contrast, mesoscopic and microscopic models are
vehicle-based. Microscopic models compute vehicle trajectories based on car-following rules.
Most models in this category use ordinary differential equations to represent the accelera-
tions of a vehicle as a function of the state of its neighbors. The car-following approach is one
of the oldest in transportation modeling, dating back to the work of Chandler et al. (1958).
1
9102
guA
21
]SM.sc[
1v90040.8091:viXra

===== PAGE 2 =====

It is used today in most micrscopic simulation software, including SUMO (Krajzewicz et al.,
2006), Aimsun (2019), and CORSIM (2005). Another sub-category of micrscopic models
are those based on cellular automata. Here space is considered as fundamentally discrete,
and the neighborhood of a vehicle consists only of its neighboring cells. These models were
introduced to transportation by Nagel and Schreckenberg (1992). Their compatibility with
image processing algorithms has lead to extremely fast implementations, such as that of
Korˇcek et al. (2011). Mesoscopic models also distinguish individual vehicles, however their
movements depend on aggregate quantities such as capacity and jam density, in addition
to the states of nearby vehicles. Queueing models fall into this category (Zhou and Taylor,
2014).
It has long been recognized that none of these model types is superior to all others, but
instead each has adomain of application (Bourrel and Lesort (2003); Burghout et al. (2005)).
For example, models that stem from the

[SNIPPET TRUNCATED. RETURN TO THE PDF OR .TXT EXTRACTION FOR MORE CONTEXT.]
```

### `kesting_2007_mobil_lane_changing.pdf`

- Extraction status: extracted
- Working title from extracted text: volume is high) (9). In addition, drivers’ lane-changing behavior has
- PDF source: `kesting_2007_mobil_lane_changing.pdf`
- Extracted text: `_extracted/Question3ref/kesting_2007_mobil_lane_changing.txt`

```text
===== PAGE 1 =====

volume is high) (9). In addition, drivers’ lane-changing behavior has
a direct inﬂuence on traffic safety.
Despite its great signiﬁcance, lane changing has not been studied
nearly as extensively as longitudinal acceleration and deceleration
behavior. One reason is the scarcity of reliable data ( 10, 11). To
measure lane changes, cross-sectional data from detectors are not
sufficient and therefore only a few empirical studies about lane-
changing rates as a function of traffic ﬂow or density are available.
Sparmann (12) investigated lane-changing rates on a German two-
lane autobahn. Data for a British motorway were presented by Yousif
and Hunt (13). Recent progress in video tracking methods, however,
allows for a collection of high-quality trajectory data from aerial
observations (14, 15). These two-dimensional data will become more
and more available in the future and will allow for a more profound
understanding of the microscopic lane-changing processes.
The modeling of lane changes is typically considered a multistep
process. On a strategic level, the driver knows about his or her route
in a network, which influences the lane choice, for example, with
regard to lane blockages, on-ramps, off-ramps, or other mandatory
merges (16). In the tactical stage, an intended lane change is prepared
and initiated by advance acceleration or deceleration by the driver
and possibly by cooperation of drivers in the target lane (4). Finally,
in the operational stage, one determines if an immediate lane change
is both safe and desirable (17 ). This choice is typically modeled by
the use of gap-acceptance models, in which drivers compare the avail-
able gaps to the smallest acceptable gap, or the critical gap. Critical
gaps depend in general on the relative speed of the subject vehicle
with respect to those of the lead and the lag vehicles in the adjacent
lane and on the type of lane change (18). Most lane-changing mod-
els in the literature classify lane changes as either mandatory or
discretionary (17–22). Although mandatory changes are performed
for strategic reasons, the driver’s motivation for discretionary lane
changes is a perceived improvement of the driving conditions in the
target lane compared with the actual situation.
A lane-changing model for microscopic car-following models is
presented that describes the rational decision to change lanes and
therefore deals only with the operational decision process. When a
lane change is considered, it is assumed that a driver makes a trade-off
between the expected own advantage and the disadvantage imposed
on other drivers. In particular, the current model includes the follower
in the target lane in the decision process. For a driver considering
a lane change, the subjective utility of a change increases with the
gap to the new leader in the target lane. However, if the velocity of
this leader is lower, it may be favorable to stay in the present lane
despite the smaller gap. A criterion for the utility including both sit-
uations is the difference in the accelerations after and before the lane
change. In this work, therefore, it is proposed that the utility function
be consideration of the difference in vehicle accelerations (or deceler-
ations) after a lane change, calculated with an underlying microscopic
Institute for Transport and Economics, Technische Universität Dresden, Andreas-
Schubert-Strasse 23, D-01062 Dresden, Germany. Corresponding author: 
A. Kesting,

[SNIPPET TRUNCATED. RETURN TO THE PDF OR .TXT EXTRACTION FOR MORE CONTEXT.]
```

### `lighthill_whitham_1955_kinematic_waves_traffic.pdf`

- Extraction status: extracted
- Working title from extracted text: KINEMATIC WAVES. II
- PDF source: `lighthill_whitham_1955_kinematic_waves_traffic.pdf`
- Extracted text: `_extracted/Question3ref/lighthill_whitham_1955_kinematic_waves_traffic.txt`

```text
===== PAGE 1 =====

I. 
) 
KINEMATIC WAVES. II 
PART II 
On kinematic waves: 
II. A theory of traffic flow on long crowded roads 
BY M. J. LIGHTHILL, F.R.S. AND G. B. WHITHAM 
(Department of Mathematics, UniverS'ity of Manchester) 
(Received 15 November 1954-Read 17 March 1955) 
This paper uses the method of kinematic waves, developed in pa.rt I, but may be read 
independently. A functional relationship between flow and concentration for tre.ffic on 
crowded arterial roads has been postulated for some time, and hes experimental be.ckllig (§ 2). 
From this a theory of the propagation of changes in traffic distribution along these rol!ds may 
be deduced (§§2, 3). The theory is applied (§4) to the problem of estimating how a 'hump', 
or region of increased concentration, will move along a crowded main road. It iii suggeeted 
that it will move slightly slower than the mean vehicle speed, and that vehicles paaaing 
through it will have to reduce speed rather suddenly (a.ta 'shook wave') on entering it, but 
can incr0886 speed a.gain only very gradually as they leave it. The hump gradually spreads 
out along the road, and the time sea.le of this proceBB is estimated. The behaviour of such 
a bump on entering a bottleneck, which is too narrow to admit the increased flow, is studied 
(§5), and methods are obtained for estimating the extent and duration of the resulting 
bold-up. 
The theory is applicable principally to tre.ffic behaviour over a. long stretch ofroa.d, but the 
paper concludes (§ 6) with a diecUBBion of its relevance to problems of flow near junctioll8, 
including a discussion of the starting flow at a. controlled junction. 
In the introductory sections 1 and 2, we have included some elementary material on the 
quantitative study of traffic flow for the benefit of scientific readers unfamiliar with the 
subject. 
1. INTRODUCTION 
A new problem, which has arisen in the twentieth century, is how to organize road 
traffic so that the full benefits of our increased mobility can be enjoyed at the 
lowest cost in human life and capital. The problem has many sides--constructional, 
legal, educational, administrative. The early lines of attack were largely intuitive. 
But, more recently, there has been an increasing tendency to adopt scientific 
methods, and try to assess the relative merits of Qifferent lines of attack by means 
of controlled experiments. This has been done both by the various authorities 
responsible for road lay-out, administration and propaganda, and also, more 
comprehensively, by organizations like the Road Research Laboratory in Grel\t 
Britain, and the Bureau of Public Roads (formerly the Public Roads Administra­
tion) in the U.S.A. (Glanville 1953; Smeed 1952). 
An important branch of the subject, with repercussions on all the other branches, 
is the quantitative study of traffic flow. An account of the experimental methods 
employed in this field has been given by the head of the traffic-flow section at the 
Road Research Laboratory (Charlesworth 1950). They include methods for 
measuring the means and standard deviations of vehicle speed at a point or journey 
time over a stretch of road, and for measuring the flow (number of vehicles passing 
a. given point per unit of tinie). Attempts to correlate these variables for roads of 
particular mean width, mean curvature, etc., are made. Also, traffic performance

===== PAGE 2 =====

8 M . J. LIGHTHILL AND G. B. WHITHAM 
is studied before and after some change in

[SNIPPET TRUNCATED. RETURN TO THE PDF OR .TXT EXTRACTION FOR MORE CONTEXT.]
```

### `Mesotrafficpaper2018.pdf`

- Extraction status: extracted
- Working title from extracted text: Availableonlineatwww.sciencedirect.com
- PDF source: `Mesotrafficpaper2018.pdf`
- Extracted text: `_extracted/Question3ref/Mesotrafficpaper2018.txt`

```text
===== PAGE 1 =====

Availableonlineatwww.sciencedirect.com
Availableonlineatwww.sciencedirect.com
Available online at www.sciencedirect.com
2 Authors/ProcediaComputerScience00(2018)000–000
ScienceDirect realistic. For example, estimating fuel consumption requires higher level of detail in vehicle-level trajectories such
ProcediaComputerScience00(2018)000–000 asspeed,acceleration,deceleration,idlingetc..Inthatregard,microscopicsimulationwouldbeanaturalchoice.On
ProcPerdoicae dCioamCpoumtepru StecrieSncciee n1c5e10 (020(21091) 88)5080–08–60300 www.elsevier.com/locate/procedia theotherhand,thesimulationmodelshouldbecomputationallyefficientinordertobeapplicableinlargenetworks.
www.elsevier.com/locate/procedia
Inthatcasemacroscopicmodelswouldbepreferredovermicroscopicmodelsasthelatterrequiresdetailednetwork
The7thInternationalWorkshoponAgent-basedMobility,TrafficandTransportationModels, representation,variousparameterstobecalibratedandiscomputationallyintensive.
The7thInternationalWorkshoponAgent-basedMobility,TrafficandTransportationModels, Here we present the traffic simulation model developed for the POLARIS tool. The model is mesoscopic in an
MethodologiesandApplications(ABMTrans2018)
MethodologiesandApplications(ABMTrans2018) attempt to obtain enough for applications that requires microscopic data while still inheriting the computational ef-
Mesoscopic Traffic Flow Model for Agent-Based Simulation ficiencyofmacroscopictrafficflowmodels.Themodelismacroscopicatthelink-levelwithdynamicsbasedonthe
Mesoscopic Traffic Flow Model for Agent-Based Simulation
NewellsModel[7]andmicroscopicatthenodelevelwithflowcomputationbasedontheunderlyingintersectiontype.
Felipe de Souzaa, , Omer Verbasa, Joshua Aulda Thispaperisorganizedasfollows.InSection2wepresentabriefreviewofrelatedliteratureandPOLARIS.In
Felipe de Souzaa,∗ ∗ , Omer Verbasa, Joshua Aulda Section 3 the POLARIS traffic flow model is presented. In Section 4 we present the results of the POLARIS traffic
aArgonneNationalLaboratory,Lemont,IL,60439,UnitedStates
simulation.Finally,westateourconclusionsandfutureworkinSection5.
aArgonneNationalLaboratory,Lemont,IL,60439,UnitedStates
2. RelatedLiterature
Abstract
Abstract
Trafficsimulationisakeyelementofagentbasedmodelsasthevariousagentsdecisionsimpactandareimpactedbytherealized
Recentlyseveralpackageshavebeendevelopedforintegratedagent-basedmodeling.OPUS[8]isaPython-based
tTrraav ffi elctsimimeuslaatniodndieslaaykseyineltehmeetrnatffiofcangeetnwtobraks.eHdemreodwelesparsetsheentvathrieoumseasgoesnctospdicectirsaioffincsiflmopwacmtoanddelairmepimlepmaecntetdedbyinthPeOrLeaAliRzeISd
ttrraavnesplotirmtaetisonansydstdeemlasyssiminultahteort.rTaffi hecmnoedtweloirskm. eHseorsecowpeicpinreasnenatttethmepmtteosoobsctaoipnicentorau ffi ghcfoflroawppmliocadteilonimstphlaetmreeqnuteirdesinmPicOroLsAcoRpIiSc open-sourcelibrarydesignedtobeeasilyextensibleallowingresearcherstointegratespecificmodelsfortheirneeds
dtraatnaspwohritlaetisotnillsyinstheemristisnigmtuhleatcoor.mTphuetamtioodnealliesffimceiseonsccyopoifcminaacnroastctoempipctttroaffiobctaflionwenmouogdhelfso.rTahpeplmicoadtieolnisstmhaatcrreoqsuciorpesicmaitctrhoescloinpkic- suchasdemandmodelinganddynamictrafficassignment.TRANSIMS[6]isanearlyattempttointegratedifferent
ldeavtealwwhiitlhedstyilnlaimnhicesritbinasgedtheoncotmhepuNtaetwioenllaslMeffi ocdieelncayndofmmicarcorsocsocpoipcicattrtah ffi ecnflodoewlmevoedl.elWs.eTphreemseondtelliniks-mleavcerloasncodpnicetawtothrke-lleinvke-l

[SNIPPET TRUNCATED. RETURN TO THE PDF OR .TXT EXTRACTION FOR MORE CONTEXT.]
```

### `NewellKynematicWave.pdf`

- Extraction status: extracted
- Working title from extracted text: Tronspn. Res:B. Vol. 27B, No. 4, pp. 281-287, 1993 0191-2615/93 f6.00 + .OO
- PDF source: `NewellKynematicWave.pdf`
- Extracted text: `_extracted/Question3ref/NewellKynematicWave.txt`

```text
===== PAGE 1 =====

Tronspn. Res:B. Vol. 27B, No. 4, pp. 281-287, 1993 0191-2615/93 f6.00 + .OO
Printed in Great Britain. 0 1993 Pergamon Press Ltd.
A SIMPLIFIED THEORY OF
KINEMATIC WAVES IN HIGHWAY TRAFFIC,
PART I: GENERAL THEORY
G. F. NEWELL
Institute of Transportation Studies, University of California,
Berkeley, CA 94720, U.S.A.
(Received 9 March 1992)
Abstract - In the theory of “kinematic waves,” as described originally by Lighthill and Whitham
in 1955, the evaluation of the shock path is typically rather tedious. Instead of using this theory
to evaluate flows or densities, one can use it to evaluate the cumulative flow A(x, t) past any
point x by time r. It is shown here how a formal solution for A(x, 1) can be evaluated directly
from boundary or initial conditions without evaluation at intermediate times and positions. If
there are shocks, however, this solution will be multiple-valued. The correct solution, which is the
lower envelope of all such formal solutions, will automatically have discontinuities in slope de-
scribing the passage of a shock. To evaluate A(x, t) at any particular location x, it is not necessary
to follow the actual path of the shock. The solution can be evaluated directly in terms of the
boundary data by either graphical or numerical techniques.
1. INTRODUCTION
In a famous paper, “On Kinematic Waves,” Lighthill and Whitham (1955) described a
theory of one-dimensional wave motion which could be applied to certain types of fluid
motion or to highway traffic flow. Richards (1956) independently proposed a similar
theory for traffic flow. The key postulate of the (L-W-R) theory was that there exists
some functional relation between the flow q and the density (concentration) k. The flow
is defined for fluids as the rate at which mass passes some point and for traffic as the rate
at which vehicles pass some point. The density is defined for fluids as the mass per unit
length of channel and for traffic as the number of vehicles per unit length of road. This
relation between q and k might vary with location x but not with time t, i.e.,
k (x, t) = k*(q (x, t), x) (1)
or
q (x, t) = q* (k (x, t), xl
for some given functions k* or q*.
The conservation equation (equation of continuity) implies that (with no entering or
exiting traffic)
6% (x, t) + a4 (x9 t) =
() (2)
at ax *
which with eqn (1) gives a partial differential equation for q(x, t),
aq
(x9 t) + a4 (x, 0 =
w (Q 0, t)J) o (3)
at ax
*
with
w (4, X) = ak* (4, x)/aq.
The (l/w) is called the “wave velocity,” but in most applications it is more meaningful to
describe the time to travel a given distance than the distance traveled in a given time. We
281

===== PAGE 2 =====

282 t:. F. NEWELL
refer to w as the “pace” of the wave for lack of any other common word for the reciprocal
of the velocity.
The usual method for solving eqn (3) is to note that eqn (3) implies that q(x, I)
remains constant along a characteristic curve (wave) for which
dt/dx = w (q, x). (4)
Thus from any initial or boundary conditions which specify a value of q at some point
x0,&, the same value of q applies at all points along the characteristic curve passing
through x,,t,, namely the curve
5I
(x) = 1, + w (4 (xo, to), z) dz. (5)
t
x0
We will be concerned here mostly with the special case of a homogeneous channel or
road section for which k*(q, x) = k*(q) is independent of x. In this case w is also constant
along the characteristic curve and the characteristic curve is a straight line.
One of the

[SNIPPET TRUNCATED. RETURN TO THE PDF OR .TXT EXTRACTION FOR MORE CONTEXT.]
```

### `schuhmann_2025_hybrid_large_scale_multimodal.pdf`

- Extraction status: extracted
- Working title from extracted text: smart cities
- PDF source: `schuhmann_2025_hybrid_large_scale_multimodal.pdf`
- Extracted text: `_extracted/Question3ref/schuhmann_2025_hybrid_large_scale_multimodal.txt`

```text
===== PAGE 1 =====

smart cities
Article
Creating and Validating Hybrid Large-Scale, Multi-Modal Traffic
Simulations for Efficient Transport Planning
FabianSchuhmann1,∗ ,NgocAnNguyen2 ,JörgSchweizer2 ,Wei-ChiehHuang3 andMarkusLienkamp1
1 InstituteofAutomotiveTechnology,DepartmentofMobilitySystemsEngineering,TUMSchoolof
EngineeringandDesign,TechnicalUniversityofMunich,85748Garching,Germany
2 DepartmentofCivil,EnvironmentalandMaterialEngineering(DICAM),UniversityofBologna,
40126Bologna,Italy
3 ProfessorshipofTravelBehavior,DepartmentofMobilitySystemsEngineering,TUMSchoolofEngineering
andDesign,TechnicalUniversityofMunich,80333Munich,Germany
* Correspondence:fabian.schuhmann@tum.de
Highlights:
Whatarethemainfindings?
• Withtheproposedhybridtoolchain,aseamlesstransferfromthegloballyassignedmeso-
demandtoasmaller,locallysimulatedmicro-networkhasbeensuccessfullyappliedtoatest
caseofthelargerMunchmetropolitanarea.
• The approach allows link-by-link validation between real measurements, as well as the
meso-andthemicro-simulations;forthetestcase,bothmodelsshowedagoodfitbetween
simulatedandmeasuredtrafficflows,butthemicro-modelshowedmorerealisticresults
with respect to the meso-model when average link speeds from floating car data were
compared.
Whatistheimplicationofthemainfinding?
• Withthepresentedhybridapproach,itwillbecomefeasibletoefficientlymodelandsimulate
large-scale transport scenarios with individual users while enabling a consistent micro-
simulationondedicatedareas,whicharesensitivetotheimplementationofawiderangeof
complextransportservicesandpolicymeasures.
• It is possible to quantify and directly compare the closeness to reality of the meso- and
AcademicEditor:PierluigiSiano
micro-model,whichisusefultodemonstratewhetheramicro-simulationdoesofferadded
Received:23October2024
valueandwhetheritisworththeadditionaleffortswithrespecttothemeso-onlyapproach.
Revised:8December2024
Accepted:9December2024
Published: 24December2024 Abstract:Mobilitydigitaltwins(MDTs),whichutilizemulti-modalmicroscopic(micro)traffic
Citation: Schuhmann,F.;Nguyen, simulationsandanactivity-baseddemandgeneration,areenvisionedasflexibleandreliable
N.A.;Schweizer,J.;Huang,W.-C.; planningtoolsforaddressingtoday’sincreasinglycomplexanddiversetransportscenarios.
Lienkamp,M.CreatingandValidating
Hybridmodelsmaybecomearesource-efficientsolutionforbuildingMDTsbycreatinglarge-
HybridLarge-Scale,Multi-Modal
scale,mesoscopic(meso)trafficsimulations,usingsimplified,queue-basednetwork-linkmodels,
TrafficSimulationsforEfficient
incombinationwithmoredetailedlocalmicro-trafficsimulationsfocusedonareasofinterest.
TransportPlanning.SmartCities2025,
8,2. https://doi.org/10.3390/ Theoverallobjectiveofthispaperistodevelopanefficienttoolchaincapableofautomatically
smartcities8010002 generating,calibrating,andvalidatinghybridscenarios,withthefollowingspecificgoals:(i)an
automatedandseamlessmergeofthemeso-andmicro-networksanddemand;(ii)avalidation
Copyright:©2024bytheauthors.
LicenseeMDPI,Basel,Switzerland. procedurethatincorporatesreal-worlddataintothehybridmodel,enablingthemeso-and
Thisarticleisanopenaccessarticle micro-sub-modelstobevalidatedseparatelyandcomparedtodeterminewhichsimulation,
distributedunderthetermsand

[SNIPPET TRUNCATED. RETURN TO THE PDF OR .TXT EXTRACTION FOR MORE CONTEXT.]
```

### `schweizer_2024_hybridpy_simulation_suite.pdf`

- Extraction status: extracted
- Working title from extracted text: SUMOUserConference2024
- PDF source: `schweizer_2024_hybridpy_simulation_suite.pdf`
- Extracted text: `_extracted/Question3ref/schweizer_2024_hybridpy_simulation_suite.txt`

```text
===== PAGE 1 =====

SUMOUserConference2024
Conferencepaper
https://doi.org/10.52825/scp.v5i.1030
©Authors. ThisworkislicensedunderaCreativeCommonsAttribution3.0DELicense
Published: 17Jul. 2024
hybridPY
The Simulation Suite for Mesoscopic and Microscopic Traffic
Simulations
Jo¨rg Schweizer1 * , Fabian Schuhmann2* ,
and Cristian Poliziani3
1UniversityofBologna,Italy
2TechnischeUniversita¨tMu¨nchen,Germany
3LawrenceBerkeleyNationalLab,USA
Correspondence: Jo¨rgSchweizer,joerg.schweizer@unibo.it
Abstract:Mesoscopic,agent-basedsimulationsefficientlymodelandassessentirere-
gions’ daily activities and travel patterns, exemplified by smaller countries like Switzer-
land. The queue-based simulation represents a compromise between computational
speed on the one hand and the necessity of detailed modeling infrastructure on the
other hand. Thus, mesoscopic simulations enable an efficient and reasonably detailed
analysis of the complex interplay between supply and demand in mobility research.
Conversely, microsimulations excel at reproducing individual speed profiles and be-
havior by modeling the interactions between traffic participants, including pedestrians,
bicycles, and scooters. Although allowing for more detailed system analysis, the down-
side is the high computational burden, which often prevents large-scale microscopic
simulations from running in optimization or calibration loops. hybridPY, an extension of
SUMOPy, aims to close the gap and benefit from both environments. The simulation
suite allowsthe running ofmesoscopic as wellas microscopic traffic simulationsbased
on the core idea: running a microscopic simulation in a smaller dedicated area, using
the routes or mobility plans generated from a larger mesoscopic model. The main fea-
tures of this software are: (i) import, editing and visualization of MATSim and BEAM
CORE networks; (ii) conversion of MATSim plans to SUMO routes or plans within the
SUMO area; (iii) configuring and running of MATSim simulations. The capability of hy-
bridPY is demonstrated by two applications: the simulation of Schwabing, Germany,
based on the MITO MATSim model, and the San Francisco municipality, USA, based
on the mesoscopic BEAM CORE model of the entire San Francisco Bay area. Both
scenarios demonstrate that the hybrid approach results in significant computational
gains with respect to a pure microscopic approach.
Keywords: MATSim, hybrid traffic simualtion
*Theseauthorscontributedequallytothiswork.
39

===== PAGE 2 =====

Schweizeretal. | SUMOConfProc5(2024)”SUMOUserConference2024”
1 Introduction
1.1 Motivation and scope
The population’s daily activities and trips of entire regions or even small countries like
Switzerland [1] can be modeled, simulated and evaluated with multi-modal, meso-
scopic simulations [2]. This queue-based simulation approach appears to represent a
virtuous compromise between computational speed on the one hand and the neces-
sity to model infrastructure and travel times to a sufficient detail on the other hand [3].
Moreover, transport services such as ride pooling or car sharing can be simulated [4]
as the system keeps track of individual persons and vehicles.
Microsimulations are capable of reproducing speed profiles by modeling the inter-
action between different types of vehicles (including bikes and scooters) as well as
between vehicles and persons [5]. In addition, the microsimulator SUMO features a
sub-lane model, where vehicle movements within the lane are simulated, for

[SNIPPET TRUNCATED. RETURN TO THE PDF OR .TXT EXTRACTION FOR MORE CONTEXT.]
```

### `ShockwaveRichard.pdf`

- Extraction status: extracted
- Working title from extracted text: Shock Waves on the Highway
- PDF source: `ShockwaveRichard.pdf`
- Extracted text: `_extracted/Question3ref/ShockwaveRichard.txt`

```text
===== PAGE 1 =====

Shock Waves on the Highway 
Author(s): Paul I. Richards 
Source: Operations Research , Feb., 1956, Vol. 4, No. 1 (Feb., 1956), pp. 42-51 
Published by: INFORMS 
Stable URL: https://www.jstor.org/stable/167515
JSTOR is a not-for-profit service that helps scholars, researchers, and students discover, use, and build upon a wide 
range of content in a trusted digital archive. We use information technology and tools to increase productivity and 
facilitate new forms of scholarship. For more information about JSTOR, please contact support@jstor.org. 
 
Your use of the JSTOR archive indicates your acceptance of the Terms & Conditions of Use, available at 
https://about.jstor.org/terms
INFORMS is collaborating with JSTOR to digitize, preserve and extend access to Operations 
Research
This content downloaded from 
198.137.18.60 on Wed, 20 May 2026 02:06:07 UTC 
All use subject to https://about.jstor.org/terms

===== PAGE 2 =====

SHOCK WAVES ON THE HIGHWAY* 
 PAUL I. RICHARDS 
 Technical Operations, Inc., Arlington, Massachusetts 
 (Received July 1, 1955) 
 A simple theory of traffic flow is developed by replacing individual vehicles 
 with a continuous 'fluid' density and applying an empirical relation between 
 speed and density. Characteristic features of the resulting theory are a 
 simple 'graph-shearing' process for following the development of traffic 
 waves in time and the frequent appearance of shock waves. The effect of 
 a traffic signal on traffic streams is studied and found to exhibit a threshold 
 effect wherein the disturbances are minor for light traffic but suddenly build 
 to large values when a critical density is exceeded. 
 T HE THEORY OF TRAFFIC FLOW to be presented below rests on an 
 idealized picture wherein many individual cars are replaced by a simple 
 continuous distribution. In this way a number of interesting results can 
 be obtained which exhibit many familiar features. 
 It is not claimed, however, that this simple picture is an accurate repre- 
 sentation of the facts under all circumstances. One problem which it will 
 not handle adequately, for example, is the length of waiting-lines at a toll 
 booth. This quantity depends primarily on statistical fluctuations in 
 traffic volume. The simple picture below gives only the result that a 
 waiting-line must have a certain density of cars, but its length is inde- 
 terminate. Thus the simple theory fails in this case, but at least does not 
 give an incorrect answer. 
 Previous analyses of problems of this nature have been reported by 
 Reuschel," 2 Pipes,3' 4 Prager,5 and Newell.6 The first two authors con- 
 sider a number of vehicles each of which is constrained to follow its prede- 
 cessor according to some assumed 'rule' that relates its velocity and ac- 
 celeration to the motions of its predecessors. These relations lead to 
 coupled differential equations that, given initial conditions and the motion 
 of the lead vehicle, can be solved by various methods to predict the motion 
 of all vehicles. In practice, these theories have necessarily been applied 
 to a relatively small number of vehicles. On the other hand, the present 
 theory is valid only for a very large number of vehicles since the basic step 
 of replacing traffic by a continuous distribution-function certainly loses 
 validity for relatively few vehicles. Likewise the present theory, instead 
 of proceeding from detailed assumptions concernring driver-behavior, eln- 


[SNIPPET TRUNCATED. RETURN TO THE PDF OR .TXT EXTRACTION FOR MORE CONTEXT.]
```

### `SimulatingLink.pdf`

- Extraction status: extracted
- Working title from extracted text: Simulation Modelling Practice and Theory 147 (2026) 103252
- PDF source: `SimulatingLink.pdf`
- Extracted text: `_extracted/Question3ref/SimulatingLink.txt`

```text
===== PAGE 1 =====

Simulation Modelling Practice and Theory 147 (2026) 103252
Contents lists available at ScienceDirect
SimulationModellingPracticeandTheory
journal homepage: www.elsevier.com/locate/simpat
Simulatinglink-levelinterruptedflowtrafficdynamicsandthe
comparisonbetweendifferentmodelsforurbanroadnetworks
Ying-Chuan Ni ∗, Anastasios Kouvelas , Michail A. Makridis
Traffic Engineering Group, Institute for Transport Planning and Systems, ETH Zurich,Stefano-Franscini-Platz 5,Zürich,8093,Switzerland
A R T I C L E I N F O A B S T R A C T
Dataset link:LIFT: Link-level Interrupted Flow T Dynamic traffic models that reproduce the pattern of congestion propagation in urban road
raffic dynamics simulation (Original data) networks are essential for the evaluation of traffic management strategies and prediction of
traffic states. Many approaches developed in the past simplified the movement of vehicles and
Keywords:
Backward traveling space
the propagation of traffic waves. The overlooked problems can be particularly influential for
Congestion propagation
the simulation of interrupted flow with traffic signal control due to the frequent accumulation
Interrupted traffic flow and dissipation of the waiting queue on a road link. This study first reviews state-of-the-art
Mesoscopic traffic simulation macroscopic and mesoscopic link-level urban traffic flow models and provides a comparative
Queue spillback discussion of their similarities, differences, and gaps. We then put forward an event-based
Urban road network mesoscopic model to simulate link-level interrupted flow traffic dynamics (LIFT). The model (i)
simulates the transmission of vehicles between links based on the demand and supply of exit
and entry events, (ii) monitors queue spillback through the consideration of backward traveling
spaces, and (iii) adheres to first-in-first-out at intersections for congested situations. Taking
the outcomes generated from microscopic simulation as ground-truth, the case studies show
that LIFT outperforms the other models by accurately capturing the evolution of link densities
and mean path speeds in congested conditions. It is reliable even in complex scenarios with
diverge blocking phenomena and desired speed heterogeneity. Without having to simulate the
interaction between individual vehicles, the model also becomes much more computationally
efficient than microscopic simulation. It can be applied in simulation-based optimization or
control problems which require the consideration of finer-level details that macroscopic models
are unable to offer.
1. Introduction
The spatiotemporal propagation pattern of traffic congestion in an urban road network has been analyzed with various
macroscopic models that contain plenty of simplification and microscopic simulation tools that require intensive computation for
relatively large networks. Fig. 1 summarizes traffic flow modeling approaches at different levels of aggregation and their principles.
Region-level urban traffic modeling based on network fundamental diagrams (NFDs) has become a popular approach in recent
years considering its superior computation efficiency for network-wide traffic management strategies, including perimeter control [1]
and toll pricing [2]. Johari et al. [3] provided an in-depth review on the development of this type of approach. Nevertheless, the
shape of an NFD is subject to changes of the origin–destination (OD) demand and trip pattern within the network [4–6],

[SNIPPET TRUNCATED. RETURN TO THE PDF OR .TXT EXTRACTION FOR MORE CONTEXT.]
```

### `son_2022_differentiable_hybrid_traffic_simulation.pdf`

- Extraction status: extracted
- Working title from extracted text: Differentiable Hybrid Traffic Simulation
- PDF source: `son_2022_differentiable_hybrid_traffic_simulation.pdf`
- Extracted text: `_extracted/Question3ref/son_2022_differentiable_hybrid_traffic_simulation.txt`

```text
===== PAGE 1 =====

Differentiable Hybrid Traffic Simulation
SANGHYUN SON,UniversityofMaryland,CollegePark,USA
YI-LING QIAO,UniversityofMaryland,CollegePark,USA
JASON SEWALL,NVIDIA,USA
MING C. LIN,UniversityofMaryland,CollegePark,USA
Weintroduceanoveldifferentiablehybridtrafficsimulator,whichsimulates
trafficusingahybridmodelofbothmacroscopicandmicroscopicmodelsand
canbedirectlyintegratedintoaneuralnetworkfortrafficcontrolandflow
optimization.Thisisthefirstdifferentiabletrafficsimulatorformacroscopic
andhybridmodelsthatcancomputegradientsfortrafficstatesacrosstime
stepsandinhomogeneouslanes.Tocomputethegradientflowbetweentwo
typesoftrafficmodelsinahybridframework,wepresentanovelintermediate
conversioncomponentthatbridgesthelanesinadifferentiablemanneraswell.
Wealsoshowthatwecanuseanalyticalgradientstoacceleratetheoverall
processandenhancescalability.Thankstothesegradients,oursimulatorcan
providemoreefficientandscalablesolutionsforcomplexlearningandcontrol
problemsposedintrafficengineeringthanotherexistingalgorithms.Referto
https://sites.google.com/umd.edu/diff-hybrid-traffic-simforourproject.
CCSConcepts:•Computingmethodologies→Agent/discretemodels; Fig.1. Trafficsimulationinanurbanenvironment.Trafficsimulation
Continuoussimulation;Continuousmodels;Real-timesimulation;Mul- canbeusedtoanalyzecomplextrafficdynamics.Amongtwopopular
tiscalesystems;Simulationbyanimation;Simulationenvironments. trafficmodels,bysimulatingareasofinterestwiththemicroscopic
oneandusingmacroscopicsimulationelsewhere,wecanreducethe
AdditionalKeyWordsandPhrases:TrafficSimulation,DifferentiablePro-
overallcomputationalcostwithoutcompromisingsignificantdetails.
gramming,MachineLearning
Forinstance,thecenterofintersectioncanbesimulatedwithdiscrete,
ACMReferenceFormat: agent-basedmodelsforhigher-fidelityinter-vehicledynamicinteraction.
SanghyunSon,Yi-LingQiao,JasonSewall,andMingC.Lin.2022.Differ- (ThissceneisrenderedinCARLA[Dosovitskiyetal.2017].)
entiableHybridTrafficSimulation.ACMTrans.Graph.41,6,Article258
Macroscopicmodelsdescribetrafficevolutionasasystemofpartial
(December2022),13pages.https://doi.org/10.1145/3550454.3555492
differentialequations(PDE),andthetrafficstateisrepresentedas
1 INTRODUCTION

[SNIPPET TRUNCATED. RETURN TO THE PDF OR .TXT EXTRACTION FOR MORE CONTEXT.]
```

### `treiber_2000_idm_congested_traffic_states.pdf`

- Extraction status: extracted
- Working title from extracted text: arXiv:cond-mat/0002177v2 [cond-mat.stat-mech] 30 Aug 2000
- PDF source: `treiber_2000_idm_congested_traffic_states.pdf`
- Extracted text: `_extracted/Question3ref/treiber_2000_idm_congested_traffic_states.txt`

```text
===== PAGE 1 =====

arXiv:cond-mat/0002177v2 [cond-mat.stat-mech] 30 Aug 2000
Congested Traﬃc States in Empirical Observations and
Microscopic Simulations
Martin Treiber, Ansgar Hennecke, and Dirk Helbing
II. Institute of Theoretical Physics, University of Stuttg art, Pfaﬀenwaldring 57, D-70550
Stuttgart, Germany
http://www.theo2.physik.uni-stuttgart.de/treiber/, h elbing/
(April 26, 2024)
Abstract
We present data from several German freeways showing diﬀere nt kinds of
congested traﬃc forming near road inhomogeneities, speciﬁ cally lane clos-
ings, intersections, or uphill gradients. The states are lo calized or extended,
homogeneous or oscillating. Combined states are observed a s well, like the
coexistence of moving localized clusters and clusters pinn ed at road inhomo-
geneities, or regions of oscillating congested traﬃc upstr eam of nearly ho-
mogeneous congested traﬃc. The experimental ﬁndings are co nsistent with
a recently proposed theoretical phase diagram for traﬃc nea r on-ramps [D.
Helbing, A. Hennecke, and M. Treiber, Phys. Rev. Lett. 82, 4360 (1999)].
We simulate these situations with a novel continuous micros copic single-lane
model, the “intelligent driver model” (IDM), using the empi rical boundary
conditions. All observations, including the coexistence o f states, are qualita-
tively reproduced by describing inhomogeneities with loca l variations of one
model parameter. We show that the results of the microscopic model can be
understood by formulating the theoretical phase diagram fo r bottlenecks in a
more general way. In particular, a local drop of the road capa city induced by
parameter variations has practically the same eﬀect as an on -ramp.
Typeset using REVTEX
1

===== PAGE 2 =====

I. INTRODUCTION
Recently, there is much interest in the dynamics of traﬃc bre akdowns behind bottle-
necks [1–15]. Measurements of traﬃc breakdowns on various f reeways in the USA [13,1,2,6],
Germany, [3,4,16,17], Holland [18–22], and Korea [12] sugg est that many dynamic aspects
are universal and therefore accessible to a physical descri ption. One common property is the
capacity drop (typically of the order of 20%) associated wit h a breakdown [1,13,6], which
leads to hysteresis eﬀects and is the basis of applications l ike dynamic traﬃc control with
the aim of avoiding the breakdown. In the majority of cases, t raﬃc breaks down upstream
of a bottleneck and the congestion has a stationary donstrea m front at the bottleneck. The
type of bottleneck, e.g., on-ramps [2,13,12,4], lane closi ngs, or uphill gradients [17], seems
not to be of importance. Several types of congested traﬃc hav e been found, among them
extended states with a relatively high traﬃc ﬂow. These stat es, sometimes referred to as
“synchronized traﬃc” [4], can be more or less homogeneously ﬂowing, or show distinct oscil-
lations in the time series of detector data [3]. Very often, t he congested traﬃc ﬂow is, apart
from ﬂuctuations, homogeneous near the bottleneck, but osc illations occur further upstream
[7]. In other cases, one ﬁnds isolated stop-and-go waves tha t propagate in the upstream
direction with a characteristic velocity of about 15 km/h [2 3,16]. Finally, there is also an
observation of a traﬃc breakdown to a pinned localized clust er near an on-ramp [12].
There are several possibilities to delineate traﬃc mathema tically, among them macro-
scopic models describing the dynamics in terms of aggregate quantities like density or ﬂow
[24–27,10], and

[SNIPPET TRUNCATED. RETURN TO THE PDF OR .TXT EXTRACTION FOR MORE CONTEXT.]
```

### `Xiaolin Hu - Dynamic Data-Driven Simulation. Real-Time Data for Dynamic System Analysis and Prediction-World Scientific Publishing (2023).pdf`

- Extraction status: extracted
- Working title from extracted text: Data-Driven
- PDF source: `Xiaolin Hu - Dynamic Data-Driven Simulation. Real-Time Data for Dynamic System Analysis and Prediction-World Scientific Publishing (2023).pdf`
- Extracted text: `_extracted/Question3ref/Xiaolin Hu - Dynamic Data-Driven Simulation. Real-Time Data for Dynamic System Analysis and Prediction-World Scientific Publishing (2023).txt`

```text
===== PAGE 1 =====

===== PAGE 2 =====

Dynamic
Data-Driven
Simulation
Real-Time Data for Dynamic
System Analysis and Prediction

===== PAGE 3 =====

Dynamic
Data-Driven
Simulation
Real-Time Data for Dynamic
System Analysis and Prediction
Xiaolin Hu
Georgia State University, USA
World Scientific
NEW JERSEY • LONDON • SINGAPORE • BEIJING • SHANGHAI • HONG KONG • TAIPEI • CHENNAI • TOKYO

===== PAGE 4 =====

Published by
World Scientific Publishing Co. Pte. Ltd.
5 Toh Tuck Link, Singapore 596224
USA office: 27 Warren Street, Suite 401-402, Hackensack, NJ 07601
UK office: 57 Shelton Street, Covent Garden, London WC2H 9HE
Library of Congress Control Number: 2022053777
British Library Cataloguing-in-Publication Data
A catalogue record for this book is available from the British Library.
DYNAMIC DATA-DRIVEN SIMULATION
Real-Time Data for Dynamic System Analysis and Prediction
Copyright © 2023 by Xiaolin Hu
All rights reserved.
ISBN 978-981-126-717-8 (hardcover)
ISBN 978-981-126-718-5 (ebook for institutions)
ISBN 978-981-126-719-2 (ebook for individuals)
For any available supplementary material, please visit
https://www.worldscientific.com/worldscibooks/10.1142/13166#t=suppl
Desk Editors: Logeshwaran Arumugam/Steven Patt
Typeset by Stallion Press
Email: enquiries@stallionpress.com
Printed in Singapore

===== PAGE 5 =====

To my parents: Zejun Hu and Qiuying Duan

===== PAGE 6 =====

TTTThhhhiiiissss ppppaaaaggggeeee iiiinnnntttteeeennnnttttiiiioooonnnnaaaallllllllyyyy lllleeeefffftttt bbbbllllaaaannnnkkkk

===== PAGE 7 =====

Preface
This book systematically presents dynamic data-driven simulation
(DDDS) as a new simulation paradigm that makes real-time data
and simulation model work together to enable simulation-based pre-
diction/analysis. The term “Dynamic Data Driven Simulation” was
first introduced in my 2011 SCS Modeling and Simulation Magazine
article. Sincethen,ithas becomeclear thatacomprehensivedescrip-
tionofDDDSneedstoanswertwokeyquestions:(1)WhatisDDDS?
(2) How to assimilate real-time data into simulation models? These
two questions drive the main themes of this book.
A central task of DDDS is data assimilation. While data assim-
ilation has been studied in other science fields (e.g., meteorology,
oceanography), it is a new topic for the modeling and simulation
community. A significant effort of this book is thus to describe data
assimilation in a way that connects with the broad audience in the
modelingandsimulation field.Thisbookbridgesthetwo studyareas
of data assimilation and modeling and simulation, which have been
developed largely independently of each other.
This book is the result of more than 10 years of research and
development. I thank the students of the Systems Integrated Mod-
eling and Simulation (SIMS) lab for their contributions to the work
related to this book. The writing of this book is also helped by sev-
eral colleagues. I am grateful to Bernard Zeigler for his guidance and
Hessam Sarjoughian and Ming Xin for their inputs during the book
writing process. A special word of gratitude is due to James Nutaro,
who has read and commented on the book manuscript.
vii

===== PAGE 8 =====

viii Dynamic Data-Driven Simulation
I thank my wife,son,and daughter for their supportandpatience
during the long wait for the completion of this book.
It is hoped that this book will provide a comprehensive presen-
tation of the DDDS topic and that it will serve as a reference and
textbook for students and researchers working on

[SNIPPET TRUNCATED. RETURN TO THE PDF OR .TXT EXTRACTION FOR MORE CONTEXT.]
```
