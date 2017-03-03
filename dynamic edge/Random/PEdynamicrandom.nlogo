extensions [matrix array table nw]
turtles-own [ ispursuer? isevader? idnumber]
links-own [ weight direction]
breed [ pursuers pursuer ]
breed [ evaders evader ]
globals [cost Plist Elist PEtable no-ticks no-sec time-traveled]

to setup
                        ;; clear everything on canvas
  ifelse ( (no-of-nodes > 0) and (no-of-nodes >= (no-of-pursuers + no-of-evaders)) and (no-of-evaders > 0) and (no-of-pursuers > 0))
   [
       clear-all
       set cost matrix:make-constant no-of-nodes no-of-nodes 99999
       set Plist []
       set Elist []
       set PEtable []
       initialize-pursuittable
       set no-ticks 0
       set no-sec 0
       set time-traveled 0
       nw:generate-random turtles links no-of-nodes 0.5 [set shape "circle" set ispursuer? false set isevader? false set idnumber who]
       setup-nodes                     ;; a procedure to set nodes
       setup-edges                     ;; a procedure to set edges
       ask turtles [ set color gray]    ;; paint nodes red
;       ask links with [direction = "to"][set color red]     ;; paint edges white
;       ask links with [direction = "from"][set color green]
;       ask links with [direction = "both"][set color white]
       initialize-pursuers
       initialize-evaders
       printAMatrix
       printPlist
       printElist
       nearestEtoP               ;a function to compute the nearest evader to every pursuer
       ask links [
        ifelse show-weights?
        [ set label weight ]
        [ set label "" ]
       ]
       ask turtles [
       ifelse show-node?
        [ set label who ]
        [ set label "" ]
       ]
       reset-ticks
  ]
  [
    ifelse no-of-evaders = 0
    [
     user-message ("no evaders to pursue")
    ]
    [
      ifelse no-of-pursuers = 0
      [
        user-message ("no pursuers to capture evaders")
      ]
      [
        user-message ("not enough nodes for pursuers and evaders")
      ]
    ]
  ]

end

to initialize-pursuittable
  let i 0
  let var 0
  let temp [0]
  while [i != no-of-pursuers]
  [
   set PEtable lput [[]] PEtable
   set i i + 1
  ]
  print(word "pursuit evader table " PEtable )
end

to printAMatrix
 print matrix:pretty-print-text cost
end

to printPlist
  print (word "Pursuer list: " Plist)
end

to printElist
  print (word "Evader list: " Elist)
end

to setup-nodes
  set-default-shape turtles "circle"
  ask turtles [ set ispursuer? false ]
  ask turtles [set isevader? false]
  ask turtles [ set idnumber who ]
  create-turtles no-of-nodes ;; users give this number from the interface
  [
    ; for visual reasons, we don't put any nodes *too* close to the edges
    setxy (random-xcor * 0.95) (random-ycor * 0.95)
  ]
end


to setup-edges
  let maxedges (no-of-nodes * (no-of-nodes - 1)) / 2
  show word "max number of edges: " maxedges
  let dir 2
  ifelse no-of-links <= maxedges
  [
    while [ count links < no-of-links ] ;; num-links given by the user from interface
    [
      ask one-of turtles
      [
        let source 0
        let target 0
        let choice (min-one-of (other turtles with [not link-neighbor? myself])
          [distance myself])
        if choice != nobody [
          create-link-with choice
          ask links[
            set source [who] of end1
            set target [who] of end2
            let ran random 100
          ;  set dir random 3
          ;  ifelse dir = 0
          ;  [
          ;    set direction "to"
          ;    matrix:set cost source target ran
          ;    matrix:set cost target source 99999
          ;  ]
          ;  [
          ;   ifelse dir = 1
          ;   [
          ;     set direction "from"
          ;     matrix:set cost target source ran
          ;     matrix:set cost source target 99999
          ;   ]
          ;   [
          ;     set direction "both"
               matrix:set cost source target ran
               matrix:set cost target source ran
           ;  ]
           ; ]
            set weight ran
          ]
        ]
      ]
    ]
    ; make the network look a little prettier
    repeat 10
    [
      layout-spring turtles links 0.3 (world-width / (sqrt no-of-nodes)) 1
    ]

  ]
  [
    user-message ("no of edges is more than max no of edges allowed")
  ]
end

to go
  let tiktok ticks
  set tiktok ticks mod 2
  print(word "evaders count: " count evaders word "tick: " ticks)
  ifelse ( (count evaders = 0 ) or (ticks >= 500))
  [
     print (word "elapsed time: " no-sec)
     if count evaders > 0
     [print (word "Evaders cannot be caught because it is isolated")]; kelangan pa ito iedit, pag hindi naman nag iinsert o nagddelete ng edge, kasi wala na talagang probability na mahuli ung evader na isolated so dapat pag gnun maidentify na kelangan na istop agad ang pursuit
     stop
  ]
  [
    tick
    reset-timer
    evader-strategy
    if count evaders > 0
    [ pursuit-strategy
      ask links [
        ifelse show-weights?
        [ set label weight ]
        [ set label "" ]
      ]
      ask turtles [
       ifelse show-node?
        [ set label who ]
        [ set label "" ]
       ]
      if tiktok = 0
      [
        dynamic_edge_random
      ]
    ]
    set no-sec no-sec + timer

    print (word "elapsed time: " no-sec)
    print (word "total travel time: " time-traveled)
  ]

end

to move-to-node [cnode nnode index]
  print(word "source:" cnode word"  target:" nnode word "  index:" index)
  let cnodeindex 99999
  let nnodeindex 99999
  ask turtle cnode
  [

   ifelse ispursuer? = true
   [

     ask link cnode nnode [
       show word "linkweight: " weight
       set time-traveled time-traveled + weight

     ]
     ifelse dupli cnode Plist = false
     [ ;return turtle to normal state
       set color gray
       set shape "circle"
       set breed turtles
       set ispursuer? false
       set isevader? false
     ]
     [
       set shape "person police"
       set ispursuer? true
       set breed pursuers
     ]

     ask turtle nnode
     [
       ifelse breed != evaders
       [
         set Plist replace-item index Plist nnode
       ]
       [
         let ecount dupli nnode Elist
         ifelse ecount = false
         [
           set nnodeindex position nnode Elist
           set Elist remove-item nnodeindex Elist
           set no-of-evaders no-of-evaders - 1
         ]
         [
           while [ecount != 0]
           [
             set nnodeindex position nnode Elist
             set Elist remove-item nnodeindex Elist
             set no-of-evaders no-of-evaders - 1
             set ecount ecount - 1
           ]
         ]
         terminate-pursuit nnode
         set Plist replace-item index Plist nnode
       ]
       set shape "person police"
       set ispursuer? true
       set breed pursuers
     ]
   ];  if isevader? = true ;if current node is evader
   [
     ifelse dupli cnode Elist = false
     [
       set color gray
       set shape "circle"
       set breed turtles
       set ispursuer? false
       set isevader? false
     ]
     [
       ;set color blue
       set shape "person service"
       set isevader? true
       set breed evaders
     ]
     ask turtle nnode
     [
       ifelse breed != pursuers
       [
        ; set color blue
         set shape "person service"
         set isevader? true
         set breed evaders
         set Elist replace-item index Elist nnode
       ]
       [
         set Elist remove-item index Elist
         set no-of-evaders no-of-evaders - 1
         terminate-pursuit cnode
         ;insert code here where it terminates the pursuit plans of pursuers to captured evaders
       ]

     ]
    ]

  ]
end

to-report dupli [ c pelist]
  let duplist filter [ ? = c] pelist
  ifelse length duplist >= 2 [report length duplist]
  [report false]
end

to terminate-pursuit [evader1]
  show word "terminate pursuit of evader: " evader1
  show word "Before PEtable:"PEtable
  let oldepath []
  let bilang 0
  let remticks 0
  let t-time 0
  foreach PEtable [
    if (length last ? > 0) and (last (last ?)  = evader1)
    [
        set oldepath ?
        set remticks length last oldepath
        set t-time first oldepath
        set no-ticks no-ticks - remticks
        set PEtable replace-item bilang PEtable replace-item 1 oldepath []
    ]
    set bilang bilang + 1
  ]

  show word "After PEtable: " PEtable
end

to pursuit-strategy
  let mlindex 99  ;index of minimimum path length in the list
  let pursuerpath [] ; list of path to pursue
  let ppathlength 0
  let targetnode 99
  let oldlist []
  let mlength 99
  let pllist []  ;path length list
  let spllist [] ;sorted path length list
  let scounter 0 ;counter to search index in a sorted list
  let pcounter 0 ;counter to search index of min in original list

  ifelse no-of-nodes = 2
  [
   ; printPlist
    move-to-node item 0 Plist item 0 Elist 0
  ]
  [
    print(word "pursuit strategy")
    ifelse no-ticks > 0
    [
      set mlength min map length map last PEtable
      ifelse mlength > 0 ;kapag ung pursuit path ng bawat pursuers ay hindi pa naubos
      [
        set mlindex position min map length map last PEtable map length map last PEtable ;get the index of the list with min path length within PEtable
        set pursuerpath item 1 item mlindex PEtable ;path to pursue
        set ppathlength length pursuerpath
        set targetnode item 0 pursuerpath
        set pursuerpath remove-item 0 pursuerpath
        set oldlist item mlindex PEtable
        set PEtable replace-item mlindex PEtable replace-item 1 oldlist pursuerpath ;;continue to access the right index of what to be replaced
      ]
      [; pag may naempty na na pursuitplan ng pursuer sa petable get the next min length compared to zero
        set pllist map length map last PEtable ;path length list
        set spllist sort map length map last PEtable  ;sorted path length list

        while [item scounter spllist = 0]
        [
          set scounter scounter + 1
        ]
        ;hanapin ung original index ng sorted
        while [item pcounter pllist != item scounter spllist]
        [ set pcounter pcounter + 1]
        set mlindex pcounter
        set pursuerpath item 1 item mlindex PEtable ;path to pursue
        set ppathlength length pursuerpath
        set targetnode item 0 pursuerpath
        set pursuerpath remove-item 0 pursuerpath
        set oldlist item mlindex PEtable
        set PEtable replace-item mlindex PEtable replace-item 1 oldlist pursuerpath
      ]

     ; show word "PEtable: " PEtable
    ;  show word "target node: " targetnode
     ; printPlist
     ; show word "mlindex: " mlindex
      move-to-node item mlindex Plist targetnode mlindex
     ; show word "pursuer index: " find-index item mlindex Plist
      set no-ticks no-ticks - 1
    ]
    [
      show word "no  more ticks" no-ticks
      nearestEtoP
    ]
  ]
  printPlist
end

to evader-strategy
  ask one-of evaders
  [
    if count link-neighbors > 0 [
      let emover who ;of one-of evaders
      let etarget [who] of one-of link-neighbors
      let index position emover Elist
      printElist
      move-to-node emover etarget index
    ]
    print(word "evader strategy")
    printElist
  ]
end

to initialize-pursuers
  set-default-shape pursuers "person police"
  while [ count pursuers != no-of-pursuers] ;; num of pursuers given by the user from interface
    [
      ask one-of turtles
      [
        set ispursuer? true
        set breed pursuers
        set Plist lput who Plist
      ]
    ]
  set Plist remove-duplicates Plist
end

to initialize-evaders
  set-default-shape evaders "person service"
  while [ count evaders != no-of-evaders]  ;; num of evaders given by the user from interface
    [
      ask one-of turtles
      [
        let choice one-of other turtles

        if ispursuer? != true
        [
         ; set color blue
          set breed evaders
          set isevader? true
          set Elist lput who Elist
        ]
      ]
    ]
    set Elist remove-duplicates Elist
end

to-report find-index [agent]
  let index-count 0

  while [item index-count Plist != agent]
  [
    set index-count index-count + 1
  ]
  report index-count
end

to nearestEtoP
  show word "enter nearestEtoP function " 0
  let pcounter 0
  let ecounter 0
  ;let co 999999
  let currentp 99999
  let currente 99999
  let costlist []
  let templist []
  let minvalue 99
  let minindex 99
  let evader1 99


  ifelse is-disconnected = false
  [
    ;set PEtable []
    while [pcounter != no-of-pursuers ]
    [
      set currentp item pcounter Plist
      while [ecounter != no-of-evaders ]
      [
        set currente item ecounter Elist
        set costlist lput dijkstra currentp currente costlist
        set ecounter ecounter + 1
      ]
      set minvalue min map first costlist
      set minindex position minvalue map first costlist
      set templist item 1 item minindex costlist
      ;set evader1 item minindex Elist
      ;show (word "costlist: " costlist)
      ;show (word "minvalue: " minvalue)
      set PEtable replace-item pcounter PEtable (list minvalue templist)
      set pcounter pcounter + 1
      set ecounter 0
      set costlist []
    ]
  ]
  [
    ;if initially connected ang graph
    let clusters nw:weak-component-clusters
    print(word "DISCONNECTED GRAPH!!!!")
   ; print(word "Number of Clusters: " length clusters)
   ; print (word "hanapin ang agent sa cluster na disconnected")

    ;; loop through the clusters and find pursuers and evaders
    (foreach clusters [
      let cluster ?1
      let cluster-list sort cluster
    ;  print (word "Size of cluster: " length cluster-list word "list: " cluster-list)
      if any? cluster with [breed = evaders or breed = pursuers] ;if the cluster has pursuers and evaders
      [
        let num-evaders count cluster with [breed = evaders]
        let num-pursuers count cluster with [breed = pursuers]
        let pursuelist cluster with [breed = pursuers]
        let evaderlist cluster with [breed = evaders]
        let p-list []
        let e-list []
        ask pursuelist [ set p-list lput who p-list ]
        ask evaderlist [ set e-list lput who e-list ]
        print (word "Evader count: " num-evaders word " list: " e-list )
        print (word "Pursuer count: " num-pursuers word " list: " p-list)
        if num-evaders > 0 [
        set pcounter 0
        set ecounter 0
        set costlist []
        set templist []
        while [pcounter != num-pursuers ]
        [
          set currentp item pcounter p-list
          while [ecounter != num-evaders ]
          [
            set currente item ecounter e-list
            show (word "pursuer: " currentp word "evader: " currente)
            set costlist lput dijkstra currentp currente costlist
            set ecounter ecounter + 1

            show (word "costlist: " costlist)
          ]
            ;show (word "costlist: " costlist)
            set minvalue min map first costlist
            set minindex position minvalue map first costlist
            set templist item 1 item minindex costlist
           ; set evader1 item minindex Elist
            ;set PEtable lput (list evader1 templist) PEtable


           ; show (word "minvalue: " minvalue)
            let pindex find-index currentp ;paano naman kung dalawang pursuer at the same node?
            set PEtable replace-item  pindex PEtable (list minvalue templist)
            set pcounter pcounter + 1
            set ecounter 0
            set costlist []


          ]
        ]
        if num-evaders = 0 and num-pursuers > 0 ; if there are pursuers in isolated graph but no evaders
        [
          set pcounter 0
          set templist []
          while [pcounter != num-pursuers ]
          [
            set currentp item pcounter p-list
            set templist []
            let pindex find-index currentp
            set PEtable replace-item  pindex PEtable (list currentp templist)
            set pcounter pcounter + 1
          ]

        ]
      ]
    ])

  ]
  print(word "pursuit evader table " PEtable );
  set no-ticks sum map length map last PEtable
  show word "ticks " no-ticks
end

to-report dijkstra [source target]
 ; print(word "pursuer: " source)
 ; print(word "evader: " target)
  let distlist []
  let selected []
  let prev []
  let path []
  let start source
  let i 0
  let m 0
  let mini 0
  let d 0
  let j 0
  let flag true
  let oldstart start
  ;initialization of lists
  while [i != no-of-nodes]
  [
   set distlist lput 99999 distlist
   set prev lput -1 prev
   set selected lput 0 selected
   set i i + 1
  ]

  set selected replace-item start  selected 1
  set distlist replace-item start distlist 0

  while [ (item target selected) = 0 and flag]
  [
    set oldstart start
    set mini 99999
    set m 0
    set i 0
    while [ i != no-of-nodes ]
    [
      set d ( item start distlist + ( matrix:get cost start i  ) )
      if ( (d < (item i distlist)) and (item i selected = 0) )
      [
        set distlist replace-item i distlist d
        set prev replace-item i prev start
      ]
      if (( mini > ( item i distlist)) and (item i selected = 0))
      [
        set mini ( item i distlist)
        set m i
      ]
      set i i + 1
    ]
   set start m
   set selected replace-item start selected 1
   if oldstart = start
   [ set flag false ]
  ]
  ;reversing the path to traverse from source to target
  ifelse flag
  [
    set start target
    while [ start != -1 ]
    [
      set path fput start path
      set start item start prev
    ]
    set path remove-item 0 path
  ]
  [
    print (word "WALANG PATH!!RETURN EMPTY PATH!")
    set path []
  ]
   ;print (word "cost to target: " item target distlist)

  report (list item target distlist path)
end

to-report is-disconnected
  let flag false
  ask turtles [
    if nw:eigenvector-centrality = false
    [
      set flag true
    ]
  ]
  if flag = true [
    color-clusters nw:weak-component-clusters
  ]
  report flag
end

to color-clusters [ clusters ]
  ask turtles [ set color gray - 3 ]
  let n length clusters
  let colors ifelse-value (n <= 12)
    [ n-of n remove gray remove white base-colors ] ;; choose base colors other than white and gray
    [ n-values n [ approximate-hsb (random 255) (255) (100 + random 100) ] ] ; too many colors - pick random ones

    ;; loop through the clusters and colors zipped together
    (foreach clusters colors [
      let cluster ?1
      let cluster-color ?2
      let cluster-list sort cluster
      foreach cluster-list [ ;; for each node in the cluster
        ask ? [
          ;; give the node the color of its cluster
          set color cluster-color
        ]
      ]
    ])
end

to dynamic_edge_direction
  if count links > 0 [
    ask one-of links [
      let source [who] of end1
      let target [who] of end2
      let ran random 3

      ifelse ran = 0
      [
        set direction "to"
        matrix:set cost source target weight
        matrix:set cost target source 99999
        set color red
      ]
      [
       ifelse ran = 1
       [
         set direction "from"
         matrix:set cost target source weight
         matrix:set cost source target 99999
         set color green
       ]
       [
         set direction "both"
         matrix:set cost source target weight
         matrix:set cost target source weight
         set color white
       ]
      ]
      print( word "CHANGE PATH DIRECTION!!!!")
      update_path source target
    ]
  ]
end

to dynamic_edge_weight
  if count links > 0 [
    ask one-of links [
      let source [who] of end1
      let target [who] of end2
      let ran random 100
      let oldweight weight
      matrix:set cost source target ran
      matrix:set cost target source ran
      set weight ran
      ifelse oldweight > ran
      [
        print (word "Nabawasan ang weight: " (oldweight - ran))
        update_edge_weight_path2 (oldweight - ran) source target

      ]
      [
        print (word "Nadagdagan ang weight: " (ran - oldweight))
        update_path source target
      ]
    ]
  ]
end

to dynamic_edge_random
  let ran random 2
  ifelse ran = 0 [ del_edge ]
  [ insert_edge ]
end

to update_path [node1 node2]
 print(word "update path with link: " node1 word " " node2)
  show word "Before PEtable:" PEtable
  let bilang 0
  let source 99999
  let target 99999
  let costlist []
  foreach PEtable [
    set costlist []
    if (length last ? > 0) and (member? node1 last ?) and (member? node2 last ?)
    [
      print( word "UPDATE PATH !!!!" bilang)
      set source item bilang Plist
      set target last (last ?)
      set costlist dijkstra source target
      show word "costlist" costlist
      let oldlength length last ?
      let newlength length last costlist
      let remticks oldlength - newlength
      show word "number of ticks added: " remticks
      set no-ticks no-ticks - remticks
      set PEtable replace-item bilang PEtable costlist
     ]
    set bilang bilang + 1
  ]
  show word "After PEtable: " PEtable
end
to update_edge_weight_path2 [computed node1 node2]
  show word "Before PEtable:" PEtable
  let bilang 0
  let costlist []
  let mi 0
  foreach PEtable [
    set costlist []
    if (length last ? > 0) and ( member? node1 item 1 ?) and (member? node2 item 1 ?) ; may problema dito, ksi baka ung isang node ay weight pala
    [
      print( word "UPDATE PATH WEIGHT DECREASE!!!!" bilang)
      set costlist ?
      set mi (first ? - computed)
      set PEtable replace-item bilang PEtable replace-item 0 costlist mi
     ]
    set bilang bilang + 1
  ]
  show word "After PEtable: " PEtable
end

to insert_edge
  ;check if source exists
  ;check if destination exists
  let insedge 0
  while [insedge = 0]
  [
  ask one-of turtles
      [
        let source 0
        let target 0
        let choice (min-one-of (other turtles with [not link-neighbor? myself])
          [distance myself])
        if choice != nobody [
          create-link-with choice
          ask links[
            set source [who] of end1
            set target [who] of end2
            let ran random 100

            matrix:set cost source target ran
            matrix:set cost target source ran
            set weight ran
          ]
          set no-of-links no-of-links + 1
          print (word "LINK INSERTED:"  )
          set insedge insedge + 1
          update_path source target

        ]
        repeat 10
        [
          layout-spring turtles links 0.3 (world-width / (sqrt no-of-nodes)) 1
        ]
      ]
  ]
end
to del_edge
  if count links > 0 [
    ask one-of links [
      let source [who] of end1
      let target [who] of end2
      matrix:set cost source target 99999
      matrix:set cost target source 99999
      print (word "REMOVE A LINK")
      set no-of-links no-of-links - 1
      update_path source target
      die
    ]
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
237
10
728
600
18
21
13.0
1
10
1
1
1
0
1
1
1
-18
18
-21
21
1
1
1
ticks
30.0

BUTTON
20
22
86
55
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
101
23
164
56
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

INPUTBOX
15
69
103
129
no-of-nodes
10
1
0
Number

INPUTBOX
14
134
103
194
no-of-pursuers
2
1
0
Number

INPUTBOX
107
70
191
130
no-of-links
12
1
0
Number

INPUTBOX
106
133
192
193
no-of-evaders
0
1
0
Number

SWITCH
15
201
164
234
show-weights?
show-weights?
1
1
-1000

SWITCH
15
240
148
273
show-node?
show-node?
1
1
-1000

MONITOR
16
276
109
321
NIL
count turtles
17
1
11

MONITOR
15
323
96
368
NIL
count links
17
1
11

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

person police
false
1
Polygon -1 true false 124 91 150 165 178 91
Polygon -13345367 true false 134 91 149 106 134 181 149 196 164 181 149 106 164 91
Polygon -13345367 true false 180 195 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285
Polygon -13345367 true false 120 90 105 90 60 195 90 210 116 158 120 195 180 195 184 158 210 210 240 195 195 90 180 90 165 105 150 165 135 105 120 90
Rectangle -7500403 true false 123 76 176 92
Circle -7500403 true false 110 5 80
Polygon -13345367 true false 150 26 110 41 97 29 137 -1 158 6 185 0 201 6 196 23 204 34 180 33
Line -13345367 false 121 90 194 90
Line -16777216 false 148 143 150 196
Rectangle -16777216 true false 116 186 182 198
Rectangle -16777216 true false 109 183 124 227
Rectangle -16777216 true false 176 183 195 205
Circle -1 true false 152 143 9
Circle -1 true false 152 166 9
Polygon -1184463 true false 172 112 191 112 185 133 179 133
Polygon -1184463 true false 175 6 194 6 189 21 180 21
Line -1184463 false 149 24 197 24
Rectangle -16777216 true false 101 177 122 187
Rectangle -16777216 true false 179 164 183 186

person service
false
12
Polygon -7500403 true false 180 195 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285
Polygon -1 true false 120 90 105 90 60 195 90 210 120 150 120 195 180 195 180 150 210 210 240 195 195 90 180 90 165 105 150 165 135 105 120 90
Polygon -1 true false 123 90 149 141 177 90
Rectangle -7500403 true false 123 76 176 92
Circle -7500403 true false 110 5 80
Line -13345367 false 121 90 194 90
Line -16777216 false 148 143 150 196
Rectangle -16777216 true false 116 186 182 198
Circle -1 true false 152 143 9
Circle -1 true false 152 166 9
Rectangle -16777216 true false 179 164 183 186
Polygon -2674135 true false 180 90 195 90 183 160 180 195 150 195 150 135 180 90
Polygon -2674135 true false 120 90 105 90 114 161 120 195 150 195 150 135 120 90
Polygon -2674135 true false 155 91 128 77 128 101
Rectangle -16777216 true false 118 129 141 140
Polygon -2674135 true false 145 91 172 77 172 101

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.3.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
