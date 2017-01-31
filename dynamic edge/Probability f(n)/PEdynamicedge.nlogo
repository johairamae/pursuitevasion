extensions [matrix array table]
turtles-own [ ispursuer? isevader? idnumber]
links-own [ weight ]
breed [ pursuers pursuer ]
breed [ evaders evader ] 
globals [AMatrix cost Plist Elist PEtable no-ticks]
 
to setup 
                        ;; clear everything on canvas
  ifelse ( (no-of-nodes > 0) and (no-of-nodes >= (no-of-pursuers + no-of-evaders)) and (no-of-evaders > 0) and (no-of-pursuers > 0)) 
   [
       clear-all 
      ; set AMatrix matrix:make-constant no-of-nodes no-of-nodes 0
       set cost matrix:make-constant no-of-nodes no-of-nodes 999
       set Plist []
       set Elist []
       set PEtable []
       set no-ticks 0
       setup-nodes                     ;; a procedure to set nodes
       setup-edges                     ;; a procedure to set edges
       ask turtles [ set color red]    ;; paint nodes red
       ask links [set color white]     ;; paint edges white
       initialize-pursuers
       initialize-evaders 
       printAMatrix
       printPlist
       printElist
       nearestEtoP               ;a function to compute the nearest evader to every pursuer
       reset-ticks
       ;reset-timer
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
  ifelse no-of-links <= maxedges
  [
    while [ count links < no-of-links ] ;; num-links given by the user from interface
    [
      ask one-of turtles
      [
        let idnum 0
        let cidnum 0
        let choice (min-one-of (other turtles with [not link-neighbor? myself])
          [distance myself])
        if choice != nobody [
          create-link-with choice       
          ;print (word "link count: " count links )
          ask links[
            set cidnum [who] of end1
            set idnum [who] of end2
            ;matrix:set AMatrix idnum cidnum 1
            ;matrix:set AMatrix cidnum idnum 1
            matrix:set cost idnum cidnum 1
            matrix:set cost cidnum idnum 1
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
  reset-timer
  print(word "evaders count: " count evaders word "tick: " ticks)
  ifelse ( (count evaders = 0 ) or (ticks >= 500))
  [
     ;set no-of-evaders 0 
     
     print (word "elapsed time: " timer)
     stop
  ]
  [
    evader-strategy
    if count evaders > 0  
    [ pursuit-strategy ]
  ]
  tick 
end

to move-to-node [cnode nnode index]
  print(word "source:" cnode word"  target:" nnode word "  index:" index)
  let cnodeindex 999
  let nnodeindex 999
  ask turtle cnode
  [
    
   ifelse ispursuer? = true
   [
     ifelse dupli cnode Plist = false
     [ ;return turtle to normal state
       set color red
       set shape "circle"
       set breed turtles
       set ispursuer? false
       set isevader? false
     ]
     [
       set color yellow 
       set shape "triangle"
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
       set color yellow 
       set shape "triangle"
       set ispursuer? true 
       set breed pursuers 
     ]
   ];  if isevader? = true ;if current node is evader
   [
     ifelse dupli cnode Elist = false
     [
       set color red
       set shape "circle"
       set breed turtles
       set ispursuer? false 
       set isevader? false
     ]
     [
       set color blue    
       set shape "square"
       set isevader? true    
       set breed evaders 
     ]
     ask turtle nnode
     [
       ifelse breed != pursuers
       [
         set color blue 
         set shape "square"
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
  ;show word "terminate pursuit of evader: " evader1
  ;show word "Before PEtable:"PEtable
  let oldepath []
  let bilang 0
  let remticks 0
  foreach PEtable [
    if first ? = evader1
    [
        set oldepath ?
        set remticks length last oldepath
        set no-ticks no-ticks - remticks
        set PEtable replace-item bilang PEtable replace-item 1 oldepath []
    ]
    set bilang bilang + 1
  ]
  ;show word "After PEtable: " PEtable
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
    printPlist
    move-to-node item 0 Plist item 0 Elist 0
  ]
  [
    print(word "pursuit strategy")   
    ifelse no-ticks > 0
    [
      set mlength min map length map last PEtable
      ifelse mlength > 0
      [
        set mlindex position min map length map last PEtable map length map last PEtable ;get the index of the list with min path length within PEtable
        set pursuerpath item 1 item mlindex PEtable ;path to pursue
        set ppathlength length pursuerpath
        set targetnode item 0 pursuerpath
        set pursuerpath remove-item 0 pursuerpath
        set oldlist item mlindex PEtable
        set PEtable replace-item mlindex PEtable replace-item 1 oldlist pursuerpath ;;continue to access the right index of what to be replaced
      ]
      [
        set pllist map length map last PEtable
        set spllist sort map length map last PEtable
        
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
      
      show word "PEtable: " PEtable
      show word "target node: " targetnode
      printPlist
      move-to-node item mlindex Plist targetnode mlindex
     ; if no-ticks mod 2 = 0
     ; [
     ;   let ran random 2
     ;   ifelse ran = 0 [ insert_edge ]
     ;   [ del_edge ] 
     ; ]
      set no-ticks no-ticks - 1
    ]
    [
      nearestEtoP
      show word "no  more ticks" no-ticks
    ]   
  ]
  printPlist
end 

to evader-strategy
  ask one-of evaders
  [
    let emover who ;of one-of evaders
    let etarget [who] of one-of link-neighbors
    let index position emover Elist
    printElist
    move-to-node emover etarget index
    print(word "evader strategy")
    printElist
  ]
end

to initialize-pursuers
  set-default-shape pursuers "triangle"
  while [ count pursuers != no-of-pursuers] ;; num of pursuers given by the user from interface
    [ 
      ask one-of turtles 
      [
        set color yellow 
        set ispursuer? true 
        set breed pursuers
        set Plist lput who Plist
      ] 
    ]
  set Plist remove-duplicates Plist
end

to initialize-evaders
  set-default-shape evaders "square"
  while [ count evaders != no-of-evaders]  ;; num of evaders given by the user from interface
    [
      ask one-of turtles 
      [
        let choice one-of other turtles

        if ispursuer? != true 
        [         
          set color blue
          set breed evaders
          set isevader? true
          set Elist lput who Elist
        ]
      ] 
    ]
    set Elist remove-duplicates Elist
end
 
to nearestEtoP
  show word "enter nearestEtoP function " 0
  let pcounter 0
  let ecounter 0
  ;let co 999999
  let currentp 9999
  let currente 9999
  let costlist []
  let templist []
  let minvalue 99
  let minindex 99
  let evader1 99
  set PEtable []
  
  while [pcounter != no-of-pursuers ]
  [
    set currentp item pcounter Plist
    while [ecounter != no-of-evaders ]
    [
     set currente item ecounter Elist
     set costlist lput dijkstra currentp currente costlist
     set ecounter ecounter + 1 
    ]
    ;print (word "costlist of pursuer " item pcounter Plist costlist)
    set minvalue min map first costlist
    set minindex position minvalue map first costlist
    ;print(word "min " minvalue word "min index " minindex)
    set templist item 1 item minindex costlist
    ;print(word "path " templist );
    ;print(word "evader" item minindex Elist)
    set evader1 item minindex Elist
    set PEtable lput (list evader1 templist) PEtable
    set pcounter pcounter + 1
    set ecounter 0
    set costlist []
  ]
  print(word "pursuit evader table " PEtable );
  set no-ticks sum map length map last PEtable
  show word "ticks " no-ticks
end

to-report dijkstra [source target]
  print(word "pursuer: " source)
  print(word "evader: " target)
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
  ;initialization of lists
  while [i != no-of-nodes]
  [  
   set distlist lput 9999 distlist
   set prev lput -1 prev 
   set selected lput 0 selected
   set i i + 1
  ]
  
  set selected replace-item start  selected 1
  set distlist replace-item start distlist 0

  while [ (item target selected) = 0]
  [
    set mini 999
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
  ]
  ;reversing the path to traverse from source to target
  set start target
  while [ start != -1 ]
  [
    set path fput start path
    set start item start prev 
  ]
  
  ;print (word "path " path)
  ;print (word "cost to target: " item target distlist)
  set path remove-item 0 path
  report (list item target distlist path)  
end

to insert_edge 
  ;check if source exists
  ;check if destination exists
  let insedge 0
  while [insedge = 0]
  [
  ask one-of turtles
      [
        let idnum 0
        let cidnum 0
        let choice (min-one-of (other turtles with [not link-neighbor? myself])
          [distance myself])
        if choice != nobody [
          create-link-with choice       
          ask links[
            set cidnum [who] of end1
            set idnum [who] of end2
            matrix:set cost idnum cidnum 1
            matrix:set cost cidnum idnum 1
          ]
          set no-of-links no-of-links + 1 
          print (word "LINK INSERTED:"  )
          set insedge insedge + 1
         ; update_pursuit_list cidnum idnum
          
        ]
        repeat 10
        [
          layout-spring turtles links 0.3 (world-width / (sqrt no-of-nodes)) 1
        ]
      ] 
  ]
end

to del_edge 
  ask one-of links [
    let source [who] of end1
    let target [who] of end2
    matrix:set cost source target 999
    matrix:set cost target source 999
    print (word "REMOVE A LINK")
    set no-of-links no-of-links - 1
    ;update_pursuit_list source target
    die
   ] 
end

to update_pursuit_list [node1 node2]   ;update the pursuit plan of pursuers affected
  show word "update pursuit of evader: " node1
  show word "Before PEtable:" PEtable
  let oldepath []
  let bilang 0
  let remticks 0
  foreach PEtable [
    if first ? = node1
    [
        set oldepath ?
        set remticks length last oldepath
        set no-ticks no-ticks - remticks
        set PEtable replace-item bilang PEtable replace-item 1 oldepath []
    ]
    set bilang bilang + 1
  ]
  ;show word "After PEtable: " PEtable
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
32
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
70
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
NetLogo 5.0.4
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 1.0 0.0
0.0 1 1.0 0.0
0.2 0 1.0 0.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
