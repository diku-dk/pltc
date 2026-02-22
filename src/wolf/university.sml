(* General utilities *)

infix |>
fun a |> f = f a

fun mapi f xs =
    let fun loop (i,a,nil) = rev a
          | loop (i,a,x::xs) = loop(i+1,f(i,x)::a,xs)
    in loop (0,nil,xs)
    end

fun ppInt i = if i < 0 then "-" ^ Int.toString (~i) else Int.toString i

fun $ id =
    case Js.getElementById Js.document id of
      SOME e => e
    | NONE => raise Fail ("no element with id '"^id^"' in DOM")

fun println s = print (s ^ "\n")

fun serverGet file =
    let val file = file ^ "?_=" ^ Real.toString (Time.toReal(Time.now())) (* avoid cache *)
        open Js.XMLHttpRequest
        val r = new()
        val () = openn r {method="GET",url=file,async=false}
        val () = send r NONE
    in case response r of
           SOME res => res
         | NONE => raise Fail ("serverGet failed on file " ^ file)
    end

fun serverGetAsync file (f:string option -> unit) : unit =
    let val file = file ^ "?_=" ^ Real.toString (Time.toReal(Time.now())) (* avoid cache *)
        open Js.XMLHttpRequest
        val r = new()
        val () = openn r {method="GET",url=file,async=true}
        val () = onStateChange r (fn () =>
                                     case status r of
                                         SOME 200 =>
                                         (case response r of
                                              SOME res => f (SOME res)
                                            | NONE => f NONE (* weird *))
                                       | SOME _ => f NONE
                                       | NONE => ())
        val () = send r NONE
    in ()
    end

fun look (k:'a->bool) (xs:'a list) : 'a option =
    List.find k xs

fun log s = let val log = $"log"
            in Js.appendChild log (Js.createTextNode s);
               Js.appendChild log (Js.createElement "br")
            end

fun windowInnerWidth () =
    JsCore.exec0 {stmt="return window.innerWidth;",res=JsCore.int} ()

fun windowInnerHeight () =
    JsCore.exec0 {stmt="return window.innerHeight;",res=JsCore.int} ()

fun isMobile () =
    windowInnerWidth () <= 1000 andalso windowInnerHeight () <= 1000

fun prefixzeros i s = if size s < i then prefixzeros i ("0" ^ s)
                      else s

val deviceStr = if isMobile() then "mobile" else "stationary"

val deviceStr = Int.toString (windowInnerWidth()) ^ "x" ^ Int.toString(windowInnerHeight())

val minimap_p = not (isMobile())


datatype lang = DA | EN

fun speak (lang:lang) (txt:string) =
    let val lan = case lang of DA => "dk-DA" | EN => "en-GB"
        val synth = JsCore.exec0{stmt="return window.speechSynthesis;",
                                 res=JsCore.fptr} ()
        val () = JsCore.exec1{stmt="synth.cancel();",
                              arg1=("synth",JsCore.fptr),
                              res=JsCore.unit} synth
        val _ = JsCore.exec0{stmt="window.utterances = [];", res=JsCore.unit} ()
        val msg = JsCore.exec1{stmt="return new SpeechSynthesisUtterance(txt);",
                               arg1=("txt",JsCore.string),
                               res=JsCore.fptr} txt
        val () = JsCore.exec1{stmt="window.utterances.push(u);",
                              arg1=("u",JsCore.fptr),
                              res=JsCore.unit} msg
        val () = JsCore.setProperty msg JsCore.string "lang" lan
    in JsCore.exec2{stmt="synth.speak(msg);",
                    arg1=("synth",JsCore.fptr),
                    arg2=("msg",JsCore.fptr),
                    res=JsCore.unit} (synth,msg)
    end

(* Staff *)

structure Staff = struct

  type sid = string (* staff id *)

  type slide = {id:string,lang:lang option,pages:int}
  type staff = {id:sid,name:string,office:string,avatar:string,slides:slide list}

  fun loadStaff (data:string) : staff list =
      let fun fail k s =
              raise Fail ("Failed to find " ^ k ^ " json object " ^ s ^ " in staff.json")
          fun look obj s =
              case Json.objLook obj s of
                  SOME obj => obj
                | NONE => fail "" s
          fun lookStr obj s =
              case look obj s of
                  Json.STRING s => s
                | _ => fail "string" s
          fun lookInt obj s =
              case Int.fromString(lookStr obj s) of
                  SOME i => i
                | NONE => fail "int" s
          fun lookLang obj s =
              case Json.objLook obj s of
                  SOME(Json.STRING "EN") => SOME EN
                | SOME(Json.STRING "en") => SOME EN
                | SOME(Json.STRING "DA") => SOME DA
                | SOME(Json.STRING "da") => SOME DA
                | SOME _ => fail "da or en" s
                | _ => NONE
          fun lookSlides obj s =
              case look obj s of
                  Json.ARRAY j =>
                  List.foldr (fn (j,a) =>
                                 case j of
                                     Json.OBJECT obj =>
                                     {id=lookStr obj "id",lang=lookLang obj "lang",
                                      pages=lookInt obj "pages"}::a
                                   | _ => fail "ARRAY OBJECT" s) nil j
                | _ => fail "ARRAY" s
          fun jsonToStaff j : staff =
              case j of
                  Json.OBJECT obj => {id=lookStr obj "id",name=lookStr obj "name",
                                      avatar=lookStr obj "avatar", office=lookStr obj "office",
                                      slides=lookSlides obj "slides"}
                | _ => fail "OBJECT" "ARRAY"
      in
        Json.foldlArrayJson (fn (j,a) => jsonToStaff j :: a) nil data
      end

  type display_content = {sid:sid,slideIdx:int ref,pngbase:string ref} list

  fun pagesDeck (staff:staff list) sid did =
      case look (fn {id,...} => id=sid) staff of
          NONE => NONE
        | SOME x =>
          case look (fn {id,...} => id = did) (#slides x) of
              NONE => NONE
            | SOME x => SOME(#pages x)

  fun nextSlide (staff:staff list) ({sid,slideIdx,pngbase=ref did}) =
      case pagesDeck staff sid did of
          SOME pg => slideIdx := ((!slideIdx + 1) mod pg)
        | NONE => ()

  fun prevSlide (staff:staff list) ({sid,slideIdx,pngbase=ref did}) =
      case pagesDeck staff sid did of
          SOME pg => slideIdx := ((!slideIdx - 1 + pg) mod pg)
        | NONE => ()

  fun onDeck f (staff:staff list) ({sid,slideIdx,pngbase}) =
      case look (fn {id,...} => id=sid) staff of
          NONE => ()
        | SOME {slides,...} =>
          let val slides = f slides
              val did = !pngbase
              fun loop ((x:slide)::y::xs) = if #id x = did then pngbase := #id y
                                            else loop (y::xs)
                | loop _ = case slides of
                               y::_ => pngbase := #id y
                             | _ => ()
          in loop slides
           ; slideIdx := 0
          end

  fun nextDeck staff x = onDeck (fn x => x) staff x
  fun prevDeck staff x = onDeck rev staff x
end

(* Sprite and map object definitions *)

datatype sprite = Table | Armor | Plant | Lamp | Desk | Sink | Toilet

datatype dir = North | South | East | West

datatype obj = Wall | Bulletin | Whiteboard
             | ShelfLow | ShelfHigh
             | WindowLeft | WindowCenter | WindowRight
             | DisplayLeft of dir -> Staff.sid option
             | DisplayRight of dir -> Staff.sid option
             | Sprite of sprite
             | Space
             | Wall1 | Wall2 | Wall3

fun isWall Space = false
  | isWall (Sprite _) = false
  | isWall _ = true

structure Map :
          sig
            val load : string -> obj Array2.array * Staff.sid list
            val findClosestDisplay : int*int -> Staff.sid option
          end = struct
  type map = string list
  type display = string*int*int
  fun loadMapAndDisplays (data: string) : map * display list =
      let fun loop nil a = (rev a,nil)
            | loop ("="::xs) a = (rev a,xs)
            | loop (x::xs) a = loop xs (x::a)
          val (m,displays) = loop (String.tokens (fn c => c= #"\n") data) nil
          fun parseDisplay s =
              case String.tokens (fn c => c= #":" orelse c= #",") s of
                  [staffid,x,y] =>
                  (case (Int.fromString x, Int.fromString y) of
                       (SOME x, SOME y) => (staffid,x,y)
                     | _ => raise Fail "Failed to parse integer locations of displays in gameMap.txt")
                | _ => raise Fail "Failed to parse display locations in gameMap.txt"
      in (m,map parseDisplay displays)
      end

  fun sq r : real = r * r
  fun dist (x0,y0) (x,y) = Math.sqrt(sq(real(x0-x)) + sq(real(y0-y)))

  fun findClosest (x,y) (displays:display list) : Staff.sid option =
      let val dists = map (fn (sid,x0,y0) => (sid,dist(x0,y0)(x,y))) displays
          fun mini NONE (x::xs) = mini (SOME x) xs
            | mini (f as SOME(sid,d)) ((sid',d')::xs) =
              if d' < d then mini (SOME(sid',d')) xs
              else mini f xs
            | mini opt nil = opt
      in case mini NONE dists of
             SOME (sid,_) => SOME sid
           | NONE => NONE
      end

  fun mkFun (x,y) (displays:display list) : dir -> Staff.sid option =
      let val north = List.filter (fn s => #3 s - 1 = y) displays (* include only indicaters to the north *)
                                  |> findClosest (x,y)
          val south = List.filter (fn s => #3 s + 1 = y) displays (* include only indicaters to the south *)
                                  |> findClosest (x,y)
          val west = List.filter (fn s => #2 s + 1 = x) displays  (* include only indicaters to the west *)
                                 |> findClosest (x,y)
          val east = List.filter (fn s => #2 s - 1 = x) displays  (* include only indicaters to the east *)
                                 |> findClosest (x,y)
          fun nosome (SOME s) = s | nosome NONE = "none"
(*
          val () = log (Int.toString x ^ "," ^ Int.toString y ^ ":" ^
                        "N:" ^ nosome north ^
                        ", S:" ^ nosome south ^
                        ", E:" ^ nosome east ^
                        ", W:" ^ nosome west)
*)
      in fn North => north
          | South => south
          | East => east
          | West => west
      end

  fun chToObj displays (x,y) c =
      case c of
        #" " => Space
      | #"w" => Wall1
      | #"=" => Wall2
      | #"%" => Wall3
      | #"M" => DisplayLeft (mkFun (x,y) displays)
      | #"N" => DisplayRight (mkFun (x,y) displays)
      | #"a" => WindowLeft
      | #"b" => WindowCenter
      | #"c" => WindowRight
      | #"*" => Wall
      | #"H" => ShelfLow
      | #"K" => ShelfHigh
      | #"O" => Whiteboard
      | #"I" => Bulletin
      | #"T" => Sprite Table
      | #"A" => Sprite Armor
      | #"P" => Sprite Plant
      | #"L" => Sprite Lamp
      | #"D" => Sprite Desk
      | #"S" => Sprite Sink
      | #"U" => Sprite Toilet
      | _ => raise Fail ("unknown character '" ^ Char.toString c ^ "' in map")

  fun line displays (y,s:string) : obj list =
      CharVector.foldri (fn (x,c,a) => chToObj displays (x,y) c :: a) [] s

  val dsps : display list ref = ref nil

  fun load (data:string) : obj Array2.array * string list =
      let val (sm,displays) = loadMapAndDisplays data
          val () = dsps := displays
      in ( Array2.fromList (mapi (line displays) sm)
         , map #1 displays
         )
      end

  fun findClosestDisplay (x,y) : Staff.sid option =
      findClosest (x,y) (!dsps)

end

fun findDisplayState (displayContent:Staff.display_content) (sid:Staff.sid) =
    List.find (fn {sid=sid',slideIdx,pngbase} => sid = sid') displayContent

local

(* Print the DOM *)
val screenWidth    = if isMobile() then windowInnerWidth() else 1600 div 2
val screenHeight   = if isMobile() then windowInnerHeight() else 900 div 2

val _ = println "<html><head>"
val _ = println "<title>Canvas example</title>"
val _ = println "<link rel='shortcut icon' type='image/x-icon' href='favicon.ico' />"
val _ = println "<style>"
val _ = if not (isMobile()) then
          ( println "div#minimapcontainer { display : flex; justify-content : center; align-items : center; margin: 10px;}"
          ; println ("canvas#minimap { position : absolute; width : " ^ ppInt screenWidth ^ "px; }")
          ; println ("canvas#minimapobjects { position : absolute; width : " ^ ppInt screenWidth ^ "px; }")
          )
        else ()
val _ = println "div#floor { position : absolute; width : 100%; height : 100%; background-color : rgb(128,128,128); z-index : -10000000; }"
val _ = println "div#ceiling { position : absolute; width : 100%; height : 50%; background-color : rgb(96,96,96); z-index : -10000000; }"
val _ = println ("#screen { position : relative; width : " ^ ppInt screenWidth ^ "px; height : " ^ ppInt screenHeight ^ "px; border : 0px; overflow : hidden; }")
val _ = println ("div#overlay { position : absolute; display : block; width : " ^
                 ppInt (screenWidth - 10) ^ "px; height : " ^ ppInt (screenHeight - 10) ^
                 "px; padding : 5px; color : white; font-family : lucida console, courier new; font-size : 20px; z-index : 1; }")
val _ = if isMobile() then
          println ("html, body {margin: 0; height: 100%; overflow: hidden}")
        else ()
val _ = println "</style>"
val _ = println "</head><body><center>"
val _ = println "<div id='screen'>"
val _ = println "<div id='floor'></div>"
val _ = println "<div id='ceiling'></div>"
val _ = println "<div id='overlay'></div>"
val _ = println "</div>"
val _ = if not(isMobile()) then
          ( println "<div id='minimapcontainer'>"
          ; println "<canvas id='minimap'></canvas>"
          ; println "<canvas id='minimapobjects'></canvas>"
          ; println "</div>"
          )
        else ()
val _ = println "<div id='log'></div>"
val _ = println "</center></body></html>"

(* Load the map *)
val (Map,staffids) : obj Array2.array * string list =
    Map.load (serverGet "data/gameMap.txt")
    handle exn as Fail msg => (log msg; raise exn)

structure Sprite = struct

fun toSprite (obj) : sprite option =
    case obj of
        Sprite spr => SOME spr
      | _ => NONE

fun blocking k =
    case k of
        Table => true
      | Armor => true
      | Plant => true
      | Lamp => false
      | Sink => false
      | Desk => true
      | Toilet => true

fun img k (p:int*int) =
    case k of
        Table => "tablechairs.png"
      | Armor => let val i = ((#1 p + #2 p) mod 4 + 1)
                 in "professor" ^ Int.toString i ^ ".gif"
                 end
      | Plant => "plantgreen.png"
      | Lamp => "lamp.png"
      | Sink => "sink.png"
      | Desk => "desk.png"
      | Toilet => "toilet.png"

type t = {img: Js.elem, visible: bool ref, block: bool, pos: int * int, enabled: bool ref}

fun new typ pos =
    let val e = Js.createElement "img"
        val () = JsCore.setProperty (Js.Element.toForeignPtr e) JsCore.string "src" (img typ pos)
        val () = Js.setStyle e ("display", "none")
        val () = Js.setStyle e ("position", "absolute")
    in {img=e, block=blocking typ, visible=ref false, pos=pos, enabled=ref true}
    end

fun init items =
    let val screen = $"screen"
        val (rows,cols) = Array2.dimensions Map
        val spriteMap : t option Array2.array = Array2.array(rows,cols,NONE)
    in List.app (fn (k,x,y) => let val s = new k (x,y)
                               in Array2.update(spriteMap,y,x,SOME s);
                                  Js.appendChild screen (#img s)
                               end) items;
       spriteMap
    end

fun region a = {base=a,row=0,col=0,nrows=NONE,ncols=NONE}

fun mapItems Map =
    Array2.foldi Array2.RowMajor (fn (y,x,obj,acc) =>
                                     case toSprite obj of
                                         SOME spr => (spr,x,y)::acc
                                       | NONE => acc) [] (region Map)
end

val MaxRotSpeed = 3.0 * Math.pi / 360.0 (* how much does the player rotate each step/update (in radians) *)

val player = {
   x = ref 16.0,        (* current x, y position *)
   y = ref 10.0,
   rot = ref 0.0,       (* the current angle of rotation *)
   speed = ref 0.0,     (* the playing moving forward (speed = 1) or backwards (speed = -1) *)
   rotSpeed = ref 0.0,  (* the direction that the player is turning, between -1.0 for left and 1.0 for right *)
   moveSpeed = 0.10,    (* how far (in map units) does the player move each step/update *)
   rotAcc = ref 0.0
}

(* Constants *)
val fov            = 60.0 * Math.pi / 180.0
val fovHalf        = fov / 2.0
val twoPI          = Math.pi * 2.0
val stripWidth     = 1
val stripWidthR    = real stripWidth
val miniMapScale   = 8
val miniMapScaleR  = real miniMapScale
val rotAccDelta = 0.1

(* Calculation of map-specific constants *)
val mapWidth       = Array2.nCols Map
val mapWidthR      = real mapWidth
val mapHeight      = Array2.nRows Map
val mapHeightR     = real mapHeight
val screenWidthR   = real screenWidth
val screenHeightR  = real screenHeight
val numRays        = Real.ceil(screenWidthR / stripWidthR)
val viewDist       = screenWidthR / 2.0 / Math.tan fovHalf

fun installDocHandler s (f:int->unit) : unit =
    JsCore.exec1 {stmt="document." ^ s ^ " = function(e) { e = e || window.event; f(e.keyCode); };",
                  res=JsCore.unit,
                  arg1=("f",JsCore.==>(JsCore.int,JsCore.unit))} f

fun installOnkeydownHandler f = installDocHandler "onkeydown" f
fun installOnkeyupHandler f = installDocHandler "onkeyup" f

fun bindKeys staff displayContent =
    let fun startSpeak {sid,slideIdx=ref idx,pngbase=ref base} : unit =
          let val src = "data/" ^ sid ^ "/" ^ base ^ "/" ^ base ^ "-" ^ prefixzeros 3 (Int.toString idx) ^ ".txt"
          in serverGetAsync src (fn NONE => speak EN ""
                                  | SOME txt => speak EN txt)
          end
        fun onDisplayContent f =
            let val x = floor (!(#x player))
                val y = floor (!(#y player))
            in case Map.findClosestDisplay (x,y) of
                   NONE => ()
                 | SOME sid =>
                   case findDisplayState displayContent sid of
                       NONE => ()
                     | SOME dc => (f dc; startSpeak dc)
            end
        val () = installOnkeydownHandler
                     (fn 38 => #speed player := 1.0   (* up, move player forward, ie. increase speed *)
	               | 40 => #speed player := ~1.0  (* down, move player backward, set negative speed *)
	               | 37 => let val rotAcc = #rotAcc player
                               in if !rotAcc > ~0.5
                                  then rotAcc := !rotAcc - rotAccDelta  (* left, rotate player left *)
                                  else ()
                               end
	               | 39 => let val rotAcc = #rotAcc player
                               in if !rotAcc < 0.5
                                  then rotAcc := !rotAcc + rotAccDelta  (* right, rotate player right *)
                                  else ()
                               end
             (*p*)     | 112 => onDisplayContent (Staff.prevSlide staff)
             (*P*)     | 80 => onDisplayContent (Staff.prevSlide staff)
             (*n*)     | 110 => onDisplayContent (Staff.nextSlide staff)
             (*N*)     | 78 => onDisplayContent (Staff.nextSlide staff)
             (* *)     | 32 => onDisplayContent (Staff.nextSlide staff)
             (*a*)     | 97 => onDisplayContent (Staff.prevDeck staff)
             (*A*)     | 65 => onDisplayContent (Staff.prevDeck staff)
             (*s*)     | 115 => onDisplayContent (Staff.nextDeck staff)
             (*S*)     | 83 => onDisplayContent (Staff.nextDeck staff)
                       | _ => (*log ("Received: " ^ Int.toString i)*) ()
                     )
        val () = installOnkeyupHandler
                     (fn 38 => #speed player := 0.0     (* stop the player movement when up/down key is released *)
	               | 40 => #speed player := 0.0
	               | 37 => (#rotSpeed player := 0.0; #rotAcc player := 0.0)  (* stop the player movement when left/right key is released *)
	               | 39 => (#rotSpeed player := 0.0; #rotAcc player := 0.0)
                       | _ => ()
                     )
    in ()
    end

infix +=
fun a += (n:real) = a := !a + n

fun isBlocking spriteMap (x,y) =
    (y < 0.0 orelse y >= mapHeightR orelse x < 0.0 orelse x >= mapWidthR) orelse
    let val Y = Real.floor y
        val X = Real.floor x
        val wt = Array2.sub(Map, Y, X)
    in isWall wt orelse
       case Array2.sub(spriteMap,Y,X) of
         SOME (s:Sprite.t) => #block s
       | NONE => false
    end

fun checkCollision spriteMap (fromX, fromY, toX, toY, radius) =
    if toY < 0.0 orelse toY >= mapHeightR orelse toX < 0.0 orelse toX >= mapWidthR then
      (fromX,fromY)
    else
      let val blockX = real (Real.floor toX)
	  val blockY = real (Real.floor toY)
          val isBlock = isBlocking spriteMap
      in if isBlock(blockX,blockY) then (fromX,fromY)
	 else
           let
	     val blockTop = isBlock(blockX,blockY-1.0)
	     val blockBottom = isBlock(blockX,blockY+1.0)
	     val blockLeft = isBlock(blockX-1.0,blockY)
	     val blockRight = isBlock(blockX+1.0,blockY)
             val toY =
                 if blockTop andalso toY - blockY < radius then blockY + radius
                 else if blockBottom andalso blockY+1.0 - toY < radius then blockY + 1.0 - radius
                 else toY
             val toX =
                 if blockLeft andalso toX - blockX < radius then blockX + radius
                 else if blockRight andalso blockX+1.0 - toX < radius then blockX + 1.0 - radius
                 else toX
           in (toX,toY)
           end
      end

infix ==
fun a == (b:real) = a >= b andalso b >= a

fun minimum (a:real) b = if a > 0.0 then
                           if a > b then b else a
                         else
                           if a < ~b then ~b else a

fun move spriteMap gameCycleDelay timeDelta =
    let val mul = real (IntInf.toInt timeDelta) / real (IntInf.toInt gameCycleDelay)
        val moveStep = mul * !(#speed player) * #moveSpeed player (* player will move this far along the current direction vector *)
        val () = #rotSpeed player := minimum (!(#rotSpeed player) + !(#rotAcc player)) 2.0
        val drot = mul * !(#rotSpeed player) * MaxRotSpeed        (* add rotation if player is rotating (!player#dir <> 0) *)
	val () = #rot player += drot
        val dx = Math.cos(!(#rot player)) * moveStep
        val dy = Math.sin(!(#rot player)) * moveStep
(*
        val () = if moveStep > 0.0 then
                   log ("dx=" ^ Real.toString dx ^ "; dy=" ^ Real.toString dy ^ "; drot=" ^ Real.toString drot ^ "; moveStep=" ^ Real.toString moveStep)
                 else ()
*)
        val fromX = !(#x player)
        val fromY = !(#y player)
        val (newX,newY) = checkCollision spriteMap (fromX,fromY,fromX+dx,fromY+dy,0.35)
    in #x player := newX;
       #y player := newY
    end

structure C = Canvas

fun getElemProperty e t s =
    JsCore.getProperty (Js.Element.toForeignPtr e) t s

fun updateMiniMap () =
    if not minimap_p then ()
    else
    let val miniMap = $ "minimap"
	val miniMapObjects = $ "minimapobjects"
        val height = getElemProperty miniMap JsCore.int "height"
        val width = getElemProperty miniMap JsCore.int "width"
	val c = C.getContext miniMapObjects "2d"
	val () = C.clearRect c 0 0 width height

	val () = (* draw a dot at the current player position *)
            C.fillRect c (Real.round(!(#x player) * miniMapScaleR - 2.0))
                       (Real.round(!(#y player) * miniMapScaleR - 2.0))
                       4 4
	val () = C.beginPath c
        val () = C.moveTo c (Real.round(!(#x player) * miniMapScaleR))
                          (Real.round(!(#y player) * miniMapScaleR))
	val () = C.lineTo c
                          (Real.round((!(#x player) + Math.cos(!(#rot player))) * miniMapScaleR))
                          (Real.round((!(#y player) + Math.sin(!(#rot player))) * miniMapScaleR))
    in C.closePath c;
       C.stroke c
    end

fun setElemPropertyInt e p i =
    JsCore.setProperty (Js.Element.toForeignPtr e) JsCore.int p i

fun drawRay (rayX, rayY) =
    let val miniMapObjects = $ "minimapobjects"
	val c = C.getContext miniMapObjects "2d"
    in
      C.strokeStyle c "rgba(0,100,0,0.3)";
      C.lineWidth c "0.5";
      C.beginPath c;
      C.moveTo c (Real.round(!(#x player) * miniMapScaleR)) (Real.round(!(#y player) * miniMapScaleR));
      C.lineTo c (Real.round(rayX * miniMapScaleR)) (Real.round (rayY * miniMapScaleR));
      C.closePath c;
      C.stroke c
    end

fun drawMiniMap () =
    if not minimap_p then ()
    else
    (* draw the topdown view minimap *)
    let	val miniMap = $ "minimap"               (* the actual map *)
	val miniMapCtr = $ "minimapcontainer"	(* the container div element *)
	val miniMapObjects = $ "minimapobjects" (* the canvas used for drawing the objects on the map (player character, etc) *)

        val w = mapWidth * miniMapScale
        val h = mapHeight * miniMapScale
	val () = setElemPropertyInt miniMap "width" w   (* resize the internal canvas dimensions *)
	val () = setElemPropertyInt miniMap "height" h (* of both the map canvas and the object canvas *)
	val () = setElemPropertyInt miniMapObjects "width" w
	val () = setElemPropertyInt miniMapObjects "height" h
	val () = setElemPropertyInt miniMapCtr "width" w
	val () = setElemPropertyInt miniMapCtr "height" h

	val w_s = ppInt w ^ "px" (* minimap CSS dimensions *)
	val h_s = ppInt h ^ "px"
	val () = Js.setStyle miniMap ("width",w_s)
        val () = Js.setStyle miniMapObjects ("width",w_s)
        val () = Js.setStyle miniMapCtr ("width", w_s)
	val () = Js.setStyle miniMap ("height",h_s)
        val () = Js.setStyle miniMapObjects ("height",h_s)
        val () = Js.setStyle miniMapCtr ("height", h_s)

	val c = C.getContext miniMap "2d"

	(* loop through all blocks on the map *)
        val entireMap = {base=Map,row=0,col=0,nrows=NONE,ncols=NONE}

        fun each (y,x,k) =
            if isWall k then (* if there is a wall block at this (x,y) ...*)
              (* ... then draw a block on the minimap *)
              (C.fillStyle c "rgb(200,200,200)";
               C.fillRect c (x * miniMapScale) (y * miniMapScale) miniMapScale miniMapScale)
            else ()

        val () = Array2.appi Array2.RowMajor each entireMap
    in
	updateMiniMap()
    end

fun RealMod (r:real,m) =
    if r < m then r
    else RealMod(r - m, m)

structure Strip = struct
  type data = {width:int ref,height:int ref,left:int ref,top:int ref,clip:string ref,zIndex:int ref, src:string ref}
  fun emptyData () = {width=ref ~1,height=ref ~1,left=ref ~1,top=ref ~1,clip=ref "",zIndex=ref ~1, src=ref "walls.png"}
  fun setProp pp (prop:string) (sel:data->int ref) (strip,data:data) (n:int) : unit =
      let val r = sel data
      in if !r = n then ()
         else (Js.setStyle strip (prop, pp n);
               r := n)
      end
  fun ppPx n = ppInt n ^ "px"
  val setWidth = setProp ppPx "width" #width
  val setHeight = setProp ppPx "height" #height
  val setTop = setProp ppPx "top" #top
  val setLeft = setProp ppPx "left" #left
  val setZindex = setProp ppInt "zIndex" #zIndex

  fun setClip (strip,data:data) (c:string) : unit =
      let val r = #clip data
      in if !r = c then ()
         else (Js.setStyle strip ("clip", c);
               r := c)
      end

  fun setSrc (strip,data:data) (s:string) : unit =
      let val r = #src data
      in if !r = s then ()
         else (Js.setAttribute strip "src" s;
               r := s)
      end
end

fun castSingleRay displayContent screenStrips
                  (spriteMap: Sprite.t option Array2.array)
                  (rayAngle, stripIdx, visibleSprites) =
    let
      (* make sure the angle is between 0 and 360 degrees *)
      val rayAngle = RealMod (rayAngle, twoPI)
      val rayAngle = if rayAngle < 0.0 then rayAngle + twoPI else rayAngle

      (* moving right/left? up/down? Determined by which quadrant the angle is in. *)
      val right = rayAngle > twoPI * 0.75 orelse rayAngle < twoPI * 0.25
      val up = rayAngle < 0.0 orelse rayAngle > Math.pi

      (* only do these once *)
      val angleSin = Math.sin rayAngle
      val angleCos = Math.cos rayAngle

      (* first check against the vertical map/wall lines
       * we do this by moving to the right or left edge of the block we're standing in
       * and then moving in 1 map unit steps horizontally. The amount we have to move vertically
       * is determined by the slope of the ray, which is simply defined as sin(angle) / cos(angle).
       *)

      val slope = angleSin / angleCos	(* the slope of the straight line made by the ray *)
      val dXVer = if right then 1.0 else ~1.0 (* we move either 1 map unit to the left or right *)
      val dYVer = dXVer * slope (* how much to move up or down *)

      val x = if right then Real.ceil (!(#x player)) else Real.floor(!(#x player)) (* starting horizontal position, at one of the edges of the current map block *)
      val x = real x
      val y = !(#y player) + (x - (!(#x player))) * slope (* starting vertical position. We add the small horizontal step we just made, multiplied by the slope. *)

      datatype hitkind = Vhit | Hhit

      val emptyHit = {xHit = 0.0, yHit = 0.0, dist = 0.0, textureX = 0.0, xWallHit = 0, yWallHit = 0, wallType = Space, swapDisplay = false, hitkind=Vhit}

      fun loopV (x, y) visibleSprites =
          if x < 0.0 orelse x >= mapWidthR orelse y < 0.0 orelse y >= mapHeightR then
            (emptyHit,visibleSprites)
          else
            let
	      val wallX = Real.floor (if right then x else x - 1.0)
	      val wallY = Real.floor y
              val wt = Array2.sub(Map, wallY, wallX)
            in
	      if isWall wt then (* is this point inside a wall block? *)
                let val distX = x - !(#x player)
		    val distY = y - !(#y player)
	            val dist = distX*distX + distY*distY (* the distance from the player to this point, squared *)

                    (* the x-coord on the texture of the block, ie. what part of the texture are we going to render *)
		    val textureX = y - real (Real.floor y) (* where exactly are we on the wall? textureX is the x coordinate
                                                            * on the texture that we'll use later when texturing the wall. *)
                    val textureX = if not right then 1.0 - textureX else textureX (* if we're looking to the left side of the
                                                                                   * map, the texture should be reversed *)
                    val swapDisplay = rayAngle < Math.pi/2.0 orelse rayAngle > 3.0*Math.pi/2.0
                in (* save the coordinates of the hit. We only really use these to draw the rays on minimap *)
                  ({xHit=x, yHit=y, wallType=wt, dist=dist, xWallHit=wallX,
                    yWallHit=wallY, textureX=textureX, swapDisplay=swapDisplay,hitkind=Vhit},visibleSprites)
                end
              else
                loopV (x+dXVer,y+dYVer)
                      (case Array2.sub(spriteMap,wallY,wallX) of
                           NONE => visibleSprites
                         | SOME sprite => if !(#visible sprite) then visibleSprites
                                          else (#visible sprite := true; sprite::visibleSprites))
            end
      val (hit,visibleSprites) = loopV (x,y) visibleSprites

      (* now check against horizontal lines. It's basically the same, just "turned around".
       * the only difference here is that once we hit a map block,
       * we check if there we also found one in the earlier, vertical run. We'll know that if dist != 0.
       * If so, we only register this hit if this distance is smaller. *)

      val slope = angleCos / angleSin
      val dYHor = if up then ~1.0 else 1.0
      val dXHor = dYHor * slope
      val y = real(if up then Real.floor(!(#y player)) else Real.ceil(!(#y player)))
      val x = !(#x player) + (y - !(#y player)) * slope

      fun loopH (x,y) visibleSprites =
	  if x < 0.0 orelse x >= mapWidthR orelse y < 0.0 orelse y >= mapHeightR orelse slope > 10000.0 orelse (up andalso y-1.0 < 0.0) then
            (hit,visibleSprites)
          else
            let val wallY = Real.floor (if up then y - 1.0 else y)
	        val wallX = Real.floor x
                val wt = Array2.sub(Map,wallY,wallX)
                         handle ? =>
                                (log ("wallX = " ^ ppInt wallX ^ "; wallY = " ^ ppInt wallY); raise ?)
            in if isWall wt then
                 let val distX = x - !(#x player)
		     val distY = y - !(#y player)
                     val blockDist = distX*distX + distY*distY
                 in
		   if not (#dist hit > 0.0) orelse blockDist < #dist hit then
                     let val textureX = x - real(Real.floor x)
                         val textureX = if up then 1.0 - textureX else textureX
                         val textureX = 1.0-textureX
                         val swapDisplay = rayAngle < Math.pi
                     in ({dist=blockDist, xHit=x, yHit=y, xWallHit=wallX, yWallHit=wallY,
                          wallType=wt, textureX=textureX, swapDisplay=swapDisplay,hitkind=Hhit},visibleSprites)
                     end
                   else (hit,visibleSprites)
                 end
               else loopH (x+dXHor,y+dYHor)
                          (case Array2.sub(spriteMap,wallY,wallX) of
                               NONE => visibleSprites
                             | SOME sprite => if !(#visible sprite) then visibleSprites
                                              else (#visible sprite := true; sprite::visibleSprites))
            end
      val ({dist,xHit,yHit,xWallHit,yWallHit,wallType,textureX,swapDisplay,hitkind},visibleSprites) =
          loopH (x,y) visibleSprites
    in
      if dist <= 0.0 then visibleSprites
      else
	let (*val () = drawRay(xHit, yHit)*)
	    val	dist = Math.sqrt dist
	    (* use perpendicular distance to adjust for fish eye
	     * distorted_dist = correct_dist / cos(relative_angle_of_ray) *)
	    val dist = dist * Math.cos(!(#rot player) - rayAngle)

	    (* now calc the position, height and width of the wall strip *)

	    (* "Real" wall height in the game world is 1 unit, the distance from the player to the screen is viewDist,
	     * thus the height on the screen is equal to wall_height_real * viewDist / dist *)

	    val height = Real.round(viewDist / dist)

	    (* width is the same, but we have to stretch the texture to a factor of stripWidth to make it fill the strip correctly *)
	    val width = height * stripWidth
            val widthR = real width
            val heightR = real height
	    (* top placement is easy since everything is centered on the x-axis, so we simply move
	     * it half way down the screen and then half the wall height back up. *)
	    val top = Real.round((screenHeightR - heightR) / 2.0)
	    val texX = Real.round(textureX * widthR)
            val texX = if texX > width-stripWidth then width - stripWidth else texX

            val stripdata = Vector.sub(screenStrips, stripIdx)

            val wallType =
                if swapDisplay then
                  case wallType of DisplayLeft f => DisplayRight f
                                 | DisplayRight f => DisplayLeft f
                                 | _ => wallType
                else wallType

            fun compFile f =
                let val dir = case hitkind of
                                  Vhit => if !(#x player) > xHit then East else West
                                | Hhit => if !(#y player) > yHit then South else North
                in case f dir of
                       SOME sid =>
                       (case findDisplayState displayContent sid of
                            SOME {sid= _, slideIdx=ref i, pngbase=ref b} =>
                            let val file = "data/" ^ sid ^ "/" ^ b ^ "/" ^ b ^ "-" ^ prefixzeros 3 (Int.toString i) ^ ".png"
                            in file
                            end
                          | NONE => "wall-white.png")
                     | NONE => "wall-white.png"
                end
            val (numTextures, textureOffset, texX, factor) =
                case wallType of
                    DisplayLeft f => (Strip.setSrc stripdata (compFile f); (1,0,texX,2.0))
                  | DisplayRight f => (Strip.setSrc stripdata (compFile f); (1,0,texX+width,2.0))
                  | WindowLeft => (Strip.setSrc stripdata "window-left.png"; (1,0,texX,1.0))
                  | WindowCenter => (Strip.setSrc stripdata "window-center.png"; (1,0,texX,1.0))
                  | WindowRight => (Strip.setSrc stripdata "window-right.png"; (1,0,texX,1.0))
                  | Wall => (Strip.setSrc stripdata "wall-white.png"; (1,0,texX,1.0))
                  | ShelfLow => (Strip.setSrc stripdata "wall-shelf1.png"; (1,0,texX,1.0))
                  | ShelfHigh => (Strip.setSrc stripdata "wall-shelf2.png"; (1,0,texX,1.0))
                  | Whiteboard => (Strip.setSrc stripdata "wall-board.png"; (1,0,texX,1.0))
                  | Bulletin => (Strip.setSrc stripdata "wall-bulletin.png"; (1,0,texX,1.0))
                  | Wall1 => (Strip.setSrc stripdata "walls.png"; (4,0,texX,2.0))
                  | Wall2 => (Strip.setSrc stripdata "walls.png"; (4,1,texX,2.0))
                  | Wall3 => (Strip.setSrc stripdata "walls.png"; (4,2,texX,2.0))
                  | _ => raise Fail "obj not supported"

            val () = Strip.setHeight stripdata height
            val () = Strip.setTop stripdata top

            val imgTop = Real.floor(heightR * real textureOffset)
            val styleHeight = Real.floor(heightR * real numTextures)
            val () = Strip.setHeight stripdata styleHeight

            val styleWidth = Real.floor(widthR * factor)
            val () = Strip.setWidth stripdata styleWidth

            val styleTop = top - imgTop
            val () = Strip.setTop stripdata styleTop

            val styleLeft = stripIdx*stripWidth - texX
            val () = Strip.setLeft stripdata styleLeft

            val styleClip =
                "rect(" ^ ppInt imgTop ^ "," ^ ppInt(texX + stripWidth) ^
                "," ^ ppInt(imgTop + height) ^ "," ^ ppInt texX ^ ")"
            val () = Strip.setClip stripdata styleClip

	    val dwx = real xWallHit - !(#x player)
	    val dwy = real yWallHit - !(#y player)
	    val wallDist = dwx*dwx + dwy*dwy
	    val () = Strip.setZindex stripdata (~(Real.floor(wallDist*1000.0)))
        in visibleSprites
        end
    end

fun castRays displayContent screenStrips spriteMap =
    (* we accumulate the collected visible sprites *)
    let fun loop i acc =
            if i >= numRays then acc
            else
              let
                (* where on the screen does ray go through? *)
		val rayScreenPos = (~ (real numRays) / 2.0 + real i) * stripWidthR;

		(* the distance from the viewer to the point on the screen, simply Pythagoras *)
		val rayViewDist = Math.sqrt(rayScreenPos*rayScreenPos + viewDist*viewDist)

		(* the angle of the ray, relative to the viewing direction.
		 * right triangle: a = sin(A) * c *)
		val rayAngle = Math.asin(rayScreenPos / rayViewDist)
                val acc =
		    castSingleRay displayContent screenStrips spriteMap
                                  (!(#rot player) + rayAngle, (* add the players viewing direction to get the angle in world space *)
			           i, acc)
              in loop(i + 1) acc
              end
    in loop 0 []
    end

local
  val zero = Time.now()
in
fun now () =
    let val n = Time.now()
    in Time.toMilliseconds(Time.-(n,zero))
    end
end

fun gameCycle spriteMap =
    let val gameCycleDelay = 1000 div 30 (* aim for 30 FPS *)
        fun cycle lastGameCycleTime () =
            let val n = now()
                val timeDelta = n - lastGameCycleTime
                val _ = move spriteMap gameCycleDelay timeDelta
                val cycleDelay = if timeDelta > gameCycleDelay then
                                   IntInf.max(1, gameCycleDelay - (timeDelta - gameCycleDelay))
                                 else gameCycleDelay
            in Js.setTimeout (IntInf.toInt cycleDelay) (cycle n);
               ()
            end
    in cycle 0 ()
    end

fun textUpdater id pp =
    let val old = ref ""
    in fn v =>
          let val s = pp v
          in if !old = s then ()
             else
               let val e = $ id
               in Js.innerHTML e s; old := s
               end
          end
    end

val updateOverlay =
    textUpdater "overlay" (fn (fps,score) => "FPS: " ^ Real.fmt (StringCvt.FIX (SOME 0)) fps ^ " | SCORE: " ^ Int.toString score ^ " | " ^ deviceStr)

fun renderSprites oldSprites sprites =
    (List.app (fn s:Sprite.t =>
                  let val (x,y) = #pos s
                      val dx = real x + 0.5 - !(#x player)
                      val dy = real y + 0.5 - !(#y player)
                      val dist = Math.sqrt (dx*dx + dy*dy)   (* distance to sprite *)
                      val spriteAngle = Math.atan2(dy,dx) - !(#rot player) (* sprite angle relative to viewing angle *)
                      val size = viewDist / (Math.cos spriteAngle * dist)
                  in if size <= 0.0 then ()
                     else
                       let val img = #img s
                           val () = Js.setStyle img ("display","block")
                           val screen_x = Math.tan spriteAngle * viewDist (* x-position on screen *)
                           val left = Real.round (screenWidthR / 2.0 + screen_x - size / 2.0)
                           val top = Real.round ((screenHeightR-size)/2.0)
                           val () = Js.setStyle img ("left", ppInt left ^ "px")
                           val () = Js.setStyle img ("top", ppInt top ^ "px")
                           val dbx = real x - !(#x player)
                           val dby = real y - !(#y player)
                           val sz = ppInt (Real.round size) ^ "px"
                           val () = Js.setStyle img ("width", sz)
                           val () = Js.setStyle img ("height", sz)
                           val blockDist = dbx*dbx + dby*dby
                           val zIndex = ~ (Real.floor (blockDist*1000.0))
                           val () = Js.setStyle img ("zIndex", ppInt zIndex)
                       in ()
                              (* log ("rendering sprite: (" ^ ppInt x ^ "," ^ ppInt y ^ "); left=" ^ ppInt left ^ "; top=" ^ ppInt top ^ "; size=" ^ Real.toString size ^ "; sz=" ^ sz) *)
                       end
                  end) sprites;
     (* hide the sprites that are no longer visible *)
     List.app (fn s:Sprite.t => if not (!(#visible s)) then
                                  (Js.setStyle (#img s) ("display","none") (* ;
                                                                            log ("hiding sprite at (" ^ ppInt(#1(#pos s)) ^ "," ^ ppInt(#2(#pos s)) ^ ")") *)
                                  )
                                else () (* part of visible sprites *)
              ) oldSprites)


fun clearSprites sprites =
    List.app (fn s:Sprite.t => #visible s := false) sprites

val score = ref 0

fun renderCycle displayContent screenStrips spriteMap =
    let val renderCycleDelay = 1000 div 30 (* aim for 30 FPS *)
        fun cycle (lastRenderCycleTime,oldSprites) () =
            let val () = updateMiniMap()
                val () = clearSprites oldSprites
                val sprites = castRays displayContent screenStrips spriteMap
                val () = renderSprites oldSprites sprites
                val n = now()
                val timeDelta = n - lastRenderCycleTime
                val cycleDelay = if timeDelta > renderCycleDelay then
		                   IntInf.max(1, renderCycleDelay - (timeDelta - renderCycleDelay))
                                 else renderCycleDelay
                val cycleDelay = IntInf.toInt cycleDelay
	        val _ =	Js.setTimeout cycleDelay (cycle (n,sprites))
                val fps = 1000.0 / real (IntInf.toInt timeDelta)
            in updateOverlay (fps,!score)
            end
    in cycle (0,[]) ()
    end

fun initScreen () =
    let val screen = $ "screen"
        fun loop i =
            if i >= screenWidth then []
            else
              let val strip = Js.createElement "img"
                  val () = Js.setStyle strip ("position", "absolute")
                  val () = JsCore.setProperty (Js.Element.toForeignPtr strip) JsCore.string "src" "walls.png"
                  val stripdata = (strip,Strip.emptyData())
                  val () = Strip.setHeight stripdata 0
                  val () = Strip.setLeft stripdata 0
                  val () = Strip.setTop stripdata 0
                  val () = Js.appendChild ($"screen") strip
              in stripdata :: loop (i + stripWidth)
              end
    in loop 0
    end

fun png0FirstSlideDeck (staff:Staff.staff list) (sid:Staff.sid) : string =
    case List.find (fn {id,...} => id = sid) staff of
        SOME (s:Staff.staff) =>
        (case #slides s of
             nil => "default"
           | (sl:Staff.slide) :: _ => #id sl)
      | NONE => "default"

fun init () =
    let val staff = Staff.loadStaff(serverGet "data/staff.json")
                    handle Fail msg =>
                           ( log ("Failed to load data/staff.json file: " ^ msg)
                           ; nil)
        val screenStrips = Vector.fromList (initScreen())
        val spriteMap = Sprite.init (Sprite.mapItems Map)
        (* Each staff owns a display state *)
        val displayContent = map (fn sid => {sid=sid, slideIdx=ref 0,
                                             pngbase=ref (png0FirstSlideDeck staff sid)})
                                 staffids
        val () = bindKeys staff displayContent
    in drawMiniMap()
     ; gameCycle spriteMap
     ; renderCycle displayContent screenStrips spriteMap
    end

fun setWindowOnload (f: unit -> unit) : unit =
    let open JsCore infix ==>
    in exec1{arg1=("a", unit ==> unit),
             stmt="return window.onload=a;",
             res=unit} f
    end

in
val _ = setWindowOnload init
end
