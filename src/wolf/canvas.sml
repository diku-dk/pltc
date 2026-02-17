 signature CANVAS = sig
  type t
  type elem = Js.elem
  val getContext  : elem -> string -> t
  val fillRect    : t -> int -> int -> int -> int -> unit
  val clearRect   : t -> int -> int -> int -> int -> unit
  val beginPath   : t -> unit
  val moveTo      : t -> int -> int -> unit
  val lineTo      : t -> int -> int -> unit
  val closePath   : t -> unit
  val stroke      : t -> unit
  val fillStyle   : t -> string -> unit
  val lineWidth   : t -> string -> unit
  val strokeStyle : t -> string -> unit
  val drawImage   : t -> elem ->
                    int -> int -> int -> int ->
                    int -> int -> int -> int -> unit
end

structure Canvas :> CANVAS = struct
  type t = foreignptr
  type elem = Js.elem
  fun getContext e s =
      let val e = Js.Element.toForeignPtr e
      in JsCore.exec2 {stmt="return e.getContext(s);", res=JsCore.fptr,
                       arg1=("e",JsCore.fptr),arg2=("s",JsCore.string)} (e, s)
      end
  fun fillRect c a1 a2 a3 a4 =
      JsCore.exec5 {stmt="c.fillRect(a1,a2,a3,a4);",
                    res=JsCore.unit,
                    arg1=("c",JsCore.fptr),
                    arg2=("a1",JsCore.int),
                    arg3=("a2",JsCore.int),
                    arg4=("a3",JsCore.int),
                    arg5=("a4",JsCore.int)} (c, a1, a2, a3, a4)
  fun clearRect c a1 a2 a3 a4 =
      JsCore.exec5 {stmt="c.clearRect(a1,a2,a3,a4);",
                    res=JsCore.unit,
                    arg1=("c",JsCore.fptr),
                    arg2=("a1",JsCore.int),
                    arg3=("a2",JsCore.int),
                    arg4=("a3",JsCore.int),
                    arg5=("a4",JsCore.int)} (c, a1, a2, a3, a4)
  fun beginPath c =
      JsCore.exec1 {stmt="c.beginPath();",
                    res=JsCore.unit,
                    arg1=("c",JsCore.fptr)} c
  fun closePath c =
      JsCore.exec1 {stmt="c.closePath();",
                    res=JsCore.unit,
                    arg1=("c",JsCore.fptr)} c
  fun stroke c =
      JsCore.exec1 {stmt="c.stroke();",
                    res=JsCore.unit,
                    arg1=("c",JsCore.fptr)} c
  fun moveTo c a1 a2 =
      JsCore.exec3 {stmt="c.moveTo(a1,a2);",
                    res=JsCore.unit,
                    arg1=("c",JsCore.fptr),
                    arg2=("a1",JsCore.int),
                    arg3=("a2",JsCore.int)} (c, a1, a2)
  fun lineTo c a1 a2 =
      JsCore.exec3 {stmt="c.lineTo(a1,a2);",
                    res=JsCore.unit,
                    arg1=("c",JsCore.fptr),
                    arg2=("a1",JsCore.int),
                    arg3=("a2",JsCore.int)} (c, a1, a2)
  fun fillStyle c s =
      JsCore.exec2 {stmt="c.fillStyle = s;",
                    res=JsCore.unit,
                    arg1=("c",JsCore.fptr),
                    arg2=("s",JsCore.string)} (c, s)
  fun lineWidth c s =
      JsCore.exec2 {stmt="c.lineWidth = s;",
                    res=JsCore.unit,
                    arg1=("c",JsCore.fptr),
                    arg2=("s",JsCore.string)} (c, s)
  fun strokeStyle c s =
      JsCore.exec2 {stmt="c.strokeStyle = s;",
                    res=JsCore.unit,
                    arg1=("c",JsCore.fptr),
                    arg2=("s",JsCore.string)} (c, s)

  fun drawImage c e x1 x2 x3 x4 x5 x6 x7 x8 =
      let val e = Js.Element.toForeignPtr e
      in JsCore.exec10 {stmt="c.drawImage(e,x1,x2,x3,x4,x5,x6,x7,x8);",
                        res=JsCore.unit,
                        arg1=("c",JsCore.fptr),
                        arg2=("e",JsCore.fptr),
                        arg3=("x1",JsCore.int),
                        arg4=("x2",JsCore.int),
                        arg5=("x3",JsCore.int),
                        arg6=("x4",JsCore.int),
                        arg7=("x5",JsCore.int),
                        arg8=("x6",JsCore.int),
                        arg9=("x7",JsCore.int),
                        arg10=("x8",JsCore.int)}
                       (c,e,x1,x2,x3,x4,x5,x6,x7,x8)
      end

end
