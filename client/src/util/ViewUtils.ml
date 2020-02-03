open Prelude
module Svg = Tea.Svg
module Regex = Util.Regex
module TL = Toplevel
module TD = TLIDDict

(* ----------------------------- *)
(* Events *)
(* ----------------------------- *)
type domEvent = msg Vdom.property

type domEventList = domEvent list

let fontAwesome (name : string) : msg Html.html =
  Html.i [Html.class' ("fa fa-" ^ name)] []


let decodeClickEvent (fn : mouseEvent -> 'a) j : 'a =
  let open Json.Decode in
  fn
    { mePos = {vx = field "pageX" int j; vy = field "pageY" int j}
    ; button = field "button" int j
    ; ctrlKey = field "ctrlKey" bool j
    ; shiftKey = field "shiftKey" bool j
    ; altKey = field "altKey" bool j
    ; detail = field "detail" int j }


let decodeTransEvent (fn : string -> 'a) j : 'a =
  let open Json.Decode in
  fn (field "propertyName" string j)


let decodeAnimEvent (fn : string -> 'a) j : 'a =
  let open Json.Decode in
  fn (field "animationName" string j)


let decodeChangePosEvent (fn : (int * int) -> 'a) j : 'a =
  let open Json.Decode in
  let decodeChange j =
    (field "oldPos" int j, field "newPos" int j)
  in
  fn (field "detail" decodeChange j)

(*  
let decodeDragEvent (fn: 'a -> 'b) : 'b =
  let open Tea.Json.Decoder in
  fn (Decoder (fun json -> Tea_result.Ok (Obj.magic json)))
*)

let eventBoth ~(key : string) (event : string) (constructor : mouseEvent -> msg)
    : msg Vdom.property =
  Tea.Html.onWithOptions
    ~key
    event
    {stopPropagation = false; preventDefault = false}
    (Decoders.wrapDecoder (decodeClickEvent constructor))


let eventPreventDefault
    ~(key : string) (event : string) (constructor : mouseEvent -> msg) :
    msg Vdom.property =
  Tea.Html.onWithOptions
    ~key
    event
    {stopPropagation = false; preventDefault = true}
    (Decoders.wrapDecoder (decodeClickEvent constructor))


let eventNeither
    ~(key : string) (event : string) (constructor : mouseEvent -> msg) :
    msg Vdom.property =
  Tea.Html.onWithOptions
    ~key
    event
    {stopPropagation = true; preventDefault = true}
    (Decoders.wrapDecoder (decodeClickEvent constructor))


let eventNoPropagation
    ~(key : string) (event : string) (constructor : mouseEvent -> msg) :
    msg Vdom.property =
  Tea.Html.onWithOptions
    ~key
    event
    {stopPropagation = true; preventDefault = false}
    (Decoders.wrapDecoder (decodeClickEvent constructor))


let onTransitionEnd ~(key : string) ~(listener : string -> msg) :
    msg Vdom.property =
  Tea.Html.onWithOptions
    ~key
    "transitionend"
    {stopPropagation = false; preventDefault = true}
    (Decoders.wrapDecoder (decodeTransEvent listener))


let onAnimationEnd ~(key : string) ~(listener : string -> msg) :
    msg Vdom.property =
  Tea.Html.onWithOptions
    ~key
    "animationend"
    {stopPropagation = false; preventDefault = true}
    (Decoders.wrapDecoder (decodeAnimEvent listener))

(* Generic event, the the listener handle and do what it wants with the event object *)
let onEvent ~(event : string) ~(key : string) ?(preventDefault = true) (listener: Web.Node.event -> msg) : msg Vdom.property =
  Tea.Html.onCB
      event
      key
      (fun evt ->
        if preventDefault then evt##preventDefault () ;
        Some (listener evt)
      )

let nothingMouseEvent (name : string) : msg Vdom.property =
  eventNoPropagation ~key:"" name (fun _ -> IgnoreMsg)

let placeHtml (pos : pos) (classes : 'a list) (html : msg Html.html list) :
    msg Html.html =
  let styles =
    Html.styles
      [("left", string_of_int pos.x ^ "px"); ("top", string_of_int pos.y ^ "px")]
  in
  Html.div [Html.classList (("node", true) :: classes); styles] html


let inCh (w : int) : string = w |> string_of_int |> fun s -> s ^ "ch"

let widthInCh (w : int) : msg Vdom.property = w |> inCh |> Html.style "width"

let svgIconFn (color : string) : msg Html.html =
  Svg.svg
    [ Svg.Attributes.viewBox "0 0 16 16"
    ; Svg.Attributes.width "16"
    ; Svg.Attributes.height "16" ]
    [ Svg.g
        [Svg.Attributes.fill color]
        [ Svg.path
            [ Svg.Attributes.d
                "M5,5.62A4.38,4.38,0,0,1,9.44,1.31h.35V3.63H9.44a2,2,0,0,0-2.1,2V6.78H9.79V9.12H7.34V11A4.38,4.38,0,0,1,2.9,15.31H2.55V13H2.9A2,2,0,0,0,5,11V9.12H3.84V6.78H5Z"
            ]
            []
        ; Svg.path
            [ Svg.Attributes.d
                "M12.89,9.91l.76.75-1.48,1.48,1.48,1.48-.76.76L11.41,12.9,9.93,14.38l-.75-.76,1.48-1.48L9.18,10.66l.75-.75,1.48,1.48Z"
            ]
            [] ] ]


let createHandlerProp (hs : handler list) : handlerProp TD.t =
  hs
  |> List.map ~f:(fun h -> (h.hTLID, Defaults.defaultHandlerProp))
  |> TD.fromList


let getHandlerState (vs : ViewState.viewState) : handlerState =
  match vs.handlerProp with
  | Some p ->
      p.handlerState
  | None ->
      Defaults.defaultHandlerProp.handlerState


let isHoverOverTL (vs : ViewState.viewState) : bool =
  match vs.hovering with
  | Some (tlid, _id) when tlid = TL.id vs.tl ->
      true
  | _ ->
      false


let intAsUnit (i : int) (u : string) : string = string_of_int i ^ u
