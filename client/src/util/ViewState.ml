open Prelude

module TL = Toplevel
module TD = TLIDDict

type viewState =
  { tl : toplevel
  ; cursorState : cursorState
  ; tlid : tlid
  ; hovering : (tlid * id) option
  ; ac : autocomplete
  ; showEntry : bool
  ; showLivevalue : bool
  ; dbLocked : bool
  ; analysisStore : analysisStore (* for current selected trace *)
  ; traces : trace list
  ; dbStats : dbStatsStore
  ; ufns : userFunction list
  ; fns : function_ list
  ; executingFunctions : id list
  ; tlTraceIDs : tlTraceIDs
  ; testVariants : variantTest list
  ; featureFlags : flagsVS
  ; handlerProp : handlerProp option
  ; canvasName : string
  ; userContentHost : string
  ; refersToRefs : (toplevel * id list) list
  ; usedInRefs : toplevel list
  ; hoveringRefs : id list
  ; fluidState : Types.fluidState
  ; avatarsList : avatar list
  ; permission : permission option
  ; workerStats : workerStats option
  ; tokens :
      (* Calculate the tokens once per render only *)
      FluidToken.tokenInfo list
  ; menuState : menuState
  ; isExecuting : bool
  ; fnSpace: ufDnD}

let createVS (m : model) (tl : toplevel) (tokens : FluidToken.tokenInfo list) :
  viewState =
let tlid = TL.id tl in
let hp =
  match tl with TLHandler _ -> TD.get ~tlid m.handlerProps | _ -> None
in
let traceID = Analysis.getSelectedTraceID m tlid in
{ tl
; cursorState = unwrapCursorState m.cursorState
; tlid
; hovering =
    m.hovering
    |> List.filter ~f:(fun (tlid, _) -> tlid = tlid)
    |> List.head
    |> Option.andThen ~f:(fun ((_, i) as res) ->
           match idOf m.cursorState with
           | Some cur ->
               if cur = i then None else Some res
           | _ ->
               Some res)
; ac = m.complete
; showEntry = true
; showLivevalue = false
; dbLocked = DB.isLocked m tlid
; ufns = m.userFunctions |> TLIDDict.values
; fns = m.builtInFunctions
; analysisStore =
    Option.map traceID ~f:(Analysis.getStoredAnalysis m)
    |> Option.withDefault ~default:LoadableNotInitialized
; traces = Analysis.getTraces m tlid
; dbStats = m.dbStats
; executingFunctions =
    List.filter ~f:(fun (tlid_, _) -> tlid_ = tlid) m.executingFunctions
    |> List.map ~f:(fun (_, id) -> id)
; tlTraceIDs = m.tlTraceIDs
; testVariants = m.tests
; featureFlags = m.featureFlags
; handlerProp = hp
; canvasName = m.canvasName
; userContentHost = m.userContentHost
; refersToRefs =
    ( if tlidOf m.cursorState = Some tlid
    then Introspect.allRefersTo tlid m
    else [] )
; usedInRefs =
    ( if tlidOf m.cursorState = Some tlid
    then Introspect.allUsedIn tlid m
    else [] )
; hoveringRefs =
    TD.get ~tlid m.handlerProps
    |> Option.map ~f:(fun x -> x.hoveringReferences)
    |> Option.withDefault ~default:[]
; fluidState = m.fluidState
; avatarsList =
    ( match m.currentPage with
    | FocusedHandler (tlid_, _)
    | FocusedType tlid_
    | FocusedFn tlid_
    | FocusedDB (tlid_, _)
    | FocusedGroup (tlid_, _)
      when tlid_ = tlid ->
        m.avatarsList
    | _ ->
        [] )
; permission = m.permission
; workerStats =
    (* Right now we patch because worker execution link depends on name instead of TLID. When we fix our worker association to depend on TLID instead of name, then we will get rid of this patchy hack. *)
    (let count = TLIDDict.get ~tlid m.workerStats in
     let asWorkerSchedule = Handlers.getWorkerSchedule m in
     let schedule =
       tl |> TL.asHandler |> Option.andThen ~f:asWorkerSchedule
     in
     match (count, schedule) with
     | None, None ->
         None
     | Some c, None ->
         Some c
     | None, Some _ ->
         Some {Defaults.defaultWorkerStats with schedule}
     | Some c, Some _ ->
         Some {c with schedule})
; tokens
; menuState =
    TLIDDict.get ~tlid m.tlMenus
    |> Option.withDefault ~default:Defaults.defaultMenu
; isExecuting =
    (* Converge can execute for functions & handlers *)
    ( match tl with
    | TLFunc _ ->
        List.any ~f:(fun (fTLID, _) -> fTLID = tlid) m.executingFunctions
    | TLHandler _ ->
      (* Doing explicit match here just to be safe, even though we can probably assume you can't have handlerProp without it being a handler from code above. *)
      (match hp with Some p -> p.execution = Executing | _ -> false)
    | TLDB _ | TLTipe _ | TLGroup _ ->
        false )
; fnSpace = m.currentUserFn}