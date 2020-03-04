(** editorViewKind represents the type of editorView. This impacts, for
  * example, how expressions are tokenized within the view. *)
type viewKind =
  | MainView
  | FeatureFlagView
[@@deriving show {with_path = false}]

type t =
  { id : string
        (** the unique ID.t *of this editor panel, used to ID.t *entify it, eg, when
          * it is clicked and needs focus *)
  ; expressionId : ID.t  (** the id of the top-most expression in this panel *)
  ; kind : viewKind }
[@@deriving show {with_path = false}]
