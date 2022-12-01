import Lean

open Lean
open Server
open Widget
open RequestM
open Meta


/-! ## GameGoal -/

structure GameLocalDecl where
  userName : Name
  type : String
deriving FromJson, ToJson

structure GameGoal where
  objects : List GameLocalDecl
  assumptions : List GameLocalDecl
  goal : String
deriving FromJson, ToJson


def Lean.MVarId.toGameGoal (goal : MVarId) : MetaM GameGoal := do
match (← getMCtx).findDecl? goal with
| none          => throwError "unknown goal"
| some mvarDecl => do
  -- toGameGoalAux below will sort local declarations from the context of goal into data and assumptions,
  -- discarding auxilliary declarations
  let rec toGameGoalAux : List (Option LocalDecl) → MetaM (List LocalDecl × List LocalDecl)
  | (some decl)::t => withLCtx mvarDecl.lctx mvarDecl.localInstances do
    let (o, a) ← toGameGoalAux t
    if decl.isAuxDecl then
      return (o, a)
    if (← inferType decl.type).isProp then
      return (o, decl::a)
    else
      return (decl::o, a)
  | none:: t => toGameGoalAux t
  | [] => return ([], [])
  withLCtx mvarDecl.lctx mvarDecl.localInstances do
    let (objects, assumptions) ← toGameGoalAux mvarDecl.lctx.decls.toList
    let objects ← objects.mapM fun decl => do
      return { userName := decl.userName, type := toString (← Meta.ppExpr decl.type) }
    let assumptions ← assumptions.mapM fun decl => do
      return { userName := decl.userName, type := toString (← Meta.ppExpr decl.type) }
    return {objects := objects, assumptions := assumptions, goal := toString (← Meta.ppExpr mvarDecl.type) }


namespace GameServer


/-- `Game.getGoals` client<-server reply. -/
structure PlainGoal where
  /-- The pretty-printed goals, empty if all accomplished. -/
  goals : Array GameGoal
  deriving FromJson, ToJson

open Elab in
/--
  Try to retrieve `TacticInfo` for `hoverPos`.
  We retrieve all `TacticInfo` nodes s.t. `hoverPos` is inside the node's range plus trailing whitespace.
  We usually prefer the innermost such nodes so that for composite tactics such as `induction`, we show the nested proofs' states.
  However, if `hoverPos` is after the tactic, we prefer nodes that are not indented relative to it, meaning that e.g. at `|` in
  ```lean
  have := by
    exact foo
  |
  ```
  we show the (final, see below) state of `have`, not `exact`.

  Moreover, we instruct the LSP server to use the state after tactic execution if
  - the hover position is after the info's start position *and*
  - there is no nested tactic info after the hover position (tactic combinators should decide for themselves
    where to show intermediate states by calling `withTacticInfoContext`) -/
partial def goalsAt? (text : FileMap) (t : InfoTree) (hoverPos : String.Pos) : List GoalsAtResult :=
  let gs := t.collectNodesBottomUp fun ctx i cs gs => Id.run do
    if let Info.ofTacticInfo ti := i then
      if let (some pos, some tailPos) := (i.pos?, i.tailPos?) then
        let trailSize := i.stx.getTrailingSize
        -- show info at EOF even if strictly outside token + trail
        let atEOF := tailPos.byteIdx + trailSize == text.source.endPos.byteIdx
        -- include at least one trailing character (see also `priority` below)
        if pos ≤ hoverPos ∧ (hoverPos.byteIdx < tailPos.byteIdx + max 1 trailSize || atEOF) then
          -- overwrite bottom-up results according to "innermost" heuristics documented above
          if gs.isEmpty || hoverPos ≥ tailPos && gs.all (·.indented) then
            return [{
              ctxInfo := ctx
              tacticInfo := ti
              useAfter := hoverPos > pos && !cs.any (hasNestedTactic pos tailPos)
              indented := (text.toPosition pos).column > (text.toPosition hoverPos).column
              -- use goals just before cursor as fall-back only
              -- thus for `(by foo)`, placing the cursor after `foo` shows its state as long
              -- as there is no state on `)`
              priority := if hoverPos.byteIdx == tailPos.byteIdx + trailSize then 0 else 1
            }]
    return gs
  let maxPrio? := gs.map (·.priority) |>.maximum?
  gs.filter (some ·.priority == maxPrio?)
where
  hasNestedTactic (pos tailPos) : InfoTree → Bool
    | InfoTree.node i@(Info.ofTacticInfo _) cs => Id.run do
      if let `(by $_) := i.stx then
        return false  -- ignore term-nested proofs such as in `simp [show p by ...]`
      if let (some pos', some tailPos') := (i.pos?, i.tailPos?) then
        -- ignore preceding nested infos
        -- ignore nested infos of the same tactic, e.g. from expansion
        if tailPos' > hoverPos && (pos', tailPos') != (pos, tailPos) then
          return true
      cs.any (hasNestedTactic pos tailPos)
    | InfoTree.node (Info.ofMacroExpansionInfo _) cs =>
      cs.any (hasNestedTactic pos tailPos)
    | _ => false

def getGoals (p : Lsp.PlainGoalParams) : RequestM (RequestTask (Option PlainGoal)) := do
  let doc ← readDoc
  let text := doc.meta.text
  let hoverPos := text.lspPosToUtf8Pos p.position
    -- NOTE: use `>=` since the cursor can be *after* the input
  withWaitFindSnap doc (fun s => ¬ (goalsAt? doc.meta.text s.infoTree hoverPos).isEmpty)
    (notFoundX := return some {goals := #[{objects := [], assumptions := [], goal := "Hello"}]}) fun snap => do
      if let rs@(_ :: _) := snap.infoTree.goalsAt? doc.meta.text hoverPos then
        let goals ← rs.mapM fun { ctxInfo := ci, tacticInfo := ti, useAfter := useAfter, .. } => do
          let ci := if useAfter then { ci with mctx := ti.mctxAfter } else { ci with mctx := ti.mctxBefore }
          let goals := List.toArray <| if useAfter then ti.goalsAfter else ti.goalsBefore
          let goals ← ci.runMetaM {} $ goals.mapM fun goal => do
            return ← goal.toGameGoal
          return goals
        return some { goals := goals.foldl (· ++ ·) ∅ }
      else
        return some {goals := #[{objects := [], assumptions := [], goal := "no_info"}]}

builtin_initialize
  registerBuiltinRpcProcedure
    `Game.getGoals
    Lsp.PlainGoalParams
    (Option PlainGoal)
    getGoals

end GameServer
