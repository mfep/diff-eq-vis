module EtoUtils

open Eto.Forms
open Eto.Drawing

type TCell =
| El of Control
| StretchedEl of Control
| EmptyElement
| TableEl of Table
and TRow =
| Row of TCell list
| StretchedRow of TCell list
| Spacing of Size
| Pad of Padding
and Table = Tbl of TRow list

let rec makeLayout (Tbl t) =
    let ret = new TableLayout()
    for r in t do
        let makeTd (tds:TCell list) =
            let row = new TableRow()
            for td in tds do
                match td with
                | El c -> row.Cells.Add(new TableCell(c, false))
                | StretchedEl c -> row.Cells.Add(new TableCell(c, true))
                | EmptyElement -> row.Cells.Add(new TableCell(null, true))
                | TableEl t -> row.Cells.Add(new TableCell(makeLayout t, true))
            row
        match r with
        | Row tds -> let r = makeTd tds in ret.Rows.Add(r)
        | StretchedRow tds -> let r = makeTd tds in r.ScaleHeight <- true; ret.Rows.Add(r)
        | Spacing sz -> ret.Spacing <- sz
        | Pad pad -> ret.Padding <- pad
    ret

type Menu =
    | Item of MenuItem
    | ActionMenuItem of string
    | CheckMenuItem of string
    | SubMenu of string * Menu list
    | Action of Menu * (MenuItem -> unit)
    | Check of Menu * bool
    | Separator
    member m.WithAction cb = Action(m, cb)
    member m.WithCheck() = Check(m, true)

let rec makeMenu menu =
    match menu with
    | Item m -> m
    | ActionMenuItem lbl ->
        let m = new ButtonMenuItem(Text = lbl)
        m :> _
    | CheckMenuItem lbl ->
        let m = new CheckMenuItem(Text = lbl)
        m :> _
    | SubMenu (lbl, lst) ->
        let m = new ButtonMenuItem(Text = lbl)
        for el in lst do
            m.Items.Add(makeMenu el)
        m :> _
    | Action (m, cb) ->
        let ret = makeMenu m
        ret.Click.Add(fun _ -> cb ret)
        ret
    | Check (m, def) ->
        let ret = makeMenu m
        match ret with
        | :? CheckMenuItem as c -> c.Checked <- def
        | _ -> ()
        ret
    | Separator -> new SeparatorMenuItem() :> _