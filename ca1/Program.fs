﻿open Eto.Forms
open Eto.Drawing
open EtoUtils
open EqParser

type DiffEq = {
    x' : (float * float) -> float;
    y' : (float * float) -> float
}
let testEq = {
    x' = fun (x, y) -> x - x*x*x - x * y*y - y;
    y' = fun (x, y) -> y - y * x*x - y*y*y + x;
}
let testEq2 = {
    x' = fun (x, y) -> x + 2.0 * y;
    y' = fun (x, y) -> -2.0 * x + y;
}

let app = new Application()
let form = new Form(Title = "Diffegyenletek", Size = Size(800, 800))
let draw = new Drawable()
let timer = new UITimer(Interval = 0.01)
let xTextBox = new TextBox()

let mutable pos = 0.0, 0.0
let maxPathLenght = 4096
let path = System.Collections.Generic.Queue()
let defaultMagni = 200.0f
let mutable magni = defaultMagni
let font = new Font(SystemFont.Default)

let posToScreenCoord (pos : float * float) (bounds : RectangleF) =
    PointF (magni * float32 (fst pos) + bounds.MiddleX, -magni * float32 (snd pos) + bounds.MiddleY)

let screenCoordToPos (coord : PointF) (bounds : RectangleF) =
    ((coord.X - bounds.MiddleX) / magni |> float, (coord.Y - bounds.MiddleY) / -magni |> float)

let updatePos() =
    let mul = 0.5 * timer.Interval
    let delta = (testEq.x' pos, testEq.y' pos)
    pos <- (fst pos + fst delta * mul, snd pos + snd delta * mul)

let updatePath() =
    path.Enqueue pos
    if path.Count > maxPathLenght then path.Dequeue() |> ignore

let drawCoordSys (graphics : Graphics) =
    let bounds = graphics.ClipBounds
    let coordSysColor = Colors.Black
    let tickPxSize = 2.0f
    
    graphics.DrawLine(coordSysColor, bounds.MiddleBottom, bounds.MiddleTop)
    graphics.DrawLine(coordSysColor, bounds.MiddleLeft, bounds.MiddleRight)
    
    let up i = bounds.Center + PointF(float32 i * magni, tickPxSize)
    let down i = bounds.Center + PointF(float32 i * magni, -tickPxSize)
    let textPosX i = bounds.Center + PointF(float32 i * magni, tickPxSize)

    let left i = bounds.Center + PointF(-tickPxSize, float32 i * magni)
    let right i = bounds.Center + PointF(tickPxSize, float32 i * magni)
    let textPosY i = bounds.Center + PointF(tickPxSize, float32 i * magni)

    let drawTicks p1 p2 pt middle nummul =
        let ticks = middle / magni |> int
        for ix = -ticks to ticks do
            if ix <> 0 then
                graphics.DrawLine(coordSysColor, p1 ix, p2 ix)
                graphics.DrawText(font, coordSysColor, pt ix, sprintf "%i" (ix * nummul))

    drawTicks up down textPosX bounds.MiddleX 1
    drawTicks left right textPosY bounds.MiddleY -1


let drawPath (graphics : Graphics) =
    let pathColor = Colors.Blue
    path
    |> Seq.map (fun pos -> posToScreenCoord pos graphics.ClipBounds)
    |> Seq.pairwise
    |> Seq.iter (fun (pos1, pos2) -> graphics.DrawLine(pathColor, pos1, pos2))

let drawPos pos (graphics : Graphics) =
    let circleRad = 2.0f
    let circleColor = Colors.Blue
    let screenCoord = posToScreenCoord pos graphics.ClipBounds
    graphics.DrawEllipse(circleColor, screenCoord.X - circleRad, screenCoord.Y - circleRad, circleRad * 2.0f, circleRad * 2.0f)

let mutable globBounds = RectangleF()

let paint (graphics : Graphics) =
    graphics.AntiAlias <- false
    globBounds <- graphics.ClipBounds

    drawCoordSys graphics
    drawPos pos graphics
    drawPath graphics

let mouseClick (coord : PointF) =
    path.Clear()
    pos <- screenCoordToPos coord globBounds
    updatePath()

xTextBox.TextChanged.Add (fun _ ->
    try
        parse xTextBox.Text |> ignore
        xTextBox.BackgroundColor <- Colors.White
    with
    | _ -> System.Console.WriteLine "gebasz" ; xTextBox.BackgroundColor <- Colors.OrangeRed
)

let menu = new MenuBar()

let fileMenu = SubMenu("File", [
                        ActionMenuItem("Exit").WithAction(fun _ -> app.Quit())
                      ])
menu.Items.Add(fileMenu |> makeMenu)

let changeMagni mul =
    let minMagni = 50.0f
    let maxMagni = 500.0f
    magni <- magni * mul
    magni <- min maxMagni (max minMagni magni)

let viewMenu = SubMenu("View", [
                        ActionMenuItem("Zoom In").WithAction(fun _ -> changeMagni 1.3333f);
                        ActionMenuItem("Zoom Out").WithAction(fun _ -> changeMagni 0.75f);
                        ActionMenuItem("Default Zoom").WithAction(fun _ -> magni <- defaultMagni)
                      ])
menu.Items.Add(viewMenu |> makeMenu)

let layout =
    Tbl [
        StretchedRow [StretchedEl (draw)];
        Row [TableEl (Tbl [
                        Pad (Padding(4))
                        Spacing (Size(0, 2))
                        Row [El (new Label(Text = "x'=")) ; StretchedEl(xTextBox)]
                        Row [El (new Label(Text = "y'=")) ; StretchedEl(new TextBox())]
                        ])]
        ] |> makeLayout

draw.Paint.Add (fun args -> paint args.Graphics)
draw.MouseDown.Add (fun args -> mouseClick args.Location)
draw.MouseWheel.Add (fun args -> changeMagni (args.Delta.Height / 3.0f + 1.0f))

timer.Elapsed.Add(fun _ -> updatePos() ; updatePath() ; draw.Invalidate())
timer.Start()

form.Content <- layout
form.Menu <- menu

[<EntryPoint>]
let main argv =
    app.Run(form)
    0
