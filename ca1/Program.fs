open Eto.Forms
open Eto.Drawing
open EtoUtils
open EqParser

let app = new Application()
let form = new Form(Title = "Differential equation visualizer", Size = Size(800, 800))
let draw = new Drawable(ToolTip = "Click to set initial condition")
let timer = new UITimer(Interval = 0.01)
let font = new Font(FontFamilies.Monospace, 12.0f)
let xTextBox = new TextBox(Text = defaultX', Font = font)
let yTextBox = new TextBox(Text = defaultY', Font = font)
let speedSlider = new Slider(Orientation = Orientation.Vertical, MinValue = 0, MaxValue = 100, TickFrequency = 1, Value = 10, ToolTip = "Adjust speed of simulation")

let mutable pos = 1.0, 1.0
let maxPathLenght = 4096
let path = System.Collections.Generic.Queue()
let defaultMagni = 200.0f
let mutable magni = defaultMagni
let mutable globBounds = RectangleF()
let mutable polyX' = parse defaultX'
let mutable polyY' = parse defaultY'
let mutable speed = 1.0

let calcDeriv point =
    calculate point polyX', calculate point polyY'

let clearPosPath() =
    path.Clear() ; pos <- 0.0, 0.0

let posToScreenCoord (pos : float * float) (bounds : RectangleF) =
    PointF (magni * float32 (fst pos) + bounds.MiddleX, -magni * float32 (snd pos) + bounds.MiddleY)

let screenCoordToPos (coord : PointF) (bounds : RectangleF) =
    ((coord.X - bounds.MiddleX) / magni |> float, (coord.Y - bounds.MiddleY) / -magni |> float)

let isOnScreen p =
    let screenPos = posToScreenCoord p globBounds
    abs screenPos.X < defaultMagni * 10.0f && abs screenPos.Y < defaultMagni * 10.0f

let updatePos() =
    let mul = speed * timer.Interval
    let delta = calcDeriv pos
    pos <- (fst pos + fst delta * mul, snd pos + snd delta * mul)
    if not (isOnScreen pos) then clearPosPath()

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

let drawLine (dirX, dirY) (graphics : Graphics) =
    let mul = 1000.0
    let b = posToScreenCoord (dirX * mul, dirY * mul) graphics.ClipBounds
    let e = posToScreenCoord (dirX * -mul, dirY * -mul) graphics.ClipBounds
    graphics.DrawLine(Colors.LightBlue, b, e)

let paint (graphics : Graphics) =
    graphics.AntiAlias <- false
    globBounds <- graphics.ClipBounds

    drawCoordSys graphics
    drawPos pos graphics
    drawPath graphics

let mouseClick (coord : PointF) =
    clearPosPath()
    pos <- screenCoordToPos coord globBounds
    updatePath()

let sliderChanged value =
    speed <- value * 0.1

xTextBox.TextChanged.Add (fun _ ->
    try
        polyX' <- parse xTextBox.Text
        clearPosPath()
        xTextBox.BackgroundColor <- Colors.White
    with
    | _ -> xTextBox.BackgroundColor <- Colors.OrangeRed
)

yTextBox.TextChanged.Add(fun _ ->
    try
        polyY' <- parse yTextBox.Text
        clearPosPath()
        yTextBox.BackgroundColor <- Colors.White
    with
    | _ -> yTextBox.BackgroundColor <- Colors.OrangeRed
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
        StretchedRow [TableEl (Tbl [
                                    StretchedRow [StretchedEl (draw); El(speedSlider)]
        ])]
        Row [TableEl (Tbl [
                        Pad (Padding(4))
                        Spacing (Size(0, 2))
                        Row [El (new Label(Text = "x'=")) ; StretchedEl(xTextBox)]
                        Row [El (new Label(Text = "y'=")) ; StretchedEl(yTextBox)]
                        ])]
        ] |> makeLayout

draw.Paint.Add (fun args -> paint args.Graphics)
draw.MouseDown.Add (fun args -> mouseClick args.Location ; draw.Focus())
draw.MouseWheel.Add (fun args -> changeMagni (args.Delta.Height / 3.0f + 1.0f))

timer.Elapsed.Add(fun _ -> updatePos() ; updatePath() ; draw.Invalidate())
timer.Start()

speedSlider.ValueChanged.Add(fun _ -> sliderChanged (float speedSlider.Value))

form.Content <- layout
form.Menu <- menu

[<EntryPoint; System.STAThread>]
let main argv =
    app.Run(form)
    0
