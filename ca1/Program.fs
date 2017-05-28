open Eto.Forms
open Eto.Drawing
open EtoUtils
open EqParser
open DeSolver

let app = new Application()
let form = new Form(Title = "Differential equation visualizer", Size = Size(800, 800))
let draw = new Drawable(ToolTip = "Click to set initial condition")
let timer = new UITimer(Interval = 0.01)
let font = new Font(FontFamilies.Monospace, 12.0f)
let xTextBox = new TextBox(Text = defaultX', Font = font)
let yTextBox = new TextBox(Text = defaultY', Font = font)
let iterSlider = new Slider(Orientation = Orientation.Horizontal, MinValue = 1, MaxValue = 50, Value = 10, ToolTip = "Number of iterations per visual step", SnapToTick = true)
let iterTextBox = new TextBox(ReadOnly = true, Text = sprintf "%d" iterSlider.Value, Width = 80)
let stepSlider = new Slider(Orientation = Orientation.Horizontal, MinValue = 1, MaxValue = 100, Value = 1)
let step() =
    float stepSlider.Value * 0.0005
let stepTextBox = new TextBox(ReadOnly = true, Width = 30, Text = sprintf "%f" (step()))
let antialiasCheck = new CheckMenuItem(Text = "Antialiasing")

let mutable pos = 1.0, 1.0
let mutable maxPathLenght = 1024
let path = System.Collections.Generic.Queue()
let defaultMagni = 200.0f
let mutable magni = defaultMagni
let mutable globBounds = RectangleF()
let mutable polyX' = parse defaultX'
let mutable polyY' = parse defaultY'

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

let iterSolver point =
    let iterationCount = iterSlider.Value
    let rec loop p i =
        if i > iterationCount then p
        else loop (rk4step (step()) calcDeriv p) (i+1)
    loop point 1

let updatePos() =
    pos <- iterSolver pos
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

let paint (graphics : Graphics) =
    graphics.AntiAlias <- antialiasCheck.Checked
    globBounds <- graphics.ClipBounds

    drawCoordSys graphics
    drawPos pos graphics
    drawPath graphics

let mouseClick (coord : PointF) =
    clearPosPath()
    pos <- screenCoordToPos coord globBounds
    updatePath()

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

let changeMaxPathCallback (str : string) =
    try
        let x = System.UInt16.Parse(str)
        maxPathLenght <- x |> int
        true
    with | _ -> MessageBox.Show("Input should be a non-negative integer", MessageBoxType.Error) |> ignore ; false

let viewMenu = SubMenu("View", [
                        ActionMenuItem("Zoom In").WithAction(fun _ -> changeMagni 1.3333f)
                        ActionMenuItem("Zoom Out").WithAction(fun _ -> changeMagni 0.75f)
                        ActionMenuItem("Default Zoom").WithAction(fun _ -> magni <- defaultMagni)
                        Separator
                        Item antialiasCheck
                        ActionMenuItem("Set path length").WithAction(fun _ -> inputDialog "Set path length" "Set path length" (maxPathLenght.ToString()) changeMaxPathCallback)
                      ])
menu.Items.Add(viewMenu |> makeMenu)

let layout =
    Tbl [
        StretchedRow [StretchedEl (draw)]
        Row [TableEl (Tbl [
                        Pad (Padding(4))
                        Spacing (Size(8, 2))
                        Row [El(null) ; El (new Label(Text = "x'=", Font = font, VerticalAlignment = VerticalAlignment.Center, TextAlignment = TextAlignment.Right)) ; StretchedEl(xTextBox)]
                        Row [El(null) ;El (new Label(Text = "y'=", Font = font, VerticalAlignment = VerticalAlignment.Center, TextAlignment = TextAlignment.Right)) ; StretchedEl(yTextBox)]
                        Row [El (new Label(Text = "Visual step:", VerticalAlignment = VerticalAlignment.Center)) ; El (iterTextBox) ; StretchedEl(iterSlider)]
                        Row [El (new Label(Text = "Solver step:", VerticalAlignment = VerticalAlignment.Center)) ; El (stepTextBox) ; StretchedEl(stepSlider)]
                        ])]
        ] |> makeLayout

draw.Paint.Add (fun args -> paint args.Graphics)
draw.MouseDown.Add (fun args -> mouseClick args.Location ; draw.Focus())
draw.MouseWheel.Add (fun args -> changeMagni (args.Delta.Height / 3.0f + 1.0f))

timer.Elapsed.Add(fun _ -> updatePos() ; updatePath() ; draw.Invalidate())
timer.Start()

iterSlider.ValueChanged.Add (fun _ -> iterTextBox.Text <- sprintf "%d" iterSlider.Value)
stepSlider.ValueChanged.Add (fun _ -> stepTextBox.Text <- sprintf "%f" (step ()))

form.Content <- layout
form.Menu <- menu

[<EntryPoint; System.STAThread>]
let main argv =
    app.Run(form)
    0
