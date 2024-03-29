if (FALSE)
{
    MODES <- list(character = "character", numeric = "numeric")
    
    list(
        ENERGY = list(
            `Global Efficiency` = MODES$numeric, 
            `Global Price` = MODES$numeric, 
            `Demand Charge` = MODES$numeric
        ), 
        REACTIONS = list(
            `Order Bulk` = MODES$numeric, 
            `Order Tank` = MODES$numeric, 
            `Order Wall` = MODES$numeric, 
            `Global Bulk` = MODES$numeric, 
            `Global Wall` = MODES$numeric, 
            `Limiting Potential` = MODES$numeric, 
            `Roughness Correlation` = MODES$numeric
        ), 
        TIMES = list(
            Duration = MODES$numeric, 
            `Hydraulic Timestep` = MODES$character, 
            `Quality Timestep` = MODES$character, 
            `Pattern Timestep` = MODES$character, 
            `Pattern Start` = MODES$character, 
            `Report Timestep` = MODES$character, 
            `Report Start` = MODES$character, 
            `Start ClockTime` = MODES$character, 
            Statistic = MODES$character
        ), 
        REPORT = list(
            Status = MODES$character, 
            Summary = MODES$character, 
            Page = MODES$numeric
        ), 
        OPTIONS = list(
            Units = MODES$character, 
            Headloss = MODES$character, 
            `Specific Gravity` = MODES$numeric, 
            Viscosity = MODES$numeric, 
            Trials = MODES$numeric, 
            Accuracy = MODES$numeric, 
            CHECKFREQ = MODES$numeric, 
            MAXCHECK = MODES$numeric, 
            DAMPLIMIT = MODES$numeric, 
            Unbalanced = MODES$character, 
            Pattern = MODES$numeric, 
            `Demand Multiplier` = MODES$numeric, 
            `Emitter Exponent` = MODES$numeric, 
            Quality = MODES$character, 
            Diffusivity = MODES$numeric, 
            Tolerance = MODES$numeric
        ), 
        BACKDROP = list(
            DIMENSIONS = MODES$character, 
            UNITS = MODES$character, 
            FILE = MODES$character, 
            OFFSET = MODES$character
        )
    )
    list(
        JUNCTIONS = list(
            ID = MODES$character, 
            Elev = MODES$numeric, 
            Demand = MODES$numeric, 
            Pattern = MODES$character
        ), 
        RESERVOIRS = list(
            ID = MODES$character, 
            Head = MODES$numeric, 
            Pattern = MODES$character
        ), 
        TANKS = list(
            ID = MODES$character, 
            Elevation = MODES$numeric, 
            InitLevel = MODES$numeric, 
            MinLevel = MODES$numeric, 
            MaxLevel = MODES$numeric, 
            Diameter = MODES$numeric, 
            MinVol = MODES$numeric, 
            VolCurve = MODES$character
        ), 
        PIPES = list(
            ID = MODES$character, 
            Node1 = MODES$character, 
            Node2 = MODES$character, 
            Length = MODES$numeric, 
            Diameter = MODES$numeric, 
            Roughness = MODES$numeric, 
            MinorLoss = MODES$numeric, 
            Status = MODES$character
        ), 
        PUMPS = list(
            ID = MODES$character, 
            Node1 = MODES$character, 
            Node2 = MODES$character, 
            Parameters = MODES$character
        ), 
        VALVES = list(
            ID = MODES$character, 
            Node1 = MODES$character, 
            Node2 = MODES$character, 
            Diameter = MODES$numeric, 
            Type = MODES$character,
            Setting = MODES$character, 
            MinorLoss = MODES$numeric
        ), EMITTERS = list(
            Junction = MODES$character, 
            Coefficient = MODES$numeric
        ), 
        CURVES = list(
            ID = MODES$character,
            `X-Value` = MODES$numeric, 
            `Y-Value` = MODES$numeric
        ), 
        PATTERNS = list(
            ID = MODES$character, 
            Multipliers = MODES$character
        ), 
        STATUS = list(
            ID = MODES$character, 
            `Status/Setting` = MODES$character
        ), 
        DEMANDS = list(
            Junction = MODES$character, 
            Demand = MODES$numeric, 
            Pattern = MODES$character, 
            Category = MODES$character
        ), 
        QUALITY = list(
            Node = MODES$character, 
            InitQual = MODES$character
        ), 
        REACTIONS = list(
            Type = MODES$character, 
            `Pipe/Tank` = MODES$character, 
            Coefficient = MODES$character
        ), 
        SOURCES = list(
            Node = MODES$character, 
            Type = MODES$character, 
            Quality = MODES$numeric, 
            Pattern = MODES$character
        ), 
        MIXING = list(
            Tank = MODES$character, 
            Model = MODES$character
        ), 
        COORDINATES = list(
            Node = MODES$character, 
            `X-Coord` = MODES$numeric, 
            `Y-Coord` = MODES$numeric
        ), 
        VERTICES = list(
            Link = MODES$character, 
            `X-Coord` = MODES$numeric, 
            `Y-Coord` = MODES$numeric
        ), 
        LABELS = list(
            `X-Coord` = MODES$numeric, 
            `Y-Coord` = MODES$numeric, 
            `Label & Anchor Node` = MODES$character
        )
    )
}
