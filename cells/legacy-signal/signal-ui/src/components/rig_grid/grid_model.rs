//! Grid model for signal flow layout.
//!
//! Represents blocks positioned on a 16x8 grid with routing connections.
//! Blocks can span multiple cells (e.g., 3x2 for an EQ widget).

use signal_control::block::BlockType;
use uuid::Uuid;

/// Grid dimensions - 16 columns x 8 rows.
pub const GRID_COLS: usize = 16;
pub const GRID_ROWS: usize = 8;

/// Position on the grid (0-indexed).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GridPosition {
    pub row: usize,
    pub col: usize,
}

impl GridPosition {
    pub const fn new(row: usize, col: usize) -> Self {
        Self { row, col }
    }

    /// Check if position is within grid bounds.
    pub const fn is_valid(&self) -> bool {
        self.row < GRID_ROWS && self.col < GRID_COLS
    }
}

/// Size of a block on the grid (in cells).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GridSize {
    /// Width in cells (1-16).
    pub width: usize,
    /// Height in cells (1-8).
    pub height: usize,
}

impl GridSize {
    pub const fn new(width: usize, height: usize) -> Self {
        Self { width, height }
    }

    /// 1x1 single cell block.
    pub const fn single() -> Self {
        Self::new(1, 1)
    }

    /// 2x1 wide block.
    pub const fn wide() -> Self {
        Self::new(2, 1)
    }

    /// 3x2 large block (for EQ, compressor visualizations).
    pub const fn large() -> Self {
        Self::new(3, 2)
    }

    /// 4x2 extra large block.
    pub const fn xlarge() -> Self {
        Self::new(4, 2)
    }

    /// 2x2 square block.
    pub const fn square() -> Self {
        Self::new(2, 2)
    }
}

impl Default for GridSize {
    fn default() -> Self {
        Self::single()
    }
}

/// Widget type to render inside a block.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum BlockWidget {
    /// Simple colored label (default for small blocks).
    #[default]
    Label,
    /// EQ graph visualization.
    EqGraph,
    /// Compressor graph visualization.
    CompressorGraph,
    /// Gate graph visualization.
    GateGraph,
    /// Delay graph/visualization.
    DelayGraph,
    /// Reverb visualization.
    ReverbGraph,
    /// Amp/cab visualization.
    AmpCab,
    /// Drive/saturation visualization.
    DriveGraph,
    /// Modulation visualization (chorus, phaser, etc.).
    ModulationGraph,
    /// Tuner display.
    Tuner,
    /// Looper controls.
    Looper,
}

/// A block placed on the grid.
#[derive(Debug, Clone, PartialEq)]
pub struct GridBlock {
    /// Unique block ID.
    pub id: Uuid,
    /// Block name for display.
    pub name: String,
    /// Short label (2-3 chars) shown on block.
    pub short_label: String,
    /// Block type for color coding.
    pub block_type: BlockType,
    /// Position on the grid (top-left corner).
    pub position: GridPosition,
    /// Size in grid cells.
    pub size: GridSize,
    /// Widget to render inside.
    pub widget: BlockWidget,
    /// Whether the block is bypassed.
    pub bypassed: bool,
}

impl GridBlock {
    pub fn new(
        name: impl Into<String>,
        short_label: impl Into<String>,
        block_type: BlockType,
        position: GridPosition,
    ) -> Self {
        Self {
            id: Uuid::new_v4(),
            name: name.into(),
            short_label: short_label.into(),
            block_type,
            position,
            size: GridSize::single(),
            widget: BlockWidget::Label,
            bypassed: false,
        }
    }

    pub fn with_size(mut self, size: GridSize) -> Self {
        self.size = size;
        self
    }

    pub fn with_widget(mut self, widget: BlockWidget) -> Self {
        self.widget = widget;
        self
    }

    pub fn with_bypassed(mut self, bypassed: bool) -> Self {
        self.bypassed = bypassed;
        self
    }

    /// Check if this block occupies a specific cell.
    pub fn occupies(&self, pos: GridPosition) -> bool {
        pos.col >= self.position.col
            && pos.col < self.position.col + self.size.width
            && pos.row >= self.position.row
            && pos.row < self.position.row + self.size.height
    }
}

/// Connection between grid positions (for routing lines).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GridConnection {
    pub from: GridPosition,
    pub to: GridPosition,
}

impl GridConnection {
    pub const fn new(from: GridPosition, to: GridPosition) -> Self {
        Self { from, to }
    }

    /// Check if this is a horizontal connection (same row).
    pub const fn is_horizontal(&self) -> bool {
        self.from.row == self.to.row
    }

    /// Check if this is a vertical connection (same column).
    pub const fn is_vertical(&self) -> bool {
        self.from.col == self.to.col
    }
}

/// Input/output jack on the grid edge.
#[derive(Debug, Clone, PartialEq)]
pub struct GridJack {
    /// Jack identifier (e.g., "In 1", "Out 1/2").
    pub label: String,
    /// Row position (0-indexed).
    pub row: usize,
    /// Whether this is an input (left) or output (right).
    pub is_input: bool,
}

impl GridJack {
    pub fn input(label: impl Into<String>, row: usize) -> Self {
        Self {
            label: label.into(),
            row,
            is_input: true,
        }
    }

    pub fn output(label: impl Into<String>, row: usize) -> Self {
        Self {
            label: label.into(),
            row,
            is_input: false,
        }
    }
}

/// The complete grid state.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct SignalFlowGrid {
    /// Blocks placed on the grid.
    pub blocks: Vec<GridBlock>,
    /// Routing connections between positions.
    pub connections: Vec<GridConnection>,
    /// Input jacks on the left edge.
    pub inputs: Vec<GridJack>,
    /// Output jacks on the right edge.
    pub outputs: Vec<GridJack>,
}

impl SignalFlowGrid {
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a block to the grid.
    pub fn add_block(&mut self, block: GridBlock) {
        self.blocks.push(block);
    }

    /// Add a connection between two positions.
    pub fn connect(&mut self, from: GridPosition, to: GridPosition) {
        self.connections.push(GridConnection::new(from, to));
    }

    /// Add an input jack.
    pub fn add_input(&mut self, label: impl Into<String>, row: usize) {
        self.inputs.push(GridJack::input(label, row));
    }

    /// Add an output jack.
    pub fn add_output(&mut self, label: impl Into<String>, row: usize) {
        self.outputs.push(GridJack::output(label, row));
    }

    /// Get block at a specific position.
    pub fn block_at(&self, pos: GridPosition) -> Option<&GridBlock> {
        self.blocks.iter().find(|b| b.occupies(pos))
    }

    /// Check if a position has a block.
    pub fn has_block_at(&self, pos: GridPosition) -> bool {
        self.block_at(pos).is_some()
    }

    /// Create a sample guitar rig grid with various widget sizes.
    pub fn sample_guitar_rig() -> Self {
        let mut grid = Self::new();

        // Input/output jacks
        grid.add_input("In 1", 0);
        grid.add_input("In 2", 3);
        grid.add_output("Out 1/2", 0);
        grid.add_output("Out 3/4", 3);
        grid.add_output("Send 1/2", 6);

        // Row 0-1: Main signal chain with visualizations

        // Gate - small 1x1
        grid.add_block(GridBlock::new(
            "Gate",
            "GTE",
            BlockType::Gate,
            GridPosition::new(0, 1),
        ));

        // Compressor - 3x2 with visualization
        grid.add_block(
            GridBlock::new(
                "Compressor",
                "CMP",
                BlockType::Compressor,
                GridPosition::new(0, 2),
            )
            .with_size(GridSize::large())
            .with_widget(BlockWidget::CompressorGraph),
        );

        // EQ - 4x2 with interactive visualization
        grid.add_block(
            GridBlock::new("EQ", "EQ", BlockType::Eq, GridPosition::new(0, 6))
                .with_size(GridSize::xlarge())
                .with_widget(BlockWidget::EqGraph),
        );

        // Drive - 2x2
        grid.add_block(
            GridBlock::new("Drive", "DRV", BlockType::Drive, GridPosition::new(0, 10))
                .with_size(GridSize::square())
                .with_widget(BlockWidget::DriveGraph),
        );

        // Output - 1x1
        grid.add_block(GridBlock::new(
            "Volume",
            "VOL",
            BlockType::Volume,
            GridPosition::new(0, 13),
        ));

        // Row 2-3: Amp section
        grid.add_block(
            GridBlock::new("Amp", "AMP", BlockType::Amp, GridPosition::new(2, 2))
                .with_size(GridSize::square())
                .with_widget(BlockWidget::AmpCab),
        );

        grid.add_block(
            GridBlock::new(
                "Cabinet",
                "CAB",
                BlockType::Cabinet,
                GridPosition::new(2, 5),
            )
            .with_size(GridSize::square())
            .with_widget(BlockWidget::AmpCab),
        );

        // Row 4-5: Effects
        grid.add_block(
            GridBlock::new(
                "Chorus",
                "CHR",
                BlockType::Modulation,
                GridPosition::new(4, 2),
            )
            .with_size(GridSize::wide())
            .with_widget(BlockWidget::ModulationGraph),
        );

        grid.add_block(
            GridBlock::new("Delay", "DLY", BlockType::Delay, GridPosition::new(4, 5))
                .with_size(GridSize::large())
                .with_widget(BlockWidget::DelayGraph),
        );

        grid.add_block(
            GridBlock::new("Reverb", "REV", BlockType::Reverb, GridPosition::new(4, 9))
                .with_size(GridSize::large())
                .with_widget(BlockWidget::ReverbGraph),
        );

        // Row 6-7: Utilities
        grid.add_block(
            GridBlock::new("Tuner", "TNR", BlockType::Tuner, GridPosition::new(6, 2))
                .with_widget(BlockWidget::Tuner),
        );

        grid.add_block(
            GridBlock::new("Looper", "LPR", BlockType::Special, GridPosition::new(6, 4))
                .with_size(GridSize::wide())
                .with_widget(BlockWidget::Looper),
        );

        // Connections - simplified horizontal flow
        for row in [0, 2, 4, 6] {
            grid.connect(GridPosition::new(row, 0), GridPosition::new(row, 1));
        }

        grid
    }
}
