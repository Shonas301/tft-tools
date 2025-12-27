"""TFT hex grid configuration and position utilities."""

from dataclasses import dataclass
from typing import Tuple

import numpy as np
from numpy.typing import NDArray


@dataclass
class GridPosition:
    """represents a position on the TFT board or bench.

    the TFT board uses a hex grid with 4 rows and 7 columns.
    the bench is a single row with 9 slots.
    """

    row: int
    col: int

    def __post_init__(self) -> None:
        """validate grid position after initialization."""
        if not self.is_valid():
            raise ValueError(
                f"invalid grid position: row={self.row}, col={self.col}. "
                f"board: 0<=row<=3, 0<=col<=6; bench: row=4, 0<=col<=8"
            )

    def is_valid(self) -> bool:
        """check if this is a valid grid position.

        Returns:
            True if position is valid (on board or bench)
        """
        # board positions: rows 0-3, cols 0-6
        if 0 <= self.row <= 3:
            return 0 <= self.col <= 6
        # bench positions: row 4, cols 0-8
        if self.row == 4:
            return 0 <= self.col <= 8
        return False

    def is_board(self) -> bool:
        """check if this position is on the board (not bench).

        Returns:
            True if on board (rows 0-3)
        """
        return 0 <= self.row <= 3

    def is_bench(self) -> bool:
        """check if this position is on the bench.

        Returns:
            True if on bench (row 4)
        """
        return self.row == 4

    def to_tuple(self) -> Tuple[int, int]:
        """convert to (row, col) tuple.

        Returns:
            tuple of (row, col)
        """
        return (self.row, self.col)

    def __str__(self) -> str:
        """string representation."""
        location = "bench" if self.is_bench() else "board"
        return f"GridPosition({location}: row={self.row}, col={self.col})"

    def __hash__(self) -> int:
        """make hashable for use in dicts/sets."""
        return hash((self.row, self.col))


@dataclass
class GridConfig:
    """configuration for TFT board and bench grid positions.

    stores pixel coordinates for each grid position at a specific resolution.
    supports mapping between pixel coordinates and grid positions.
    """

    resolution: Tuple[int, int]  # (width, height)
    board_positions: dict[Tuple[int, int], Tuple[float, float]]
    bench_positions: dict[int, Tuple[float, float]]

    def get_pixel_position(self, grid_pos: GridPosition) -> Tuple[float, float]:
        """get pixel coordinates for a grid position.

        Args:
            grid_pos: grid position to look up

        Returns:
            tuple of (x, y) pixel coordinates

        Raises:
            KeyError: if grid position not found in configuration
        """
        if grid_pos.is_bench():
            if grid_pos.col not in self.bench_positions:
                raise KeyError(f"bench position {grid_pos.col} not configured")
            return self.bench_positions[grid_pos.col]

        pos_tuple = grid_pos.to_tuple()
        if pos_tuple not in self.board_positions:
            raise KeyError(f"board position {pos_tuple} not configured")
        return self.board_positions[pos_tuple]

    def snap_to_grid(
        self, x: float, y: float, board_only: bool = False
    ) -> GridPosition:
        """snap pixel coordinates to nearest grid position.

        Args:
            x: pixel x coordinate
            y: pixel y coordinate
            board_only: if True, only consider board positions (not bench)

        Returns:
            nearest GridPosition

        Raises:
            ValueError: if no positions configured
        """
        candidates = []

        # check board positions
        for (row, col), (px, py) in self.board_positions.items():
            dist = np.sqrt((x - px) ** 2 + (y - py) ** 2)
            candidates.append((dist, GridPosition(row, col)))

        # check bench positions (unless board_only)
        if not board_only:
            for col, (px, py) in self.bench_positions.items():
                dist = np.sqrt((x - px) ** 2 + (y - py) ** 2)
                candidates.append((dist, GridPosition(4, col)))

        if not candidates:
            raise ValueError("no grid positions configured")

        # return closest position
        _, closest_pos = min(candidates, key=lambda x: x[0])
        return closest_pos

    def batch_snap_to_grid(
        self, coordinates: NDArray[np.float64], board_only: bool = False
    ) -> list[GridPosition]:
        """snap multiple pixel coordinates to nearest grid positions.

        Args:
            coordinates: Nx2 array of (x, y) coordinates
            board_only: if True, only consider board positions

        Returns:
            list of GridPositions
        """
        return [
            self.snap_to_grid(float(x), float(y), board_only=board_only)
            for x, y in coordinates
        ]


def create_default_grid_1080p() -> GridConfig:
    """create default grid configuration for 1920x1080 resolution.

    this is a placeholder implementation. actual pixel coordinates should be
    determined by manually marking positions on a reference frame.

    Returns:
        GridConfig for 1080p resolution
    """
    # placeholder coordinates - these need to be calibrated from actual game frames
    # hex grid has offset rows (even rows shifted right)
    board_positions = {}

    # example hex grid layout (needs calibration)
    # assuming board is centered around x=960, y=540
    # with hex spacing of ~80 pixels horizontally, ~70 pixels vertically
    base_x = 600
    base_y = 300
    hex_width = 80
    hex_height = 70

    for row in range(4):
        for col in range(7):
            # hex grid offset: odd rows shifted right by half hex width
            offset_x = (hex_width // 2) if row % 2 == 1 else 0
            x = base_x + col * hex_width + offset_x
            y = base_y + row * hex_height
            board_positions[(row, col)] = (float(x), float(y))

    # bench positions (single row, evenly spaced)
    bench_positions = {}
    bench_base_x = 600
    bench_y = 900
    bench_spacing = 100

    for col in range(9):
        x = bench_base_x + col * bench_spacing
        bench_positions[col] = (float(x), float(bench_y))

    return GridConfig(
        resolution=(1920, 1080),
        board_positions=board_positions,
        bench_positions=bench_positions,
    )


def create_grid_from_calibration(
    resolution: Tuple[int, int],
    board_coords: dict[Tuple[int, int], Tuple[float, float]],
    bench_coords: dict[int, Tuple[float, float]],
) -> GridConfig:
    """create grid configuration from manually calibrated coordinates.

    Args:
        resolution: target resolution (width, height)
        board_coords: dict mapping (row, col) to (x, y) pixel coords
        bench_coords: dict mapping bench_slot to (x, y) pixel coords

    Returns:
        configured GridConfig

    Raises:
        ValueError: if coordinates are invalid or incomplete
    """
    # validate board has all 28 positions
    if len(board_coords) != 28:
        raise ValueError(
            f"board must have 28 positions, got {len(board_coords)}"
        )

    # validate bench has all 9 positions
    if len(bench_coords) != 9:
        raise ValueError(
            f"bench must have 9 positions, got {len(bench_coords)}"
        )

    # validate all positions are valid
    for row, col in board_coords:
        GridPosition(row, col)  # will raise if invalid

    for col in bench_coords:
        GridPosition(4, col)  # will raise if invalid

    return GridConfig(
        resolution=resolution,
        board_positions=board_coords,
        bench_positions=bench_coords,
    )
