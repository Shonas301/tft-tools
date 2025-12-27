"""unit tests for TFT grid configuration."""

import numpy as np
import pytest

from tft_vod_analysis.data.grid_config import (
    GridConfig,
    GridPosition,
    create_default_grid_1080p,
    create_grid_from_calibration,
)


class TestGridPosition:
    """tests for GridPosition class."""

    def test_valid_board_position(self) -> None:
        """test creating valid board positions."""
        # corner positions
        pos = GridPosition(0, 0)
        assert pos.row == 0
        assert pos.col == 0
        assert pos.is_board()
        assert not pos.is_bench()

        pos = GridPosition(3, 6)
        assert pos.is_board()

    def test_valid_bench_position(self) -> None:
        """test creating valid bench positions."""
        pos = GridPosition(4, 0)
        assert pos.row == 4
        assert pos.col == 0
        assert pos.is_bench()
        assert not pos.is_board()

        pos = GridPosition(4, 8)
        assert pos.is_bench()

    def test_invalid_position_negative(self) -> None:
        """test invalid positions with negative indices."""
        with pytest.raises(ValueError, match="invalid grid position"):
            GridPosition(-1, 0)

        with pytest.raises(ValueError, match="invalid grid position"):
            GridPosition(0, -1)

    def test_invalid_position_out_of_bounds(self) -> None:
        """test invalid positions out of bounds."""
        # row too high
        with pytest.raises(ValueError, match="invalid grid position"):
            GridPosition(5, 0)

        # col too high for board
        with pytest.raises(ValueError, match="invalid grid position"):
            GridPosition(0, 7)

        # col too high for bench
        with pytest.raises(ValueError, match="invalid grid position"):
            GridPosition(4, 9)

    def test_to_tuple(self) -> None:
        """test converting GridPosition to tuple."""
        pos = GridPosition(2, 3)
        assert pos.to_tuple() == (2, 3)

    def test_str_representation(self) -> None:
        """test string representation."""
        board_pos = GridPosition(0, 0)
        assert "board" in str(board_pos).lower()

        bench_pos = GridPosition(4, 0)
        assert "bench" in str(bench_pos).lower()

    def test_hashable(self) -> None:
        """test GridPosition is hashable (can be used in dicts/sets)."""
        pos1 = GridPosition(1, 2)
        pos2 = GridPosition(1, 2)
        pos3 = GridPosition(2, 1)

        # same position should have same hash
        assert hash(pos1) == hash(pos2)

        # can use in dict
        grid_dict = {pos1: "value"}
        assert grid_dict[pos2] == "value"

        # can use in set
        grid_set = {pos1, pos2, pos3}
        assert len(grid_set) == 2  # pos1 and pos2 are same


class TestGridConfig:
    """tests for GridConfig class."""

    @pytest.fixture
    def simple_grid(self) -> GridConfig:
        """create a simple grid for testing."""
        # 2x2 board + 2 bench slots
        board_positions = {
            (0, 0): (100.0, 100.0),
            (0, 1): (200.0, 100.0),
            (1, 0): (100.0, 200.0),
            (1, 1): (200.0, 200.0),
        }
        bench_positions = {
            0: (100.0, 400.0),
            1: (200.0, 400.0),
        }
        return GridConfig(
            resolution=(800, 600),
            board_positions=board_positions,
            bench_positions=bench_positions,
        )

    def test_get_pixel_position_board(self, simple_grid: GridConfig) -> None:
        """test getting pixel position for board positions."""
        pos = GridPosition(0, 0)
        assert simple_grid.get_pixel_position(pos) == (100.0, 100.0)

        pos = GridPosition(1, 1)
        assert simple_grid.get_pixel_position(pos) == (200.0, 200.0)

    def test_get_pixel_position_bench(self, simple_grid: GridConfig) -> None:
        """test getting pixel position for bench positions."""
        pos = GridPosition(4, 0)
        assert simple_grid.get_pixel_position(pos) == (100.0, 400.0)

        pos = GridPosition(4, 1)
        assert simple_grid.get_pixel_position(pos) == (200.0, 400.0)

    def test_get_pixel_position_not_configured(
        self, simple_grid: GridConfig
    ) -> None:
        """test getting position not in configuration raises KeyError."""
        # board position not in simple grid
        pos = GridPosition(2, 2)
        with pytest.raises(KeyError):
            simple_grid.get_pixel_position(pos)

        # bench position not in simple grid
        pos = GridPosition(4, 5)
        with pytest.raises(KeyError):
            simple_grid.get_pixel_position(pos)

    def test_snap_to_grid_exact(self, simple_grid: GridConfig) -> None:
        """test snapping to exact grid position."""
        result = simple_grid.snap_to_grid(100.0, 100.0)
        assert result == GridPosition(0, 0)

        result = simple_grid.snap_to_grid(200.0, 200.0)
        assert result == GridPosition(1, 1)

    def test_snap_to_grid_nearest(self, simple_grid: GridConfig) -> None:
        """test snapping to nearest grid position."""
        # slightly offset from (100, 100)
        result = simple_grid.snap_to_grid(105.0, 95.0)
        assert result == GridPosition(0, 0)

        # between (100, 100) and (200, 100), closer to (100, 100)
        result = simple_grid.snap_to_grid(130.0, 100.0)
        assert result == GridPosition(0, 0)

        # closer to (200, 100)
        result = simple_grid.snap_to_grid(180.0, 100.0)
        assert result == GridPosition(0, 1)

    def test_snap_to_grid_bench(self, simple_grid: GridConfig) -> None:
        """test snapping to bench position."""
        result = simple_grid.snap_to_grid(100.0, 400.0)
        assert result == GridPosition(4, 0)
        assert result.is_bench()

    def test_snap_to_grid_board_only(self, simple_grid: GridConfig) -> None:
        """test snapping with board_only flag."""
        # position closer to bench than any board position
        # but with board_only=True, should snap to nearest board
        result = simple_grid.snap_to_grid(
            100.0, 350.0, board_only=True
        )
        assert result.is_board()
        # should be (1, 0) at (100, 200) - closest board position
        assert result == GridPosition(1, 0)

    def test_batch_snap_to_grid(self, simple_grid: GridConfig) -> None:
        """test batch snapping multiple coordinates."""
        coords = np.array(
            [
                [100.0, 100.0],
                [200.0, 200.0],
                [100.0, 400.0],
            ]
        )
        results = simple_grid.batch_snap_to_grid(coords)

        assert len(results) == 3
        assert results[0] == GridPosition(0, 0)
        assert results[1] == GridPosition(1, 1)
        assert results[2] == GridPosition(4, 0)


class TestGridCreation:
    """tests for grid creation functions."""

    def test_create_default_grid_1080p(self) -> None:
        """test creating default 1080p grid."""
        grid = create_default_grid_1080p()

        assert grid.resolution == (1920, 1080)
        # should have 28 board positions
        assert len(grid.board_positions) == 28
        # should have 9 bench positions
        assert len(grid.bench_positions) == 9

        # verify all board positions are valid
        for row in range(4):
            for col in range(7):
                pos = GridPosition(row, col)
                pixel_pos = grid.get_pixel_position(pos)
                assert isinstance(pixel_pos, tuple)
                assert len(pixel_pos) == 2

        # verify all bench positions are valid
        for col in range(9):
            pos = GridPosition(4, col)
            pixel_pos = grid.get_pixel_position(pos)
            assert isinstance(pixel_pos, tuple)
            assert len(pixel_pos) == 2

    def test_create_grid_from_calibration_success(self) -> None:
        """test creating grid from manual calibration."""
        # create full board (4x7 = 28 positions)
        board_coords = {}
        for row in range(4):
            for col in range(7):
                board_coords[(row, col)] = (float(col * 100), float(row * 100))

        # create full bench (9 positions)
        bench_coords = {col: (float(col * 100), 500.0) for col in range(9)}

        grid = create_grid_from_calibration(
            resolution=(1920, 1080),
            board_coords=board_coords,
            bench_coords=bench_coords,
        )

        assert grid.resolution == (1920, 1080)
        assert len(grid.board_positions) == 28
        assert len(grid.bench_positions) == 9

    def test_create_grid_from_calibration_incomplete_board(self) -> None:
        """test creating grid with incomplete board raises ValueError."""
        # only 27 board positions
        board_coords = {}
        for row in range(4):
            for col in range(7):
                if not (row == 3 and col == 6):  # skip one position
                    board_coords[(row, col)] = (
                        float(col * 100),
                        float(row * 100),
                    )

        bench_coords = {col: (float(col * 100), 500.0) for col in range(9)}

        with pytest.raises(ValueError, match="board must have 28 positions"):
            create_grid_from_calibration(
                resolution=(1920, 1080),
                board_coords=board_coords,
                bench_coords=bench_coords,
            )

    def test_create_grid_from_calibration_incomplete_bench(self) -> None:
        """test creating grid with incomplete bench raises ValueError."""
        # complete board
        board_coords = {}
        for row in range(4):
            for col in range(7):
                board_coords[(row, col)] = (float(col * 100), float(row * 100))

        # only 8 bench positions
        bench_coords = {col: (float(col * 100), 500.0) for col in range(8)}

        with pytest.raises(ValueError, match="bench must have 9 positions"):
            create_grid_from_calibration(
                resolution=(1920, 1080),
                board_coords=board_coords,
                bench_coords=bench_coords,
            )

    def test_create_grid_from_calibration_invalid_position(self) -> None:
        """test creating grid with invalid positions raises ValueError."""
        # board with invalid position
        board_coords = {}
        for row in range(4):
            for col in range(7):
                board_coords[(row, col)] = (float(col * 100), float(row * 100))
        # add extra invalid position
        board_coords[(5, 0)] = (0.0, 0.0)

        bench_coords = {col: (float(col * 100), 500.0) for col in range(9)}

        # should fail during validation
        with pytest.raises(ValueError):
            create_grid_from_calibration(
                resolution=(1920, 1080),
                board_coords=board_coords,
                bench_coords=bench_coords,
            )
