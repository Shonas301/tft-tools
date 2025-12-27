"""integration tests for the champion detection pipeline."""

import pytest

from tft_vod_analysis.data.grid_config import (
    GridConfig,
    GridPosition,
    create_default_grid_1080p,
)
from tft_vod_analysis.detection.grid_snapper import (
    Detection,
    GridSnapper,
    create_mock_detections,
)
from tft_vod_analysis.output.tsl_formatter import (
    GameState,
    TSLFormatter,
    detections_to_game_state,
    format_detections_to_tsl,
)


@pytest.fixture
def grid_config() -> GridConfig:
    """create default grid config for testing."""
    return create_default_grid_1080p()


@pytest.fixture
def grid_snapper(grid_config: GridConfig) -> GridSnapper:
    """create grid snapper for testing."""
    return GridSnapper(grid_config)


@pytest.fixture
def tsl_formatter() -> TSLFormatter:
    """create TSL formatter for testing."""
    return TSLFormatter()


class TestDetectionToTSLPipeline:
    """end-to-end tests for detection → TSL pipeline."""

    def test_empty_board(
        self, grid_snapper: GridSnapper, tsl_formatter: TSLFormatter
    ) -> None:
        """test formatting empty board."""
        detections = []
        board_state = grid_snapper.get_board_state(detections)
        state = detections_to_game_state(board_state)

        result = tsl_formatter.format_full(state)

        # should have empty board FEN
        assert "[7/7/7/7/9]" in result
        assert "1 1-1 0g 100h" in result

    def test_single_champion_board(
        self, grid_config: GridConfig, grid_snapper: GridSnapper
    ) -> None:
        """test single champion on board."""
        # create mock detection at position (0, 0)
        mock_positions = [(GridPosition(0, 0), "ANI", 2)]
        detections = create_mock_detections(mock_positions, grid_config)

        board_state = grid_snapper.get_board_state(detections)
        result = format_detections_to_tsl(board_state)

        # should have 2-star Anivia at position 0,0
        assert "2ANI" in result
        assert "[2ANI|6/" in result  # champion at col 0, then 6 empty

    def test_multiple_champions_board(
        self, grid_config: GridConfig, grid_snapper: GridSnapper
    ) -> None:
        """test multiple champions on board."""
        mock_positions = [
            (GridPosition(0, 0), "ANI", 2),
            (GridPosition(0, 3), "BLI", 1),
            (GridPosition(2, 5), "ASH", 3),
        ]
        detections = create_mock_detections(mock_positions, grid_config)

        board_state = grid_snapper.get_board_state(detections)
        result = format_detections_to_tsl(board_state)

        # verify all champions appear
        assert "2ANI" in result
        assert "1BLI" in result
        assert "3ASH" in result

    def test_champion_on_bench(
        self, grid_config: GridConfig, grid_snapper: GridSnapper
    ) -> None:
        """test champion on bench."""
        mock_positions = [(GridPosition(4, 2), "LUL", 1)]  # bench position
        detections = create_mock_detections(mock_positions, grid_config)

        board_state = grid_snapper.get_board_state(detections)
        result = format_detections_to_tsl(board_state)

        # verify bench has champion
        assert "1LUL" in result
        # board should be empty, bench has champion at position 2
        assert "7/7/7/7/2|1LUL|6" in result

    def test_mixed_board_and_bench(
        self, grid_config: GridConfig, grid_snapper: GridSnapper
    ) -> None:
        """test champions on both board and bench."""
        mock_positions = [
            (GridPosition(0, 0), "ANI", 2),
            (GridPosition(1, 3), "BLI", 1),
            (GridPosition(4, 0), "ASH", 1),  # bench
            (GridPosition(4, 8), "LUL", 3),  # bench
        ]
        detections = create_mock_detections(mock_positions, grid_config)

        board_state = grid_snapper.get_board_state(detections)
        state = detections_to_game_state(board_state)

        formatter = TSLFormatter()
        result = formatter.format_full(state)

        # verify all champions
        assert "2ANI" in result
        assert "1BLI" in result
        assert "1ASH" in result
        assert "3LUL" in result

    def test_with_game_metadata(
        self, grid_config: GridConfig, grid_snapper: GridSnapper
    ) -> None:
        """test TSL output with game metadata."""
        mock_positions = [(GridPosition(0, 0), "ANI", 2)]
        detections = create_mock_detections(mock_positions, grid_config)

        board_state = grid_snapper.get_board_state(detections)
        result = format_detections_to_tsl(
            board_state,
            level=7,
            stage=3,
            round_num=5,
            gold=32,
            health=67,
        )

        assert result.startswith("7 3-5 32g 67h")

    def test_board_only_output(
        self, grid_config: GridConfig, grid_snapper: GridSnapper
    ) -> None:
        """test board-only TSL output (no metadata)."""
        mock_positions = [(GridPosition(0, 0), "ANI", 2)]
        detections = create_mock_detections(mock_positions, grid_config)

        board_state = grid_snapper.get_board_state(detections)
        result = format_detections_to_tsl(
            board_state, include_metadata=False
        )

        # should only have board FEN
        assert result.startswith("[")
        assert result.endswith("]")
        assert "g" not in result  # no gold
        assert "h" not in result  # no health


class TestGridSnapping:
    """tests for grid snapping behavior."""

    def test_conflict_resolution_highest_confidence(
        self, grid_config: GridConfig
    ) -> None:
        """test conflict resolution keeps highest confidence detection."""
        snapper = GridSnapper(
            grid_config, conflict_resolution="highest_confidence"
        )

        # two detections at same position
        px, py = grid_config.get_pixel_position(GridPosition(0, 0))

        detections = [
            Detection(
                bbox=(int(px - 30), int(py - 30), 60, 60),
                champion_shorthand="ANI",
                confidence=0.9,
                star_level=2,
            ),
            Detection(
                bbox=(int(px - 25), int(py - 25), 60, 60),
                champion_shorthand="BLI",
                confidence=0.95,  # higher confidence
                star_level=1,
            ),
        ]

        result = snapper.snap_multiple(detections)

        # should keep BLI (higher confidence)
        assert len(result.snapped) == 1
        assert result.snapped[0].detection.champion_shorthand == "BLI"
        assert len(result.conflicts) == 1

    def test_detection_too_far_unassigned(
        self, grid_config: GridConfig
    ) -> None:
        """test that detections far from grid are unassigned."""
        snapper = GridSnapper(grid_config, max_distance=30)

        # detection far from any grid position
        detection = Detection(
            bbox=(0, 0, 50, 50),  # top-left corner, far from grid
            champion_shorthand="ANI",
            confidence=0.9,
            star_level=1,
        )

        result = snapper.snap_multiple([detection])

        assert len(result.snapped) == 0
        assert len(result.unassigned) == 1


class TestTSLFormatter:
    """tests for TSL formatting."""

    def test_format_champion(self, tsl_formatter: TSLFormatter) -> None:
        """test single champion formatting."""
        assert tsl_formatter.format_champion("ANI", 1) == "1ANI"
        assert tsl_formatter.format_champion("ANI", 2) == "2ANI"
        assert tsl_formatter.format_champion("ANI", 3) == "3ANI"
        assert tsl_formatter.format_champion("BLI", 1) == "1BLI"

    def test_format_row_empty(self, tsl_formatter: TSLFormatter) -> None:
        """test formatting empty row."""
        result = tsl_formatter.format_row({}, 7)
        assert result == "7"

    def test_format_row_single_champion(
        self, tsl_formatter: TSLFormatter
    ) -> None:
        """test formatting row with single champion."""
        champions = {0: ("ANI", 2)}
        result = tsl_formatter.format_row(champions, 7)
        assert result == "2ANI|6"

    def test_format_row_multiple_champions(
        self, tsl_formatter: TSLFormatter
    ) -> None:
        """test formatting row with multiple champions."""
        champions = {0: ("ANI", 1), 3: ("BLI", 2), 6: ("ASH", 3)}
        result = tsl_formatter.format_row(champions, 7)
        assert result == "1ANI|2|2BLI|2|3ASH"

    def test_format_row_consecutive_champions(
        self, tsl_formatter: TSLFormatter
    ) -> None:
        """test formatting row with consecutive champions."""
        champions = {0: ("ANI", 1), 1: ("BLI", 2)}
        result = tsl_formatter.format_row(champions, 7)
        assert result == "1ANI|1BLI|5"

    def test_format_full_state(self, tsl_formatter: TSLFormatter) -> None:
        """test formatting complete game state."""
        state = GameState(
            level=7,
            stage=3,
            round=5,
            gold=32,
            health=67,
            board={GridPosition(0, 0): ("ANI", 2)},
            bench={2: ("BLI", 1)},
            items=[],
            augments=[],
        )

        result = tsl_formatter.format_full(state)

        assert result == "7 3-5 32g 67h [2ANI|6/7/7/7/2|1BLI|6] [] []"
