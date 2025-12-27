"""unit tests for visualization utilities."""

import numpy as np
import pytest

from tft_vod_analysis.data.grid_config import GridConfig, GridPosition
from tft_vod_analysis.utils.visualization import (
    DetectionVisualizer,
    save_visualization,
)


@pytest.fixture
def sample_frame() -> np.ndarray:
    """create a sample frame for testing."""
    return np.zeros((1080, 1920, 3), dtype=np.uint8)


@pytest.fixture
def simple_grid_config() -> GridConfig:
    """create a simple grid configuration for testing."""
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
        resolution=(1920, 1080),
        board_positions=board_positions,
        bench_positions=bench_positions,
    )


@pytest.fixture
def visualizer() -> DetectionVisualizer:
    """create a basic detection visualizer."""
    return DetectionVisualizer()


@pytest.fixture
def visualizer_with_grid(
    simple_grid_config: GridConfig,
) -> DetectionVisualizer:
    """create a detection visualizer with grid config."""
    return DetectionVisualizer(grid_config=simple_grid_config)


class TestDetectionVisualizer:
    """tests for DetectionVisualizer class."""

    def test_initialization_default(self) -> None:
        """test initializing visualizer with defaults."""
        viz = DetectionVisualizer()
        assert viz.grid_config is None
        assert viz.font_scale == 0.5
        assert viz.thickness == 2

    def test_initialization_with_config(
        self, simple_grid_config: GridConfig
    ) -> None:
        """test initializing visualizer with grid config."""
        viz = DetectionVisualizer(
            grid_config=simple_grid_config,
            font_scale=0.7,
            thickness=3,
        )
        assert viz.grid_config == simple_grid_config
        assert viz.font_scale == 0.7
        assert viz.thickness == 3

    def test_draw_bbox_basic(
        self, visualizer: DetectionVisualizer, sample_frame: np.ndarray
    ) -> None:
        """test drawing a basic bounding box."""
        result = visualizer.draw_bbox(sample_frame, (100, 100, 200, 150))

        # verify frame is not modified in-place
        assert not np.array_equal(result, sample_frame)
        # verify result has same shape
        assert result.shape == sample_frame.shape
        # verify some pixels were modified (bbox was drawn)
        assert not np.array_equal(result, sample_frame)

    def test_draw_bbox_with_label(
        self, visualizer: DetectionVisualizer, sample_frame: np.ndarray
    ) -> None:
        """test drawing bounding box with label."""
        result = visualizer.draw_bbox(
            sample_frame,
            (100, 100, 200, 150),
            label="Anivia",
        )

        assert result.shape == sample_frame.shape
        # verify pixels were modified
        assert not np.array_equal(result, sample_frame)

    def test_draw_bbox_with_confidence(
        self, visualizer: DetectionVisualizer, sample_frame: np.ndarray
    ) -> None:
        """test drawing bounding box with confidence score."""
        result = visualizer.draw_bbox(
            sample_frame,
            (100, 100, 200, 150),
            label="Anivia",
            confidence=0.95,
        )

        assert result.shape == sample_frame.shape
        assert not np.array_equal(result, sample_frame)

    def test_draw_bbox_custom_color(
        self, visualizer: DetectionVisualizer, sample_frame: np.ndarray
    ) -> None:
        """test drawing bounding box with custom color."""
        custom_color = (255, 0, 0)  # blue
        result = visualizer.draw_bbox(
            sample_frame,
            (100, 100, 200, 150),
            color=custom_color,
        )

        assert result.shape == sample_frame.shape
        assert not np.array_equal(result, sample_frame)

    def test_draw_multiple_bboxes(
        self, visualizer: DetectionVisualizer, sample_frame: np.ndarray
    ) -> None:
        """test drawing multiple bounding boxes."""
        detections = [
            ((100, 100, 200, 150), "Anivia", 0.95),
            ((400, 200, 150, 120), "Blitzcrank", 0.87),
            ((700, 300, 180, 140), "Ashe", 0.92),
        ]

        result = visualizer.draw_multiple_bboxes(sample_frame, detections)

        assert result.shape == sample_frame.shape
        assert not np.array_equal(result, sample_frame)

    def test_draw_grid_positions_without_config(
        self, visualizer: DetectionVisualizer, sample_frame: np.ndarray
    ) -> None:
        """test drawing grid positions without grid_config raises error."""
        with pytest.raises(ValueError, match="grid_config must be set"):
            visualizer.draw_grid_positions(sample_frame)

    def test_draw_grid_positions_with_config(
        self,
        visualizer_with_grid: DetectionVisualizer,
        sample_frame: np.ndarray,
    ) -> None:
        """test drawing grid positions with grid_config."""
        result = visualizer_with_grid.draw_grid_positions(sample_frame)

        assert result.shape == sample_frame.shape
        assert not np.array_equal(result, sample_frame)

    def test_draw_grid_positions_with_labels(
        self,
        visualizer_with_grid: DetectionVisualizer,
        sample_frame: np.ndarray,
    ) -> None:
        """test drawing grid positions with labels."""
        result = visualizer_with_grid.draw_grid_positions(
            sample_frame, show_labels=True
        )

        assert result.shape == sample_frame.shape
        assert not np.array_equal(result, sample_frame)

    def test_draw_grid_positions_without_labels(
        self,
        visualizer_with_grid: DetectionVisualizer,
        sample_frame: np.ndarray,
    ) -> None:
        """test drawing grid positions without labels."""
        result = visualizer_with_grid.draw_grid_positions(
            sample_frame, show_labels=False
        )

        assert result.shape == sample_frame.shape
        assert not np.array_equal(result, sample_frame)

    def test_draw_champion_at_position(
        self,
        visualizer_with_grid: DetectionVisualizer,
        sample_frame: np.ndarray,
    ) -> None:
        """test drawing a champion at a grid position."""
        grid_pos = GridPosition(0, 0)
        result = visualizer_with_grid.draw_champion_at_position(
            sample_frame,
            grid_pos,
            "ANI",
            2,
        )

        assert result.shape == sample_frame.shape
        assert not np.array_equal(result, sample_frame)

    def test_draw_champion_at_position_without_config(
        self, visualizer: DetectionVisualizer, sample_frame: np.ndarray
    ) -> None:
        """test drawing champion without grid_config raises error."""
        grid_pos = GridPosition(0, 0)
        with pytest.raises(ValueError, match="grid_config must be set"):
            visualizer.draw_champion_at_position(
                sample_frame,
                grid_pos,
                "ANI",
                2,
            )

    def test_draw_champion_bench_position(
        self,
        visualizer_with_grid: DetectionVisualizer,
        sample_frame: np.ndarray,
    ) -> None:
        """test drawing a champion at a bench position."""
        bench_pos = GridPosition(4, 0)
        result = visualizer_with_grid.draw_champion_at_position(
            sample_frame,
            bench_pos,
            "BLI",
            1,
        )

        assert result.shape == sample_frame.shape
        assert not np.array_equal(result, sample_frame)

    def test_draw_roi_overlay_basic(
        self, visualizer: DetectionVisualizer, sample_frame: np.ndarray
    ) -> None:
        """test drawing basic ROI overlay."""
        result = visualizer.draw_roi_overlay(
            sample_frame,
            (100, 100, 400, 300),
        )

        assert result.shape == sample_frame.shape
        assert not np.array_equal(result, sample_frame)

    def test_draw_roi_overlay_with_label(
        self, visualizer: DetectionVisualizer, sample_frame: np.ndarray
    ) -> None:
        """test drawing ROI overlay with label."""
        result = visualizer.draw_roi_overlay(
            sample_frame,
            (100, 100, 400, 300),
            label="Board ROI",
        )

        assert result.shape == sample_frame.shape
        assert not np.array_equal(result, sample_frame)

    def test_create_detection_summary_basic(
        self, visualizer: DetectionVisualizer, sample_frame: np.ndarray
    ) -> None:
        """test creating detection summary without grid."""
        detections = [
            ((100, 100, 200, 150), "Anivia", 0.95),
            ((400, 200, 150, 120), "Blitzcrank", 0.87),
        ]

        result = visualizer.create_detection_summary(
            sample_frame, detections
        )

        assert result.shape == sample_frame.shape
        assert not np.array_equal(result, sample_frame)

    def test_create_detection_summary_with_grid(
        self,
        visualizer_with_grid: DetectionVisualizer,
        sample_frame: np.ndarray,
    ) -> None:
        """test creating detection summary with grid positions."""
        detections = [
            ((100, 100, 200, 150), "Anivia", 0.95),
        ]
        grid_positions = [
            (GridPosition(0, 0), "ANI", 2),
            (GridPosition(4, 0), "BLI", 1),
        ]

        result = visualizer_with_grid.create_detection_summary(
            sample_frame, detections, grid_positions
        )

        assert result.shape == sample_frame.shape
        assert not np.array_equal(result, sample_frame)


class TestVisualizationHelpers:
    """tests for standalone visualization helper functions."""

    def test_save_visualization(
        self, sample_frame: np.ndarray, tmp_path: pytest.TempPathFactory
    ) -> None:
        """test saving visualization to file."""
        import cv2
        from pathlib import Path

        output_path = tmp_path / "test_output.png"
        save_visualization(sample_frame, str(output_path))

        # verify file was created
        assert output_path.exists()

        # verify file can be loaded
        loaded = cv2.imread(str(output_path))
        assert loaded is not None
        assert loaded.shape == sample_frame.shape

    def test_show_visualization_no_display(
        self, sample_frame: np.ndarray
    ) -> None:
        """test show_visualization (skip if no display available)."""
        # this test will fail in headless environments
        # so we just verify the function exists and is callable
        from tft_vod_analysis.utils.visualization import (
            show_visualization,
        )

        assert callable(show_visualization)
        # we don't actually call it as it requires a display
