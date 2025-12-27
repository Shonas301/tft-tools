"""unit tests for frame preprocessor."""

from pathlib import Path
from tempfile import NamedTemporaryFile

import cv2
import numpy as np
import pytest

from tft_vod_analysis.frame_processing.preprocessor import (
    ExtractedROIs,
    FramePreprocessor,
    ROIConfig,
    create_default_roi_config_1080p,
    create_roi_config_from_calibration,
)


@pytest.fixture
def sample_frame_1080p() -> np.ndarray:
    """create a sample 1920x1080 frame for testing."""
    # create a simple test frame with different colored regions
    frame = np.zeros((1080, 1920, 3), dtype=np.uint8)
    # add some colored regions for visual distinction
    frame[:, :, 0] = 50  # blue channel
    frame[:, :, 1] = 100  # green channel
    frame[:, :, 2] = 150  # red channel
    return frame


@pytest.fixture
def sample_frame_file_1080p(sample_frame_1080p: np.ndarray) -> Path:
    """create a temporary file with a sample frame."""
    with NamedTemporaryFile(suffix=".png", delete=False) as f:
        temp_path = Path(f.name)
        cv2.imwrite(str(temp_path), sample_frame_1080p)
    return temp_path


@pytest.fixture
def simple_roi_config() -> ROIConfig:
    """create a simple ROI configuration for testing."""
    return ROIConfig(
        resolution=(1920, 1080),
        board_roi=(100, 100, 400, 300),
        bench_roi=(100, 500, 400, 100),
        gold_roi=(50, 50, 100, 30),
        health_roi=(50, 100, 100, 30),
        level_roi=(1770, 50, 100, 30),
        stage_roi=(860, 50, 200, 30),
    )


@pytest.fixture
def preprocessor(simple_roi_config: ROIConfig) -> FramePreprocessor:
    """create a FramePreprocessor instance with simple config."""
    return FramePreprocessor(simple_roi_config)


class TestROIConfig:
    """tests for ROIConfig dataclass."""

    def test_roi_config_creation(self) -> None:
        """test creating ROI configuration."""
        config = ROIConfig(
            resolution=(1920, 1080),
            board_roi=(0, 0, 100, 100),
            bench_roi=(0, 100, 100, 50),
            gold_roi=(0, 0, 50, 20),
            health_roi=(0, 20, 50, 20),
            level_roi=(1870, 0, 50, 20),
            stage_roi=(900, 0, 120, 20),
        )
        assert config.resolution == (1920, 1080)
        assert config.board_roi == (0, 0, 100, 100)


class TestFramePreprocessor:
    """tests for FramePreprocessor class."""

    def test_load_frame_success(
        self,
        preprocessor: FramePreprocessor,
        sample_frame_file_1080p: Path,
    ) -> None:
        """test successfully loading a frame."""
        frame = preprocessor.load_frame(sample_frame_file_1080p)
        assert frame.shape == (1080, 1920, 3)
        assert frame.dtype == np.uint8

        # cleanup
        sample_frame_file_1080p.unlink()

    def test_load_frame_file_not_found(
        self, preprocessor: FramePreprocessor
    ) -> None:
        """test loading non-existent frame raises FileNotFoundError."""
        with pytest.raises(FileNotFoundError):
            preprocessor.load_frame("/nonexistent/frame.png")

    def test_load_frame_invalid_file(
        self, preprocessor: FramePreprocessor
    ) -> None:
        """test loading invalid image file raises ValueError."""
        # create a text file, not an image
        with NamedTemporaryFile(suffix=".png", delete=False, mode="w") as f:
            f.write("not an image")
            temp_path = Path(f.name)

        with pytest.raises(ValueError, match="failed to load frame"):
            preprocessor.load_frame(temp_path)

        # cleanup
        temp_path.unlink()

    def test_load_frame_wrong_resolution(
        self, preprocessor: FramePreprocessor
    ) -> None:
        """test loading frame with wrong resolution raises ValueError."""
        # create a 640x480 frame instead of 1920x1080
        small_frame = np.zeros((480, 640, 3), dtype=np.uint8)

        with NamedTemporaryFile(suffix=".png", delete=False) as f:
            temp_path = Path(f.name)
            cv2.imwrite(str(temp_path), small_frame)

        with pytest.raises(ValueError, match="resolution mismatch"):
            preprocessor.load_frame(temp_path)

        # cleanup
        temp_path.unlink()

    def test_extract_roi_success(
        self, preprocessor: FramePreprocessor, sample_frame_1080p: np.ndarray
    ) -> None:
        """test extracting valid ROI."""
        roi = preprocessor.extract_roi(sample_frame_1080p, (100, 100, 200, 150))
        assert roi.shape == (150, 200, 3)
        assert roi.dtype == np.uint8

    def test_extract_roi_out_of_bounds(
        self, preprocessor: FramePreprocessor, sample_frame_1080p: np.ndarray
    ) -> None:
        """test extracting ROI out of bounds raises ValueError."""
        # ROI extends beyond frame
        with pytest.raises(ValueError, match="out of bounds"):
            preprocessor.extract_roi(sample_frame_1080p, (1800, 1000, 200, 150))

        # negative coordinates
        with pytest.raises(ValueError, match="out of bounds"):
            preprocessor.extract_roi(sample_frame_1080p, (-10, 100, 200, 150))

    def test_extract_all_rois(
        self,
        preprocessor: FramePreprocessor,
        sample_frame_1080p: np.ndarray,
        simple_roi_config: ROIConfig,
    ) -> None:
        """test extracting all configured ROIs."""
        rois = preprocessor.extract_all_rois(sample_frame_1080p)

        # verify all ROIs were extracted
        assert isinstance(rois, ExtractedROIs)
        assert rois.board.shape == (300, 400, 3)  # from config
        assert rois.bench.shape == (100, 400, 3)
        assert rois.gold.shape == (30, 100, 3)
        assert rois.health.shape == (30, 100, 3)
        assert rois.level.shape == (30, 100, 3)
        assert rois.stage.shape == (30, 200, 3)

        # verify original frame is preserved
        assert rois.original_frame.shape == sample_frame_1080p.shape
        np.testing.assert_array_equal(
            rois.original_frame, sample_frame_1080p
        )

    def test_normalize_for_detection(
        self, preprocessor: FramePreprocessor
    ) -> None:
        """test normalizing ROI for detection."""
        # create a test ROI with known values
        roi = np.ones((100, 100, 3), dtype=np.uint8) * 128  # mid-gray

        normalized = preprocessor.normalize_for_detection(roi)

        # should be in range [0, 1]
        assert normalized.dtype == np.float32
        assert normalized.min() >= 0.0
        assert normalized.max() <= 1.0
        # 128 / 255 ≈ 0.502
        assert np.allclose(normalized, 128.0 / 255.0, atol=0.001)

    def test_normalize_for_detection_with_resize(
        self, preprocessor: FramePreprocessor
    ) -> None:
        """test normalizing with resizing."""
        roi = np.ones((100, 100, 3), dtype=np.uint8) * 255

        normalized = preprocessor.normalize_for_detection(
            roi, target_size=(50, 50)
        )

        # should be resized
        assert normalized.shape == (50, 50, 3)
        # should be normalized to [0, 1]
        assert np.allclose(normalized, 1.0, atol=0.001)

    def test_preprocess_frame(
        self,
        preprocessor: FramePreprocessor,
        sample_frame_file_1080p: Path,
    ) -> None:
        """test end-to-end frame preprocessing."""
        rois = preprocessor.preprocess_frame(sample_frame_file_1080p)

        # verify all ROIs extracted
        assert isinstance(rois, ExtractedROIs)
        assert rois.board.shape == (300, 400, 3)
        assert rois.bench.shape == (100, 400, 3)
        assert rois.original_frame.shape == (1080, 1920, 3)

        # cleanup
        sample_frame_file_1080p.unlink()


class TestROIConfigCreation:
    """tests for ROI configuration creation functions."""

    def test_create_default_roi_config_1080p(self) -> None:
        """test creating default 1080p ROI configuration."""
        config = create_default_roi_config_1080p()

        assert config.resolution == (1920, 1080)
        # verify all ROIs are defined
        assert len(config.board_roi) == 4
        assert len(config.bench_roi) == 4
        assert len(config.gold_roi) == 4
        assert len(config.health_roi) == 4
        assert len(config.level_roi) == 4
        assert len(config.stage_roi) == 4

        # verify ROIs have positive dimensions
        for roi in [
            config.board_roi,
            config.bench_roi,
            config.gold_roi,
            config.health_roi,
            config.level_roi,
            config.stage_roi,
        ]:
            x, y, w, h = roi
            assert w > 0, f"ROI width must be positive, got {w}"
            assert h > 0, f"ROI height must be positive, got {h}"

    def test_create_roi_config_from_calibration_success(self) -> None:
        """test creating ROI config from manual calibration."""
        config = create_roi_config_from_calibration(
            resolution=(1920, 1080),
            board_roi=(500, 200, 920, 600),
            bench_roi=(500, 850, 920, 150),
            gold_roi=(50, 950, 100, 40),
            health_roi=(50, 1000, 100, 40),
            level_roi=(1770, 950, 100, 40),
            stage_roi=(860, 50, 200, 40),
        )

        assert config.resolution == (1920, 1080)
        assert config.board_roi == (500, 200, 920, 600)
        assert config.bench_roi == (500, 850, 920, 150)

    def test_create_roi_config_invalid_dimensions(self) -> None:
        """test creating ROI config with invalid dimensions raises ValueError."""
        # negative width
        with pytest.raises(ValueError, match="invalid dimensions"):
            create_roi_config_from_calibration(
                resolution=(1920, 1080),
                board_roi=(500, 200, -920, 600),  # negative width
                bench_roi=(500, 850, 920, 150),
                gold_roi=(50, 950, 100, 40),
                health_roi=(50, 1000, 100, 40),
                level_roi=(1770, 950, 100, 40),
                stage_roi=(860, 50, 200, 40),
            )

        # zero height
        with pytest.raises(ValueError, match="invalid dimensions"):
            create_roi_config_from_calibration(
                resolution=(1920, 1080),
                board_roi=(500, 200, 920, 600),
                bench_roi=(500, 850, 920, 0),  # zero height
                gold_roi=(50, 950, 100, 40),
                health_roi=(50, 1000, 100, 40),
                level_roi=(1770, 950, 100, 40),
                stage_roi=(860, 50, 200, 40),
            )

    def test_create_roi_config_negative_coordinates(self) -> None:
        """test creating ROI config with negative coordinates raises ValueError."""
        with pytest.raises(ValueError, match="negative coordinates"):
            create_roi_config_from_calibration(
                resolution=(1920, 1080),
                board_roi=(500, 200, 920, 600),
                bench_roi=(500, 850, 920, 150),
                gold_roi=(-50, 950, 100, 40),  # negative x
                health_roi=(50, 1000, 100, 40),
                level_roi=(1770, 950, 100, 40),
                stage_roi=(860, 50, 200, 40),
            )


class TestExtractedROIs:
    """tests for ExtractedROIs dataclass."""

    def test_extracted_rois_creation(self) -> None:
        """test creating ExtractedROIs instance."""
        board = np.zeros((300, 400, 3), dtype=np.uint8)
        bench = np.zeros((100, 400, 3), dtype=np.uint8)
        gold = np.zeros((30, 100, 3), dtype=np.uint8)
        health = np.zeros((30, 100, 3), dtype=np.uint8)
        level = np.zeros((30, 100, 3), dtype=np.uint8)
        stage = np.zeros((30, 200, 3), dtype=np.uint8)
        original = np.zeros((1080, 1920, 3), dtype=np.uint8)

        rois = ExtractedROIs(
            board=board,
            bench=bench,
            gold=gold,
            health=health,
            level=level,
            stage=stage,
            original_frame=original,
        )

        assert rois.board.shape == (300, 400, 3)
        assert rois.bench.shape == (100, 400, 3)
        assert rois.original_frame.shape == (1080, 1920, 3)
