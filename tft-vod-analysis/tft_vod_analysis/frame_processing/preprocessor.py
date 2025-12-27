"""frame preprocessing and ROI extraction for TFT gameplay analysis."""

from dataclasses import dataclass
from pathlib import Path
from typing import Tuple

import cv2
import numpy as np
from numpy.typing import NDArray


@dataclass
class ROIConfig:
    """configuration for regions of interest in TFT gameplay frames.

    coordinates are specified as (x, y, width, height) in pixels.
    """

    resolution: Tuple[int, int]  # (width, height)
    board_roi: Tuple[int, int, int, int]  # (x, y, w, h)
    bench_roi: Tuple[int, int, int, int]  # (x, y, w, h)
    gold_roi: Tuple[int, int, int, int]  # (x, y, w, h)
    health_roi: Tuple[int, int, int, int]  # (x, y, w, h)
    level_roi: Tuple[int, int, int, int]  # (x, y, w, h)
    stage_roi: Tuple[int, int, int, int]  # (x, y, w, h)


@dataclass
class ExtractedROIs:
    """container for extracted regions of interest from a frame."""

    board: NDArray[np.uint8]
    bench: NDArray[np.uint8]
    gold: NDArray[np.uint8]
    health: NDArray[np.uint8]
    level: NDArray[np.uint8]
    stage: NDArray[np.uint8]
    original_frame: NDArray[np.uint8]


class FramePreprocessor:
    """preprocesses TFT gameplay frames for champion detection.

    handles frame loading, ROI extraction, and normalization.
    """

    def __init__(self, roi_config: ROIConfig) -> None:
        """initialize the frame preprocessor.

        Args:
            roi_config: configuration specifying ROI locations
        """
        self.roi_config = roi_config

    def load_frame(self, frame_path: Path | str) -> NDArray[np.uint8]:
        """load a frame from an image file.

        Args:
            frame_path: path to the image file

        Returns:
            loaded frame as numpy array (BGR format)

        Raises:
            FileNotFoundError: if frame file doesn't exist
            ValueError: if frame cannot be loaded or is invalid
        """
        frame_path = Path(frame_path)
        if not frame_path.exists():
            raise FileNotFoundError(f"frame not found: {frame_path}")

        frame = cv2.imread(str(frame_path))
        if frame is None:
            raise ValueError(f"failed to load frame: {frame_path}")

        # validate frame dimensions
        height, width = frame.shape[:2]
        expected_width, expected_height = self.roi_config.resolution
        if (width, height) != (expected_width, expected_height):
            raise ValueError(
                f"frame resolution mismatch: expected "
                f"{expected_width}x{expected_height}, got {width}x{height}"
            )

        return frame

    def extract_roi(
        self,
        frame: NDArray[np.uint8],
        roi: Tuple[int, int, int, int],
    ) -> NDArray[np.uint8]:
        """extract a region of interest from a frame.

        Args:
            frame: input frame
            roi: region as (x, y, width, height)

        Returns:
            extracted ROI as numpy array

        Raises:
            ValueError: if ROI is out of bounds
        """
        x, y, w, h = roi
        frame_height, frame_width = frame.shape[:2]

        # validate ROI bounds
        if x < 0 or y < 0 or x + w > frame_width or y + h > frame_height:
            raise ValueError(
                f"ROI ({x}, {y}, {w}, {h}) out of bounds for frame "
                f"{frame_width}x{frame_height}"
            )

        return frame[y : y + h, x : x + w].copy()

    def extract_all_rois(
        self, frame: NDArray[np.uint8]
    ) -> ExtractedROIs:
        """extract all configured ROIs from a frame.

        Args:
            frame: input frame

        Returns:
            ExtractedROIs containing all extracted regions

        Raises:
            ValueError: if any ROI is invalid
        """
        return ExtractedROIs(
            board=self.extract_roi(frame, self.roi_config.board_roi),
            bench=self.extract_roi(frame, self.roi_config.bench_roi),
            gold=self.extract_roi(frame, self.roi_config.gold_roi),
            health=self.extract_roi(frame, self.roi_config.health_roi),
            level=self.extract_roi(frame, self.roi_config.level_roi),
            stage=self.extract_roi(frame, self.roi_config.stage_roi),
            original_frame=frame.copy(),
        )

    def normalize_for_detection(
        self,
        roi: NDArray[np.uint8],
        target_size: Tuple[int, int] | None = None,
    ) -> NDArray[np.float32]:
        """normalize an ROI for model inference.

        Args:
            roi: region of interest to normalize
            target_size: optional (width, height) to resize to

        Returns:
            normalized array in range [0, 1]
        """
        # resize if target size specified
        if target_size is not None:
            roi = cv2.resize(roi, target_size, interpolation=cv2.INTER_LINEAR)

        # normalize to [0, 1]
        normalized = roi.astype(np.float32) / 255.0
        return normalized

    def preprocess_frame(
        self, frame_path: Path | str
    ) -> ExtractedROIs:
        """load a frame and extract all ROIs.

        convenience method combining load_frame and extract_all_rois.

        Args:
            frame_path: path to the frame file

        Returns:
            ExtractedROIs containing all regions

        Raises:
            FileNotFoundError: if frame file doesn't exist
            ValueError: if frame is invalid or ROIs are out of bounds
        """
        frame = self.load_frame(frame_path)
        return self.extract_all_rois(frame)


def create_default_roi_config_1080p() -> ROIConfig:
    """create default ROI configuration for 1920x1080 resolution.

    this is a placeholder implementation. actual ROI coordinates should be
    determined by analyzing reference TFT gameplay frames.

    Returns:
        ROIConfig for 1080p resolution
    """
    # placeholder coordinates - need calibration from actual frames
    # these are rough estimates based on typical TFT UI layout

    return ROIConfig(
        resolution=(1920, 1080),
        # board area (center of screen, hex grid)
        board_roi=(500, 200, 920, 600),
        # bench area (bottom center)
        bench_roi=(500, 850, 920, 150),
        # UI elements (need refinement)
        gold_roi=(50, 950, 100, 40),  # bottom left
        health_roi=(50, 1000, 100, 40),  # bottom left
        level_roi=(1770, 950, 100, 40),  # bottom right
        stage_roi=(860, 50, 200, 40),  # top center
    )


def create_roi_config_from_calibration(
    resolution: Tuple[int, int],
    board_roi: Tuple[int, int, int, int],
    bench_roi: Tuple[int, int, int, int],
    gold_roi: Tuple[int, int, int, int],
    health_roi: Tuple[int, int, int, int],
    level_roi: Tuple[int, int, int, int],
    stage_roi: Tuple[int, int, int, int],
) -> ROIConfig:
    """create ROI configuration from manually calibrated coordinates.

    Args:
        resolution: target resolution (width, height)
        board_roi: board region (x, y, width, height)
        bench_roi: bench region (x, y, width, height)
        gold_roi: gold UI region (x, y, width, height)
        health_roi: health UI region (x, y, width, height)
        level_roi: level UI region (x, y, width, height)
        stage_roi: stage/round UI region (x, y, width, height)

    Returns:
        configured ROIConfig

    Raises:
        ValueError: if any ROI has invalid dimensions
    """
    # validate all ROIs have positive dimensions
    for roi_name, roi in [
        ("board", board_roi),
        ("bench", bench_roi),
        ("gold", gold_roi),
        ("health", health_roi),
        ("level", level_roi),
        ("stage", stage_roi),
    ]:
        x, y, w, h = roi
        if w <= 0 or h <= 0:
            raise ValueError(
                f"{roi_name} ROI has invalid dimensions: {w}x{h}"
            )
        if x < 0 or y < 0:
            raise ValueError(
                f"{roi_name} ROI has negative coordinates: ({x}, {y})"
            )

    return ROIConfig(
        resolution=resolution,
        board_roi=board_roi,
        bench_roi=bench_roi,
        gold_roi=gold_roi,
        health_roi=health_roi,
        level_roi=level_roi,
        stage_roi=stage_roi,
    )
