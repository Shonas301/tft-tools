"""visualization utilities for TFT champion detection debugging."""

from typing import List, Tuple

import cv2
import numpy as np
from numpy.typing import NDArray

from tft_vod_analysis.data.grid_config import GridConfig, GridPosition


class DetectionVisualizer:
    """utilities for visualizing champion detections and grid positions."""

    # color palette for visualization (BGR format)
    COLOR_BBOX = (0, 255, 0)  # green
    COLOR_GRID = (255, 0, 0)  # blue
    COLOR_TEXT = (255, 255, 255)  # white
    COLOR_ROI = (0, 255, 255)  # yellow
    COLOR_CHAMPION = (0, 165, 255)  # orange

    def __init__(
        self,
        grid_config: GridConfig | None = None,
        font_scale: float = 0.5,
        thickness: int = 2,
    ) -> None:
        """initialize the detection visualizer.

        Args:
            grid_config: optional grid configuration for drawing grid positions
            font_scale: scale factor for text
            thickness: line thickness for drawings
        """
        self.grid_config = grid_config
        self.font_scale = font_scale
        self.thickness = thickness
        self.font = cv2.FONT_HERSHEY_SIMPLEX

    def draw_bbox(
        self,
        frame: NDArray[np.uint8],
        bbox: Tuple[int, int, int, int],
        label: str | None = None,
        confidence: float | None = None,
        color: Tuple[int, int, int] | None = None,
    ) -> NDArray[np.uint8]:
        """draw a bounding box on a frame.

        Args:
            frame: input frame (will be copied, not modified in-place)
            bbox: bounding box as (x, y, width, height)
            label: optional label text
            confidence: optional confidence score
            color: optional color (BGR), defaults to green

        Returns:
            frame with bounding box drawn
        """
        frame = frame.copy()
        color = color or self.COLOR_BBOX

        x, y, w, h = bbox
        # draw rectangle
        cv2.rectangle(
            frame,
            (x, y),
            (x + w, y + h),
            color,
            self.thickness,
        )

        # draw label if provided
        if label is not None:
            text = label
            if confidence is not None:
                text = f"{label} {confidence:.2f}"

            # calculate text size for background
            (text_width, text_height), baseline = cv2.getTextSize(
                text, self.font, self.font_scale, self.thickness
            )

            # draw background rectangle for text
            cv2.rectangle(
                frame,
                (x, y - text_height - baseline - 5),
                (x + text_width, y),
                color,
                -1,  # filled
            )

            # draw text
            cv2.putText(
                frame,
                text,
                (x, y - baseline - 2),
                self.font,
                self.font_scale,
                self.COLOR_TEXT,
                self.thickness,
            )

        return frame

    def draw_multiple_bboxes(
        self,
        frame: NDArray[np.uint8],
        detections: List[Tuple[Tuple[int, int, int, int], str, float]],
    ) -> NDArray[np.uint8]:
        """draw multiple bounding boxes on a frame.

        Args:
            frame: input frame
            detections: list of (bbox, label, confidence) tuples

        Returns:
            frame with all bounding boxes drawn
        """
        result = frame.copy()
        for bbox, label, confidence in detections:
            result = self.draw_bbox(result, bbox, label, confidence)
        return result

    def draw_grid_positions(
        self,
        frame: NDArray[np.uint8],
        show_labels: bool = True,
    ) -> NDArray[np.uint8]:
        """draw grid positions on a frame.

        requires grid_config to be set.

        Args:
            frame: input frame
            show_labels: whether to show position labels (row, col)

        Returns:
            frame with grid positions drawn

        Raises:
            ValueError: if grid_config is not set
        """
        if self.grid_config is None:
            raise ValueError("grid_config must be set to draw grid positions")

        frame = frame.copy()

        # draw board positions
        for (row, col), (px, py) in self.grid_config.board_positions.items():
            # draw circle at grid position
            cv2.circle(
                frame,
                (int(px), int(py)),
                5,
                self.COLOR_GRID,
                -1,
            )

            # draw label if requested
            if show_labels:
                label = f"({row},{col})"
                cv2.putText(
                    frame,
                    label,
                    (int(px) + 10, int(py)),
                    self.font,
                    0.3,
                    self.COLOR_TEXT,
                    1,
                )

        # draw bench positions
        for col, (px, py) in self.grid_config.bench_positions.items():
            # draw square for bench positions
            size = 5
            cv2.rectangle(
                frame,
                (int(px) - size, int(py) - size),
                (int(px) + size, int(py) + size),
                self.COLOR_GRID,
                -1,
            )

            # draw label if requested
            if show_labels:
                label = f"B{col}"
                cv2.putText(
                    frame,
                    label,
                    (int(px) + 10, int(py)),
                    self.font,
                    0.3,
                    self.COLOR_WHITE,
                    1,
                )

        return frame

    def draw_champion_at_position(
        self,
        frame: NDArray[np.uint8],
        grid_pos: GridPosition,
        champion_shorthand: str,
        star_level: int,
    ) -> NDArray[np.uint8]:
        """draw a champion marker at a grid position.

        Args:
            frame: input frame
            grid_pos: grid position where champion is located
            champion_shorthand: champion shorthand code (e.g., "ANI")
            star_level: star level (1, 2, or 3)

        Returns:
            frame with champion marker drawn

        Raises:
            ValueError: if grid_config is not set
        """
        if self.grid_config is None:
            raise ValueError("grid_config must be set to draw champions")

        frame = frame.copy()

        # get pixel position
        px, py = self.grid_config.get_pixel_position(grid_pos)

        # draw champion marker (larger circle)
        cv2.circle(
            frame,
            (int(px), int(py)),
            15,
            self.COLOR_CHAMPION,
            -1,
        )

        # draw champion label
        label = f"{star_level}★{champion_shorthand}"
        cv2.putText(
            frame,
            label,
            (int(px) + 20, int(py)),
            self.font,
            self.font_scale,
            self.COLOR_TEXT,
            self.thickness,
        )

        return frame

    def draw_roi_overlay(
        self,
        frame: NDArray[np.uint8],
        roi: Tuple[int, int, int, int],
        label: str | None = None,
    ) -> NDArray[np.uint8]:
        """draw an ROI rectangle overlay on a frame.

        Args:
            frame: input frame
            roi: region as (x, y, width, height)
            label: optional label for the ROI

        Returns:
            frame with ROI overlay drawn
        """
        frame = frame.copy()
        x, y, w, h = roi

        # draw ROI rectangle
        cv2.rectangle(
            frame,
            (x, y),
            (x + w, y + h),
            self.COLOR_ROI,
            self.thickness,
        )

        # draw label if provided
        if label is not None:
            cv2.putText(
                frame,
                label,
                (x, y - 10),
                self.font,
                self.font_scale,
                self.COLOR_ROI,
                self.thickness,
            )

        return frame

    def create_detection_summary(
        self,
        frame: NDArray[np.uint8],
        detections: List[Tuple[Tuple[int, int, int, int], str, float]],
        grid_positions: List[Tuple[GridPosition, str, int]] | None = None,
    ) -> NDArray[np.uint8]:
        """create a comprehensive visualization with all detection info.

        Args:
            frame: input frame
            detections: list of (bbox, champion, confidence) tuples
            grid_positions: optional list of (grid_pos, champion, star_level)

        Returns:
            frame with complete visualization
        """
        result = frame.copy()

        # draw grid if available
        if self.grid_config is not None:
            result = self.draw_grid_positions(result, show_labels=False)

        # draw bounding boxes
        result = self.draw_multiple_bboxes(result, detections)

        # draw champions at grid positions if provided
        if grid_positions is not None and self.grid_config is not None:
            for grid_pos, champion, star_level in grid_positions:
                result = self.draw_champion_at_position(
                    result, grid_pos, champion, star_level
                )

        return result


def save_visualization(
    frame: NDArray[np.uint8],
    output_path: str,
) -> None:
    """save a visualization frame to disk.

    Args:
        frame: frame to save
        output_path: path where to save the image
    """
    cv2.imwrite(output_path, frame)


def show_visualization(
    frame: NDArray[np.uint8],
    window_name: str = "TFT Detection",
    wait_key: int = 0,
) -> None:
    """display a visualization frame in a window.

    Args:
        frame: frame to display
        window_name: name of the display window
        wait_key: milliseconds to wait (0 = wait indefinitely)
    """
    cv2.imshow(window_name, frame)
    cv2.waitKey(wait_key)
    cv2.destroyAllWindows()


# color constant that was referenced
COLOR_WHITE = (255, 255, 255)
