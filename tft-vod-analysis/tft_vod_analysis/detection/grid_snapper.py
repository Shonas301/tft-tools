"""grid snapping utilities for mapping detections to TFT board positions."""

from dataclasses import dataclass
from typing import Dict, List, Tuple

import numpy as np

from tft_vod_analysis.data.grid_config import GridConfig, GridPosition


@dataclass
class Detection:
    """represents a champion detection from the model."""

    bbox: Tuple[int, int, int, int]  # (x, y, width, height) in pixels
    champion_shorthand: str
    confidence: float
    star_level: int = 1

    @property
    def center(self) -> Tuple[float, float]:
        """get bounding box center coordinates."""
        x, y, w, h = self.bbox
        return (x + w / 2, y + h / 2)

    @property
    def area(self) -> int:
        """get bounding box area."""
        return self.bbox[2] * self.bbox[3]


@dataclass
class SnappedDetection:
    """a detection that has been snapped to a grid position."""

    detection: Detection
    grid_position: GridPosition
    distance_to_grid: float  # distance from detection center to grid center


@dataclass
class SnappingResult:
    """result of snapping multiple detections to the grid."""

    snapped: List[SnappedDetection]
    conflicts: List[Tuple[GridPosition, List[Detection]]]  # positions with multiple detections
    unassigned: List[Detection]  # detections too far from any grid position


class GridSnapper:
    """snaps champion detections to TFT grid positions.

    handles:
    - mapping detection centers to nearest grid positions
    - conflict resolution when multiple detections map to same position
    - filtering detections too far from valid positions
    """

    def __init__(
        self,
        grid_config: GridConfig,
        max_distance: float = 50.0,
        conflict_resolution: str = "highest_confidence",
    ) -> None:
        """initialize the grid snapper.

        Args:
            grid_config: grid configuration with pixel positions
            max_distance: maximum distance from grid center for valid snap
            conflict_resolution: strategy for resolving conflicts
                - "highest_confidence": keep detection with highest confidence
                - "largest_bbox": keep detection with largest bounding box
                - "nearest": keep detection closest to grid center
        """
        self.grid_config = grid_config
        self.max_distance = max_distance
        self.conflict_resolution = conflict_resolution

    def snap_single(
        self, detection: Detection, board_only: bool = False
    ) -> SnappedDetection | None:
        """snap a single detection to the nearest grid position.

        Args:
            detection: detection to snap
            board_only: if True, only consider board positions (not bench)

        Returns:
            SnappedDetection if within max_distance, None otherwise
        """
        cx, cy = detection.center
        grid_pos = self.grid_config.snap_to_grid(cx, cy, board_only=board_only)

        # calculate distance
        px, py = self.grid_config.get_pixel_position(grid_pos)
        distance = np.sqrt((cx - px) ** 2 + (cy - py) ** 2)

        if distance > self.max_distance:
            return None

        return SnappedDetection(
            detection=detection,
            grid_position=grid_pos,
            distance_to_grid=distance,
        )

    def snap_multiple(
        self,
        detections: List[Detection],
        board_only: bool = False,
    ) -> SnappingResult:
        """snap multiple detections to grid positions with conflict resolution.

        Args:
            detections: list of detections to snap
            board_only: if True, only consider board positions

        Returns:
            SnappingResult with snapped detections, conflicts, and unassigned
        """
        # first pass: snap all detections
        position_map: Dict[GridPosition, List[SnappedDetection]] = {}
        unassigned: List[Detection] = []

        for detection in detections:
            snapped = self.snap_single(detection, board_only)
            if snapped is None:
                unassigned.append(detection)
            else:
                if snapped.grid_position not in position_map:
                    position_map[snapped.grid_position] = []
                position_map[snapped.grid_position].append(snapped)

        # resolve conflicts
        final_snapped: List[SnappedDetection] = []
        conflicts: List[Tuple[GridPosition, List[Detection]]] = []

        for position, candidates in position_map.items():
            if len(candidates) == 1:
                final_snapped.append(candidates[0])
            else:
                # conflict - multiple detections for same position
                winner = self._resolve_conflict(candidates)
                final_snapped.append(winner)

                # record conflict for reporting
                conflict_detections = [c.detection for c in candidates]
                conflicts.append((position, conflict_detections))

        return SnappingResult(
            snapped=final_snapped,
            conflicts=conflicts,
            unassigned=unassigned,
        )

    def _resolve_conflict(
        self, candidates: List[SnappedDetection]
    ) -> SnappedDetection:
        """resolve conflict between multiple detections at same position.

        Args:
            candidates: list of candidate detections

        Returns:
            winning detection based on conflict resolution strategy
        """
        if self.conflict_resolution == "highest_confidence":
            return max(candidates, key=lambda c: c.detection.confidence)
        elif self.conflict_resolution == "largest_bbox":
            return max(candidates, key=lambda c: c.detection.area)
        elif self.conflict_resolution == "nearest":
            return min(candidates, key=lambda c: c.distance_to_grid)
        else:
            raise ValueError(
                f"unknown conflict resolution: {self.conflict_resolution}"
            )

    def get_board_state(
        self,
        detections: List[Detection],
    ) -> Dict[GridPosition, Detection]:
        """get a clean board state from detections.

        convenience method that returns a simple mapping of grid positions
        to detections after snapping and conflict resolution.

        Args:
            detections: list of detections

        Returns:
            dict mapping GridPosition to winning Detection
        """
        result = self.snap_multiple(detections)
        return {
            snapped.grid_position: snapped.detection
            for snapped in result.snapped
        }


def detections_from_yolo_results(
    results: list,  # ultralytics Results objects
    class_names: Dict[int, str],
    default_star_level: int = 1,
) -> List[Detection]:
    """convert YOLO inference results to Detection objects.

    Args:
        results: list of ultralytics Results objects
        class_names: mapping from class_id to champion shorthand
        default_star_level: star level to assign (until star detection is implemented)

    Returns:
        list of Detection objects
    """
    detections = []

    for result in results:
        boxes = result.boxes
        if boxes is None:
            continue

        for i in range(len(boxes)):
            # get box coordinates (xyxy format)
            x1, y1, x2, y2 = boxes.xyxy[i].tolist()
            bbox = (int(x1), int(y1), int(x2 - x1), int(y2 - y1))

            # get class and confidence
            class_id = int(boxes.cls[i])
            confidence = float(boxes.conf[i])

            if class_id not in class_names:
                continue

            shorthand = class_names[class_id]

            detections.append(
                Detection(
                    bbox=bbox,
                    champion_shorthand=shorthand,
                    confidence=confidence,
                    star_level=default_star_level,
                )
            )

    return detections


def create_mock_detections(
    positions: List[Tuple[GridPosition, str, int]],
    grid_config: GridConfig,
    confidence: float = 0.95,
    jitter: float = 5.0,
) -> List[Detection]:
    """create mock detections for testing.

    Args:
        positions: list of (grid_position, shorthand, star_level)
        grid_config: grid config for pixel positions
        confidence: confidence to assign
        jitter: random offset to add to positions

    Returns:
        list of mock Detection objects
    """
    detections = []

    for grid_pos, shorthand, star_level in positions:
        px, py = grid_config.get_pixel_position(grid_pos)

        # add jitter
        px += np.random.uniform(-jitter, jitter)
        py += np.random.uniform(-jitter, jitter)

        # create bbox (approximate size)
        width = 60 + (star_level - 1) * 10  # bigger for higher star
        height = 60 + (star_level - 1) * 10

        bbox = (
            int(px - width / 2),
            int(py - height / 2),
            width,
            height,
        )

        detections.append(
            Detection(
                bbox=bbox,
                champion_shorthand=shorthand,
                confidence=confidence,
                star_level=star_level,
            )
        )

    return detections
