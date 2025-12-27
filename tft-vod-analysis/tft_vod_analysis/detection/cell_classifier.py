"""cell-based champion classification for TFT board analysis.

alternative to full object detection - crops each grid cell and
classifies independently. simpler annotation, requires good grid calibration.
"""

from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Protocol, Tuple

import cv2
import numpy as np
from numpy.typing import NDArray

from tft_vod_analysis.data.grid_config import GridConfig, GridPosition


@dataclass
class CellClassification:
    """classification result for a single grid cell."""

    position: GridPosition
    champion_shorthand: str | None  # None if empty
    confidence: float
    star_level: int | None  # None if empty or unknown


@dataclass
class BoardState:
    """complete classification of all board and bench positions."""

    board: Dict[Tuple[int, int], CellClassification]  # (row, col) -> classification
    bench: Dict[int, CellClassification]  # col -> classification


class ClassifierProtocol(Protocol):
    """protocol for cell classifiers (allows different backends)."""

    def classify(
        self, cell_image: NDArray[np.uint8]
    ) -> Tuple[str | None, float, int | None]:
        """classify a cell image.

        Args:
            cell_image: cropped cell image

        Returns:
            tuple of (champion_shorthand, confidence, star_level)
            shorthand is None for empty cells
        """
        ...


@dataclass
class CellConfig:
    """configuration for cell extraction from grid positions."""

    cell_width: int
    cell_height: int
    offset_x: int = 0  # offset from grid center
    offset_y: int = 0


class CellExtractor:
    """extracts individual cells from frames based on grid configuration."""

    def __init__(
        self,
        grid_config: GridConfig,
        cell_config: CellConfig,
    ) -> None:
        """initialize the cell extractor.

        Args:
            grid_config: grid configuration with pixel positions
            cell_config: cell extraction configuration
        """
        self.grid_config = grid_config
        self.cell_config = cell_config

    def extract_cell(
        self,
        frame: NDArray[np.uint8],
        position: GridPosition,
    ) -> NDArray[np.uint8]:
        """extract a single cell from the frame.

        Args:
            frame: input frame
            position: grid position to extract

        Returns:
            cropped cell image

        Raises:
            ValueError: if cell is out of frame bounds
        """
        # get pixel center
        px, py = self.grid_config.get_pixel_position(position)

        # calculate cell bounds
        half_w = self.cell_config.cell_width // 2
        half_h = self.cell_config.cell_height // 2

        x1 = int(px + self.cell_config.offset_x - half_w)
        y1 = int(py + self.cell_config.offset_y - half_h)
        x2 = x1 + self.cell_config.cell_width
        y2 = y1 + self.cell_config.cell_height

        # validate bounds
        frame_h, frame_w = frame.shape[:2]
        if x1 < 0 or y1 < 0 or x2 > frame_w or y2 > frame_h:
            raise ValueError(
                f"cell at {position} out of bounds: "
                f"({x1}, {y1}, {x2}, {y2}) vs frame {frame_w}x{frame_h}"
            )

        return frame[y1:y2, x1:x2].copy()

    def extract_all_cells(
        self,
        frame: NDArray[np.uint8],
        include_board: bool = True,
        include_bench: bool = True,
    ) -> Dict[GridPosition, NDArray[np.uint8]]:
        """extract all cells from the frame.

        Args:
            frame: input frame
            include_board: whether to extract board cells
            include_bench: whether to extract bench cells

        Returns:
            dict mapping GridPosition to cropped cell images
        """
        cells = {}

        if include_board:
            for row in range(4):
                for col in range(7):
                    pos = GridPosition(row, col)
                    try:
                        cells[pos] = self.extract_cell(frame, pos)
                    except ValueError:
                        pass  # skip out-of-bounds cells

        if include_bench:
            for col in range(9):
                pos = GridPosition(4, col)
                try:
                    cells[pos] = self.extract_cell(frame, pos)
                except ValueError:
                    pass

        return cells


class CellClassificationPipeline:
    """end-to-end pipeline for cell-based champion detection."""

    def __init__(
        self,
        grid_config: GridConfig,
        cell_config: CellConfig,
        classifier: ClassifierProtocol,
    ) -> None:
        """initialize the classification pipeline.

        Args:
            grid_config: grid configuration
            cell_config: cell extraction configuration
            classifier: cell classifier implementation
        """
        self.extractor = CellExtractor(grid_config, cell_config)
        self.classifier = classifier

    def classify_frame(
        self,
        frame: NDArray[np.uint8],
        include_board: bool = True,
        include_bench: bool = True,
        confidence_threshold: float = 0.5,
    ) -> BoardState:
        """classify all cells in a frame.

        Args:
            frame: input frame
            include_board: whether to classify board cells
            include_bench: whether to classify bench cells
            confidence_threshold: minimum confidence for non-empty classification

        Returns:
            BoardState with all classifications
        """
        cells = self.extractor.extract_all_cells(
            frame, include_board, include_bench
        )

        board_results: Dict[Tuple[int, int], CellClassification] = {}
        bench_results: Dict[int, CellClassification] = {}

        for position, cell_image in cells.items():
            shorthand, confidence, star_level = self.classifier.classify(cell_image)

            # apply confidence threshold
            if confidence < confidence_threshold:
                shorthand = None
                star_level = None

            classification = CellClassification(
                position=position,
                champion_shorthand=shorthand,
                confidence=confidence,
                star_level=star_level,
            )

            if position.is_board():
                board_results[position.to_tuple()] = classification
            else:
                bench_results[position.col] = classification

        return BoardState(board=board_results, bench=bench_results)


class TemplateMatchingClassifier:
    """simple classifier using template matching.

    useful as a baseline or when you have champion sprites but no trained model.
    """

    def __init__(
        self,
        templates_dir: Path | str,
        champion_shorthands: List[str],
        template_size: Tuple[int, int] = (64, 64),
        match_threshold: float = 0.7,
    ) -> None:
        """initialize the template matching classifier.

        Args:
            templates_dir: directory containing champion templates
            champion_shorthands: list of champion shorthands to match
            template_size: size to resize templates to
            match_threshold: minimum correlation for a match
        """
        self.templates_dir = Path(templates_dir)
        self.template_size = template_size
        self.match_threshold = match_threshold

        # load templates
        self.templates: Dict[str, NDArray[np.uint8]] = {}
        self._load_templates(champion_shorthands)

    def _load_templates(self, shorthands: List[str]) -> None:
        """load champion templates from disk."""
        for shorthand in shorthands:
            for ext in ["png", "jpg", "jpeg"]:
                template_path = self.templates_dir / f"{shorthand}.{ext}"
                if template_path.exists():
                    template = cv2.imread(str(template_path))
                    if template is not None:
                        template = cv2.resize(template, self.template_size)
                        self.templates[shorthand] = template
                    break

    def classify(
        self, cell_image: NDArray[np.uint8]
    ) -> Tuple[str | None, float, int | None]:
        """classify a cell using template matching.

        Args:
            cell_image: cropped cell image

        Returns:
            tuple of (champion_shorthand, confidence, star_level)
        """
        if not self.templates:
            return (None, 0.0, None)

        # resize cell to template size
        cell_resized = cv2.resize(cell_image, self.template_size)

        best_match = None
        best_score = 0.0

        for shorthand, template in self.templates.items():
            # calculate normalized cross-correlation
            result = cv2.matchTemplate(
                cell_resized, template, cv2.TM_CCOEFF_NORMED
            )
            score = float(result.max())

            if score > best_score:
                best_score = score
                best_match = shorthand

        if best_score >= self.match_threshold and best_match is not None:
            return (best_match, best_score, 1)  # default to 1-star
        return (None, best_score, None)


def create_default_cell_config_1080p() -> CellConfig:
    """create default cell configuration for 1080p resolution.

    placeholder values - need calibration from actual frames.

    Returns:
        CellConfig for 1080p
    """
    return CellConfig(
        cell_width=80,
        cell_height=80,
        offset_x=0,
        offset_y=-10,  # champions often rendered above center
    )
