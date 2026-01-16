"""calibration tool for marking grid positions and ROI regions.

interactive tool for calibrating the detection system to a specific
screen resolution by marking grid positions and ROI boundaries.
"""

import json
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, List, Tuple

import cv2
import numpy as np
from numpy.typing import NDArray


@dataclass
class CalibrationData:
    """stores calibration data for a specific resolution."""

    resolution: Tuple[int, int]
    board_positions: Dict[Tuple[int, int], Tuple[float, float]] = field(
        default_factory=dict
    )
    bench_positions: Dict[int, Tuple[float, float]] = field(default_factory=dict)
    board_roi: Tuple[int, int, int, int] | None = None
    bench_roi: Tuple[int, int, int, int] | None = None
    gold_roi: Tuple[int, int, int, int] | None = None
    health_roi: Tuple[int, int, int, int] | None = None
    level_roi: Tuple[int, int, int, int] | None = None
    stage_roi: Tuple[int, int, int, int] | None = None

    def is_grid_complete(self) -> bool:
        """check if all grid positions have been marked."""
        return len(self.board_positions) == 28 and len(self.bench_positions) == 9

    def is_roi_complete(self) -> bool:
        """check if all ROI regions have been marked."""
        return all([
            self.board_roi is not None,
            self.bench_roi is not None,
            self.gold_roi is not None,
            self.health_roi is not None,
            self.level_roi is not None,
            self.stage_roi is not None,
        ])

    def to_dict(self) -> dict:
        """convert to dictionary for JSON serialization."""
        return {
            "resolution": list(self.resolution),
            "board_positions": {
                f"{r},{c}": list(pos)
                for (r, c), pos in self.board_positions.items()
            },
            "bench_positions": {
                str(c): list(pos) for c, pos in self.bench_positions.items()
            },
            "board_roi": list(self.board_roi) if self.board_roi else None,
            "bench_roi": list(self.bench_roi) if self.bench_roi else None,
            "gold_roi": list(self.gold_roi) if self.gold_roi else None,
            "health_roi": list(self.health_roi) if self.health_roi else None,
            "level_roi": list(self.level_roi) if self.level_roi else None,
            "stage_roi": list(self.stage_roi) if self.stage_roi else None,
        }

    @classmethod
    def from_dict(cls, data: dict) -> "CalibrationData":
        """create from dictionary (JSON deserialization)."""
        board_positions = {
            tuple(map(int, k.split(","))): tuple(v)
            for k, v in data.get("board_positions", {}).items()
        }
        bench_positions = {
            int(k): tuple(v) for k, v in data.get("bench_positions", {}).items()
        }

        return cls(
            resolution=tuple(data["resolution"]),
            board_positions=board_positions,
            bench_positions=bench_positions,
            board_roi=tuple(data["board_roi"]) if data.get("board_roi") else None,
            bench_roi=tuple(data["bench_roi"]) if data.get("bench_roi") else None,
            gold_roi=tuple(data["gold_roi"]) if data.get("gold_roi") else None,
            health_roi=tuple(data["health_roi"]) if data.get("health_roi") else None,
            level_roi=tuple(data["level_roi"]) if data.get("level_roi") else None,
            stage_roi=tuple(data["stage_roi"]) if data.get("stage_roi") else None,
        )

    def generate_python_code(self) -> str:
        """generate Python code to create configs from this calibration."""
        lines = [
            "# auto-generated calibration code",
            f"# resolution: {self.resolution[0]}x{self.resolution[1]}",
            "",
            "from tft_vod_analysis.data.grid_config import create_grid_from_calibration",
            "from tft_vod_analysis.frame_processing.preprocessor import create_roi_config_from_calibration",
            "",
            "# grid configuration",
            "board_coords = {",
        ]

        # board positions
        for row in range(4):
            for col in range(7):
                if (row, col) in self.board_positions:
                    px, py = self.board_positions[(row, col)]
                    lines.append(f"    ({row}, {col}): ({px:.1f}, {py:.1f}),")

        lines.append("}")
        lines.append("")
        lines.append("bench_coords = {")

        # bench positions
        for col in range(9):
            if col in self.bench_positions:
                px, py = self.bench_positions[col]
                lines.append(f"    {col}: ({px:.1f}, {py:.1f}),")

        lines.append("}")
        lines.append("")

        # grid config creation
        lines.append(f"grid_config = create_grid_from_calibration(")
        lines.append(f"    resolution={self.resolution},")
        lines.append(f"    board_coords=board_coords,")
        lines.append(f"    bench_coords=bench_coords,")
        lines.append(f")")
        lines.append("")

        # roi config
        if self.is_roi_complete():
            lines.append("# roi configuration")
            lines.append(f"roi_config = create_roi_config_from_calibration(")
            lines.append(f"    resolution={self.resolution},")
            lines.append(f"    board_roi={self.board_roi},")
            lines.append(f"    bench_roi={self.bench_roi},")
            lines.append(f"    gold_roi={self.gold_roi},")
            lines.append(f"    health_roi={self.health_roi},")
            lines.append(f"    level_roi={self.level_roi},")
            lines.append(f"    stage_roi={self.stage_roi},")
            lines.append(f")")

        return "\n".join(lines)


class GridCalibrationTool:
    """interactive tool for calibrating grid positions.

    usage:
        tool = GridCalibrationTool("reference_frame.png")
        tool.run()
        tool.save("calibration.json")
    """

    # colors (BGR)
    COLOR_BOARD = (0, 255, 0)  # green
    COLOR_BENCH = (255, 0, 0)  # blue
    COLOR_CURRENT = (0, 0, 255)  # red
    COLOR_TEXT = (255, 255, 255)  # white
    COLOR_ROI = (0, 255, 255)  # yellow

    def __init__(self, frame_path: Path | str) -> None:
        """initialize the calibration tool.

        Args:
            frame_path: path to reference frame image

        Raises:
            FileNotFoundError: if frame doesn't exist
            ValueError: if frame can't be loaded
        """
        self.frame_path = Path(frame_path)
        if not self.frame_path.exists():
            raise FileNotFoundError(f"frame not found: {self.frame_path}")

        self.original_frame = cv2.imread(str(self.frame_path))
        if self.original_frame is None:
            raise ValueError(f"failed to load frame: {self.frame_path}")

        height, width = self.original_frame.shape[:2]
        self.calibration = CalibrationData(resolution=(width, height))

        # state for interactive marking
        self.current_row = 0
        self.current_col = 0
        self.mode = "board"  # "board", "bench", or roi names
        self.roi_start = None  # for drawing roi rectangles

        # window name
        self.window_name = "TFT Grid Calibration"

    def _get_current_position_name(self) -> str:
        """get human-readable name for current position to mark."""
        if self.mode == "board":
            return f"Board ({self.current_row}, {self.current_col})"
        elif self.mode == "bench":
            return f"Bench slot {self.current_col}"
        else:
            return f"ROI: {self.mode}"

    def _advance_position(self) -> bool:
        """advance to next position to mark.

        Returns:
            True if there are more positions, False if done with current mode
        """
        if self.mode == "board":
            self.current_col += 1
            if self.current_col >= 7:
                self.current_col = 0
                self.current_row += 1
            if self.current_row >= 4:
                # done with board, move to bench
                self.mode = "bench"
                self.current_row = 4
                self.current_col = 0
                return True
            return True
        elif self.mode == "bench":
            self.current_col += 1
            if self.current_col >= 9:
                # done with bench, move to ROI
                self.mode = "board_roi"
                return True
            return True
        else:
            # roi modes
            roi_order = [
                "board_roi", "bench_roi", "gold_roi",
                "health_roi", "level_roi", "stage_roi", "done"
            ]
            current_idx = roi_order.index(self.mode)
            if current_idx < len(roi_order) - 1:
                self.mode = roi_order[current_idx + 1]
                return self.mode != "done"
            return False

    def _draw_overlay(self) -> NDArray[np.uint8]:
        """draw current state overlay on frame."""
        frame = self.original_frame.copy()

        # draw already marked board positions
        for (row, col), (px, py) in self.calibration.board_positions.items():
            cv2.circle(frame, (int(px), int(py)), 5, self.COLOR_BOARD, -1)
            cv2.putText(
                frame, f"{row},{col}",
                (int(px) + 8, int(py) + 4),
                cv2.FONT_HERSHEY_SIMPLEX, 0.3, self.COLOR_TEXT, 1
            )

        # draw already marked bench positions
        for col, (px, py) in self.calibration.bench_positions.items():
            cv2.circle(frame, (int(px), int(py)), 5, self.COLOR_BENCH, -1)
            cv2.putText(
                frame, f"B{col}",
                (int(px) + 8, int(py) + 4),
                cv2.FONT_HERSHEY_SIMPLEX, 0.3, self.COLOR_TEXT, 1
            )

        # draw already marked ROIs
        for roi_name, color in [
            ("board_roi", (0, 255, 0)),
            ("bench_roi", (255, 0, 0)),
            ("gold_roi", (0, 255, 255)),
            ("health_roi", (255, 0, 255)),
            ("level_roi", (255, 255, 0)),
            ("stage_roi", (0, 128, 255)),
        ]:
            roi = getattr(self.calibration, roi_name)
            if roi is not None:
                x, y, w, h = roi
                cv2.rectangle(frame, (x, y), (x + w, y + h), color, 2)
                cv2.putText(
                    frame, roi_name.replace("_roi", ""),
                    (x, y - 5),
                    cv2.FONT_HERSHEY_SIMPLEX, 0.4, color, 1
                )

        # draw instructions
        instructions = [
            f"Current: {self._get_current_position_name()}",
            "Click to mark position" if self.mode in ["board", "bench"] else "Drag to draw ROI",
            "Press 'u' to undo last",
            "Press 's' to save and quit",
            "Press 'q' to quit without saving",
            "Press 'n' to skip current position",
        ]

        y_offset = 30
        for instruction in instructions:
            cv2.putText(
                frame, instruction,
                (10, y_offset),
                cv2.FONT_HERSHEY_SIMPLEX, 0.6, self.COLOR_TEXT, 2
            )
            y_offset += 25

        # show progress
        board_done = len(self.calibration.board_positions)
        bench_done = len(self.calibration.bench_positions)
        progress = f"Progress: Board {board_done}/28, Bench {bench_done}/9"
        cv2.putText(
            frame, progress,
            (10, frame.shape[0] - 10),
            cv2.FONT_HERSHEY_SIMPLEX, 0.6, self.COLOR_TEXT, 2
        )

        return frame

    def _mouse_callback(self, event: int, x: int, y: int, flags: int, param: object) -> None:
        """handle mouse events."""
        if self.mode in ["board", "bench"]:
            if event == cv2.EVENT_LBUTTONDOWN:
                # mark grid position
                if self.mode == "board":
                    self.calibration.board_positions[
                        (self.current_row, self.current_col)
                    ] = (float(x), float(y))
                else:
                    self.calibration.bench_positions[self.current_col] = (
                        float(x), float(y)
                    )
                self._advance_position()
        else:
            # roi drawing mode
            if event == cv2.EVENT_LBUTTONDOWN:
                self.roi_start = (x, y)
            elif event == cv2.EVENT_LBUTTONUP and self.roi_start is not None:
                x1, y1 = self.roi_start
                x2, y2 = x, y
                # normalize to (x, y, w, h)
                roi = (
                    min(x1, x2),
                    min(y1, y2),
                    abs(x2 - x1),
                    abs(y2 - y1),
                )
                setattr(self.calibration, self.mode, roi)
                self.roi_start = None
                self._advance_position()

    def _undo_last(self) -> None:
        """undo the last marked position."""
        if self.mode == "board":
            if self.current_col > 0:
                self.current_col -= 1
            elif self.current_row > 0:
                self.current_row -= 1
                self.current_col = 6
            key = (self.current_row, self.current_col)
            if key in self.calibration.board_positions:
                del self.calibration.board_positions[key]
        elif self.mode == "bench":
            if self.current_col > 0:
                self.current_col -= 1
                if self.current_col in self.calibration.bench_positions:
                    del self.calibration.bench_positions[self.current_col]
            else:
                # go back to board
                self.mode = "board"
                self.current_row = 3
                self.current_col = 6
        else:
            # undo roi
            roi_order = [
                "board_roi", "bench_roi", "gold_roi",
                "health_roi", "level_roi", "stage_roi"
            ]
            current_idx = roi_order.index(self.mode) if self.mode in roi_order else -1
            if current_idx > 0:
                prev_roi = roi_order[current_idx - 1]
                setattr(self.calibration, prev_roi, None)
                self.mode = prev_roi
            elif current_idx == 0:
                # go back to bench
                self.mode = "bench"
                self.current_col = 8

    def run(self) -> CalibrationData:
        """run the interactive calibration tool.

        Returns:
            CalibrationData with marked positions
        """
        cv2.namedWindow(self.window_name)
        cv2.setMouseCallback(self.window_name, self._mouse_callback)

        while True:
            frame = self._draw_overlay()
            cv2.imshow(self.window_name, frame)

            key = cv2.waitKey(1) & 0xFF

            if key == ord('q'):
                # quit without saving
                break
            elif key == ord('s'):
                # save and quit
                break
            elif key == ord('u'):
                # undo
                self._undo_last()
            elif key == ord('n'):
                # skip current position
                self._advance_position()

            if self.mode == "done":
                break

        cv2.destroyAllWindows()
        return self.calibration

    def save(self, output_path: Path | str) -> None:
        """save calibration data to JSON file.

        Args:
            output_path: path to save JSON file
        """
        output_path = Path(output_path)
        with output_path.open("w") as f:
            json.dump(self.calibration.to_dict(), f, indent=2)

    def save_python_code(self, output_path: Path | str) -> None:
        """save generated Python code to file.

        Args:
            output_path: path to save Python file
        """
        output_path = Path(output_path)
        with output_path.open("w") as f:
            f.write(self.calibration.generate_python_code())


def load_calibration(path: Path | str) -> CalibrationData:
    """load calibration data from JSON file.

    Args:
        path: path to JSON file

    Returns:
        CalibrationData instance
    """
    path = Path(path)
    with path.open() as f:
        data = json.load(f)
    return CalibrationData.from_dict(data)


def run_calibration_cli(
    frame_path: str,
    output_json: str = "calibration.json",
    output_python: str = "calibration_config.py",
) -> None:
    """run calibration tool from command line.

    Args:
        frame_path: path to reference frame
        output_json: path for JSON output
        output_python: path for Python code output
    """
    print(f"Loading frame: {frame_path}")
    tool = GridCalibrationTool(frame_path)

    print("\nInstructions:")
    print("  - Click to mark each grid position (board then bench)")
    print("  - Drag to draw ROI rectangles")
    print("  - Press 'u' to undo last mark")
    print("  - Press 'n' to skip current position")
    print("  - Press 's' to save and quit")
    print("  - Press 'q' to quit without saving")
    print()

    calibration = tool.run()

    if calibration.board_positions or calibration.bench_positions:
        print(f"\nSaving calibration to: {output_json}")
        tool.save(output_json)

        print(f"Saving Python code to: {output_python}")
        tool.save_python_code(output_python)

        print("\nCalibration summary:")
        print(f"  Board positions marked: {len(calibration.board_positions)}/28")
        print(f"  Bench positions marked: {len(calibration.bench_positions)}/9")
        print(f"  Grid complete: {calibration.is_grid_complete()}")
        print(f"  ROIs complete: {calibration.is_roi_complete()}")
    else:
        print("\nNo positions marked, nothing saved.")


if __name__ == "__main__":
    import sys

    if len(sys.argv) < 2:
        print("Usage: python calibration.py <frame_path> [output_json] [output_python]")
        sys.exit(1)

    frame_path = sys.argv[1]
    output_json = sys.argv[2] if len(sys.argv) > 2 else "calibration.json"
    output_python = sys.argv[3] if len(sys.argv) > 3 else "calibration_config.py"

    run_calibration_cli(frame_path, output_json, output_python)
