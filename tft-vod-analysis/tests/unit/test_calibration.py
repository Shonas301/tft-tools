"""unit tests for calibration tool."""

import json
from pathlib import Path

import pytest

from tft_vod_analysis.utils.calibration import (
    CalibrationData,
    load_calibration,
)


@pytest.fixture
def empty_calibration() -> CalibrationData:
    """create empty calibration data."""
    return CalibrationData(resolution=(1920, 1080))


@pytest.fixture
def partial_calibration() -> CalibrationData:
    """create calibration with some positions marked."""
    return CalibrationData(
        resolution=(1920, 1080),
        board_positions={
            (0, 0): (100.0, 100.0),
            (0, 1): (180.0, 100.0),
            (1, 0): (100.0, 170.0),
        },
        bench_positions={
            0: (100.0, 900.0),
            1: (200.0, 900.0),
        },
    )


@pytest.fixture
def complete_calibration() -> CalibrationData:
    """create calibration with all positions marked."""
    board_positions = {}
    for row in range(4):
        for col in range(7):
            board_positions[(row, col)] = (
                100.0 + col * 80,
                100.0 + row * 70,
            )

    bench_positions = {col: (100.0 + col * 100, 900.0) for col in range(9)}

    return CalibrationData(
        resolution=(1920, 1080),
        board_positions=board_positions,
        bench_positions=bench_positions,
        board_roi=(100, 50, 600, 400),
        bench_roi=(100, 850, 900, 100),
        gold_roi=(50, 950, 100, 40),
        health_roi=(50, 1000, 100, 40),
        level_roi=(1770, 950, 100, 40),
        stage_roi=(860, 50, 200, 40),
    )


class TestCalibrationData:
    """tests for CalibrationData class."""

    def test_empty_calibration(self, empty_calibration: CalibrationData) -> None:
        """test empty calibration state."""
        assert empty_calibration.resolution == (1920, 1080)
        assert len(empty_calibration.board_positions) == 0
        assert len(empty_calibration.bench_positions) == 0
        assert not empty_calibration.is_grid_complete()
        assert not empty_calibration.is_roi_complete()

    def test_partial_calibration(
        self, partial_calibration: CalibrationData
    ) -> None:
        """test partial calibration state."""
        assert len(partial_calibration.board_positions) == 3
        assert len(partial_calibration.bench_positions) == 2
        assert not partial_calibration.is_grid_complete()
        assert not partial_calibration.is_roi_complete()

    def test_complete_calibration(
        self, complete_calibration: CalibrationData
    ) -> None:
        """test complete calibration state."""
        assert len(complete_calibration.board_positions) == 28
        assert len(complete_calibration.bench_positions) == 9
        assert complete_calibration.is_grid_complete()
        assert complete_calibration.is_roi_complete()

    def test_to_dict(self, partial_calibration: CalibrationData) -> None:
        """test converting to dictionary."""
        data = partial_calibration.to_dict()

        assert data["resolution"] == [1920, 1080]
        assert "0,0" in data["board_positions"]
        assert data["board_positions"]["0,0"] == [100.0, 100.0]
        assert "0" in data["bench_positions"]
        assert data["bench_positions"]["0"] == [100.0, 900.0]

    def test_from_dict(self, partial_calibration: CalibrationData) -> None:
        """test creating from dictionary."""
        data = partial_calibration.to_dict()
        restored = CalibrationData.from_dict(data)

        assert restored.resolution == partial_calibration.resolution
        assert len(restored.board_positions) == len(
            partial_calibration.board_positions
        )
        assert len(restored.bench_positions) == len(
            partial_calibration.bench_positions
        )

    def test_to_dict_from_dict_roundtrip(
        self, complete_calibration: CalibrationData
    ) -> None:
        """test that to_dict and from_dict are inverses."""
        data = complete_calibration.to_dict()
        restored = CalibrationData.from_dict(data)

        assert restored.resolution == complete_calibration.resolution
        assert restored.board_positions == complete_calibration.board_positions
        assert restored.bench_positions == complete_calibration.bench_positions
        assert restored.board_roi == complete_calibration.board_roi
        assert restored.bench_roi == complete_calibration.bench_roi

    def test_generate_python_code(
        self, complete_calibration: CalibrationData
    ) -> None:
        """test Python code generation."""
        code = complete_calibration.generate_python_code()

        # verify code contains expected elements
        assert "from tft_vod_analysis.data.grid_config" in code
        assert "create_grid_from_calibration" in code
        assert "board_coords = {" in code
        assert "bench_coords = {" in code
        assert "(0, 0):" in code
        assert "grid_config = create_grid_from_calibration" in code
        assert "roi_config = create_roi_config_from_calibration" in code
        assert "board_roi=" in code

    def test_generate_python_code_without_roi(
        self, partial_calibration: CalibrationData
    ) -> None:
        """test Python code generation without ROI data."""
        code = partial_calibration.generate_python_code()

        # should have grid code but not roi code
        assert "board_coords = {" in code
        assert "grid_config = create_grid_from_calibration" in code
        # roi config should not be present (incomplete)
        assert "roi_config = create_roi_config_from_calibration" not in code


class TestCalibrationIO:
    """tests for calibration file I/O."""

    def test_save_and_load(
        self,
        complete_calibration: CalibrationData,
        tmp_path: Path,
    ) -> None:
        """test saving and loading calibration data."""
        # save
        output_path = tmp_path / "calibration.json"
        with output_path.open("w") as f:
            json.dump(complete_calibration.to_dict(), f)

        # load
        loaded = load_calibration(output_path)

        assert loaded.resolution == complete_calibration.resolution
        assert loaded.is_grid_complete()
        assert loaded.is_roi_complete()

    def test_load_partial_calibration(self, tmp_path: Path) -> None:
        """test loading calibration with missing optional fields."""
        data = {
            "resolution": [1920, 1080],
            "board_positions": {"0,0": [100, 100]},
            "bench_positions": {},
        }

        output_path = tmp_path / "partial.json"
        with output_path.open("w") as f:
            json.dump(data, f)

        loaded = load_calibration(output_path)

        assert loaded.resolution == (1920, 1080)
        assert len(loaded.board_positions) == 1
        assert loaded.board_roi is None


class TestCalibrationCodeExecution:
    """test that generated code is valid Python."""

    def test_generated_code_is_valid_python(
        self, complete_calibration: CalibrationData
    ) -> None:
        """test that generated code compiles."""
        code = complete_calibration.generate_python_code()

        # this should not raise SyntaxError
        compile(code, "<string>", "exec")

    def test_generated_code_partial_is_valid_python(
        self, partial_calibration: CalibrationData
    ) -> None:
        """test that partial generated code compiles."""
        code = partial_calibration.generate_python_code()

        # this should not raise SyntaxError
        compile(code, "<string>", "exec")
