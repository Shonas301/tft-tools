# TFT VOD Analysis

A Python package for analyzing Teamfight Tactics gameplay videos using computer vision and deep learning. Detects champions from game frames and outputs game state in TSL (TFT Session Language) format.

## Features

- **Video Frame Extraction**: Extract keyframes from gameplay videos with scene change detection
- **Champion Detection**: Two approaches available:
  - YOLOv8 object detection (handles occlusion)
  - Cell-based classification (simpler annotation)
- **Grid Snapping**: Map detections to TFT's hex grid positions
- **TSL Output**: FEN-like notation for board state (e.g., `[2ANI|6/7/7/7/9]`)
- **Synthetic Data Generation**: Auto-generate training data from champion assets

## Installation

**Requirements**: Python 3.12 (PyTorch doesn't support 3.13 yet)

```bash
# set up environment
pyenv install 3.12
pyenv local 3.12
python -m venv .venv
source .venv/bin/activate

# install dependencies
pip install pip-tools
pip-compile requirements.in
pip-compile requirements-dev.in
pip install -r requirements.txt -r requirements-dev.txt
pip install -e .
```

## Quick Start

### Extract Frames from Video
```python
from tft_vod_analysis.frame_processing.frame_extractor import extract_keyframes_for_annotation

frames = extract_keyframes_for_annotation(
    "gameplay.mp4",
    "output/keyframes/",
    frames_per_minute=2.0,
)
```

### Generate Synthetic Training Data
```python
from tft_vod_analysis.data.dataset_builder import SyntheticDatasetBuilder, DatasetConfig
from tft_vod_analysis.data.champion_loader import load_champions
from tft_vod_analysis.data.grid_config import create_default_grid_1080p

builder = SyntheticDatasetBuilder(
    champion_data=load_champions(),
    grid_config=create_default_grid_1080p(),
    assets_dir="path/to/champion/assets/",
)

builder.generate_dataset(DatasetConfig(
    output_dir="training_data/",
    num_frames=500,
))
```

### Format Detections to TSL
```python
from tft_vod_analysis.output.tsl_formatter import format_detections_to_tsl

tsl = format_detections_to_tsl(
    board_detections,
    level=7, stage=3, round_num=5, gold=32, health=67
)
# Output: "7 3-5 32g 67h [2ANI|6/7/7/7/9] [] []"
```

---

## Screen Resolution Considerations

⚠️ **Important**: This system is sensitive to screen resolution. Models and configurations calibrated for one resolution may not work correctly at another.

### Resolution-Dependent Components

| Component | What Changes | Impact |
|-----------|--------------|--------|
| **Grid Positions** | Pixel coordinates for hex cells | Grid snapping fails if miscalibrated |
| **ROI Coordinates** | Board/bench/UI region boundaries | Wrong areas extracted |
| **Champion Size** | Pixel dimensions of models | Detection accuracy drops |
| **UI Element Positions** | Gold/health/level locations | OCR/template matching fails |

### Supported Resolutions

Currently, placeholder configurations exist for:
- **1920×1080** (1080p) - `create_default_grid_1080p()`, `create_default_roi_config_1080p()`

**To add support for other resolutions**, you must calibrate:

1. **Grid positions**: Mark the center pixel of each hex cell (28 board + 9 bench = 37 positions)
2. **ROI regions**: Define bounding boxes for board, bench, and UI elements
3. **Cell dimensions**: Measure champion model sizes at that resolution

### Calibrating for a New Resolution

#### Step 1: Capture a Reference Frame
Take a screenshot during the planning phase with champions on the board.

#### Step 2: Mark Grid Positions
Use an image editor to find the center coordinates of each hex cell:

```python
from tft_vod_analysis.data.grid_config import create_grid_from_calibration

# example for 2560x1440 (scale factor ~1.33 from 1080p)
board_coords = {
    (0, 0): (800.0, 400.0),   # row 0, col 0
    (0, 1): (907.0, 400.0),   # row 0, col 1
    # ... all 28 board positions
}
bench_coords = {
    0: (800.0, 1200.0),
    1: (933.0, 1200.0),
    # ... all 9 bench positions
}

grid_1440p = create_grid_from_calibration(
    resolution=(2560, 1440),
    board_coords=board_coords,
    bench_coords=bench_coords,
)
```

#### Step 3: Define ROI Regions
```python
from tft_vod_analysis.frame_processing.preprocessor import create_roi_config_from_calibration

roi_1440p = create_roi_config_from_calibration(
    resolution=(2560, 1440),
    board_roi=(667, 267, 1227, 800),    # scaled from 1080p
    bench_roi=(667, 1133, 1227, 200),
    gold_roi=(67, 1267, 133, 53),
    health_roi=(67, 1333, 133, 53),
    level_roi=(2360, 1267, 133, 53),
    stage_roi=(1147, 67, 267, 53),
)
```

### Training Across Resolutions

#### Option A: Single Resolution (Recommended for Starting)
- Train and run inference at one resolution only
- Simplest approach, most reliable
- Downscale 4K footage to 1080p before processing

#### Option B: Resolution Normalization
- Resize all frames to a standard resolution (e.g., 1080p) before processing
- Train model on normalized frames
- Pros: One model works for all resolutions
- Cons: May lose detail from higher resolutions

```python
import cv2

def normalize_frame(frame, target_resolution=(1920, 1080)):
    return cv2.resize(frame, target_resolution)
```

#### Option C: Multi-Resolution Training
- Include training data from multiple resolutions
- Use data augmentation with random scaling
- Pros: Most robust model
- Cons: Requires more training data, longer training time

#### Option D: Resolution-Specific Models
- Train separate models for each resolution
- Select model based on input video resolution
- Pros: Optimal accuracy per resolution
- Cons: Multiple models to maintain

### Resolution Quick Reference

| Resolution | Name | Scale from 1080p | Approx Hex Size |
|------------|------|------------------|-----------------|
| 1920×1080 | 1080p (FHD) | 1.00× | ~80px |
| 2560×1440 | 1440p (QHD) | 1.33× | ~107px |
| 3440×1440 | Ultrawide | 1.33× (height) | ~107px |
| 3840×2160 | 4K (UHD) | 2.00× | ~160px |

### Aspect Ratio Considerations

TFT supports different aspect ratios, which affect horizontal positioning:

| Aspect Ratio | Common Resolutions | Notes |
|--------------|-------------------|-------|
| 16:9 | 1080p, 1440p, 4K | Standard, most common |
| 21:9 | 2560×1080, 3440×1440 | Ultrawide, board shifts |
| 16:10 | 1920×1200, 2560×1600 | Slightly taller |

For non-16:9 aspect ratios, you'll need separate grid calibrations as the board position shifts horizontally.

### Recommended Workflow

1. **Determine your target resolution(s)** before starting
2. **Calibrate grid and ROI** for your primary resolution
3. **Stick to one resolution** during initial development
4. **Add multi-resolution support** only after the pipeline works at one resolution
5. **Consider normalizing to 1080p** if you have mixed-resolution footage

---

## Project Structure

```
tft_vod_analysis/
├── data/
│   ├── champion_loader.py    # load champion CSV data
│   ├── grid_config.py        # TFT hex grid positions
│   └── dataset_builder.py    # synthetic training data
├── detection/
│   ├── champion_detector.py  # YOLOv8 wrapper (TBD)
│   ├── cell_classifier.py    # cell-based classification
│   ├── grid_snapper.py       # bbox → grid position
│   └── star_classifier.py    # star level detection (TBD)
├── frame_processing/
│   ├── frame_extractor.py    # video → frames
│   └── preprocessor.py       # ROI extraction
├── output/
│   └── tsl_formatter.py      # detection → TSL format
└── utils/
    └── visualization.py      # debug visualization
```

## TSL Output Format

The system outputs game state in TSL (TFT Session Language) FEN notation:

```
<level> <stage>-<round> <gold>g <health>h [<board>] [<items>] [<augments>]
```

**Board FEN Format**: `[row0/row1/row2/row3/bench]`
- Champion: `<star><SHORTHAND>` (e.g., `2ANI` for 2-star Anivia)
- Empty cells: number (e.g., `7` for empty row)

**Examples**:
```
# empty board
7 1-1 50g 100h [7/7/7/7/9] [] []

# 2-star Anivia at (0,0), 1-star Blitzcrank on bench slot 2
7 3-5 32g 67h [2ANI|6/7/7/7/2|1BLI|6] [] []
```

## Development

```bash
# run tests
pytest

# run linter
ruff check .

# type checking
mypy .

# format code
ruff format .
```

## License

MIT
