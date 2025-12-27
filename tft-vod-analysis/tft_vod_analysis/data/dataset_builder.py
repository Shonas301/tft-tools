"""synthetic dataset generation for TFT champion detection training."""

import json
import random
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, List, Tuple

import cv2
import numpy as np
from numpy.typing import NDArray

from tft_vod_analysis.data.champion_loader import ChampionData
from tft_vod_analysis.data.grid_config import GridConfig, GridPosition


@dataclass
class ChampionAsset:
    """represents a champion sprite/portrait asset."""

    shorthand: str
    image: NDArray[np.uint8]
    star_level: int = 1

    @property
    def height(self) -> int:
        """asset height in pixels."""
        return self.image.shape[0]

    @property
    def width(self) -> int:
        """asset width in pixels."""
        return self.image.shape[1]


@dataclass
class BoundingBox:
    """yolo-format bounding box."""

    class_id: int
    x_center: float  # normalized [0, 1]
    y_center: float  # normalized [0, 1]
    width: float  # normalized [0, 1]
    height: float  # normalized [0, 1]

    def to_yolo_string(self) -> str:
        """convert to YOLO annotation format string."""
        return f"{self.class_id} {self.x_center:.6f} {self.y_center:.6f} {self.width:.6f} {self.height:.6f}"

    def to_pixel_coords(
        self, img_width: int, img_height: int
    ) -> Tuple[int, int, int, int]:
        """convert to pixel coordinates (x, y, w, h)."""
        w = int(self.width * img_width)
        h = int(self.height * img_height)
        x = int(self.x_center * img_width - w / 2)
        y = int(self.y_center * img_height - h / 2)
        return (x, y, w, h)


@dataclass
class SyntheticFrame:
    """a synthetically generated training frame."""

    image: NDArray[np.uint8]
    annotations: List[BoundingBox]
    champions_placed: List[Tuple[GridPosition, str, int]]  # (pos, shorthand, star)


@dataclass
class DatasetConfig:
    """configuration for synthetic dataset generation."""

    output_dir: Path
    num_frames: int = 500
    min_champions: int = 3
    max_champions: int = 10
    include_bench: bool = True
    augment: bool = True
    # augmentation settings
    brightness_range: Tuple[float, float] = (0.8, 1.2)
    contrast_range: Tuple[float, float] = (0.8, 1.2)
    noise_level: float = 0.02
    # star level distribution
    star_weights: List[float] = field(default_factory=lambda: [0.6, 0.3, 0.1])


class SyntheticDatasetBuilder:
    """generates synthetic training data for champion detection.

    uses champion assets (sprites/portraits) to create training frames
    with automatically generated annotations.
    """

    def __init__(
        self,
        champion_data: ChampionData,
        grid_config: GridConfig,
        assets_dir: Path | str,
        background: NDArray[np.uint8] | None = None,
    ) -> None:
        """initialize the dataset builder.

        Args:
            champion_data: loaded champion data for class mappings
            grid_config: grid configuration for placement
            assets_dir: directory containing champion asset images
            background: optional background image to use
        """
        self.champion_data = champion_data
        self.grid_config = grid_config
        self.assets_dir = Path(assets_dir)
        self.background = background

        # build class mapping (shorthand -> class_id)
        self.class_mapping: Dict[str, int] = {}
        for idx, shorthand in enumerate(champion_data.get_all_shorthands()):
            self.class_mapping[shorthand] = idx

        # load champion assets
        self.assets: Dict[str, List[ChampionAsset]] = {}
        self._load_assets()

    def _load_assets(self) -> None:
        """load champion assets from assets directory.

        expects structure:
        assets_dir/
            ANI/
                1star.png
                2star.png
                3star.png
            BLI/
                1star.png
                ...
        or:
        assets_dir/
            ANI.png
            BLI.png
            ...
        """
        if not self.assets_dir.exists():
            return

        for shorthand in self.champion_data.get_all_shorthands():
            self.assets[shorthand] = []

            # check for directory structure (with star variants)
            champ_dir = self.assets_dir / shorthand
            if champ_dir.is_dir():
                for star in [1, 2, 3]:
                    for ext in ["png", "jpg", "jpeg"]:
                        star_path = champ_dir / f"{star}star.{ext}"
                        if star_path.exists():
                            img = cv2.imread(str(star_path), cv2.IMREAD_UNCHANGED)
                            if img is not None:
                                self.assets[shorthand].append(
                                    ChampionAsset(shorthand, img, star)
                                )

            # check for single file (no star variants)
            for ext in ["png", "jpg", "jpeg"]:
                single_path = self.assets_dir / f"{shorthand}.{ext}"
                if single_path.exists():
                    img = cv2.imread(str(single_path), cv2.IMREAD_UNCHANGED)
                    if img is not None:
                        # create for all star levels
                        for star in [1, 2, 3]:
                            self.assets[shorthand].append(
                                ChampionAsset(shorthand, img, star)
                            )
                    break

    def get_available_champions(self) -> List[str]:
        """get list of champions with loaded assets.

        Returns:
            list of champion shorthands that have assets loaded
        """
        return [s for s, assets in self.assets.items() if len(assets) > 0]

    def _create_background(
        self, width: int, height: int
    ) -> NDArray[np.uint8]:
        """create or resize background image.

        Args:
            width: target width
            height: target height

        Returns:
            background image
        """
        if self.background is not None:
            return cv2.resize(self.background, (width, height))

        # create a simple dark background
        bg = np.zeros((height, width, 3), dtype=np.uint8)
        bg[:] = (40, 40, 50)  # dark gray-blue, similar to TFT
        return bg

    def _place_asset(
        self,
        frame: NDArray[np.uint8],
        asset: ChampionAsset,
        position: Tuple[float, float],
        scale: float = 1.0,
    ) -> Tuple[int, int, int, int]:
        """place a champion asset on the frame.

        Args:
            frame: frame to modify (in-place)
            asset: champion asset to place
            position: (x, y) center position in pixels
            scale: scale factor for the asset

        Returns:
            bounding box as (x, y, width, height) in pixels
        """
        # resize asset
        new_width = int(asset.width * scale)
        new_height = int(asset.height * scale)
        resized = cv2.resize(asset.image, (new_width, new_height))

        # calculate placement coordinates
        cx, cy = position
        x = int(cx - new_width / 2)
        y = int(cy - new_height / 2)

        # clip to frame bounds
        frame_h, frame_w = frame.shape[:2]
        x1 = max(0, x)
        y1 = max(0, y)
        x2 = min(frame_w, x + new_width)
        y2 = min(frame_h, y + new_height)

        # calculate source region
        src_x1 = x1 - x
        src_y1 = y1 - y
        src_x2 = src_x1 + (x2 - x1)
        src_y2 = src_y1 + (y2 - y1)

        # handle alpha channel if present
        if resized.shape[2] == 4:
            alpha = resized[src_y1:src_y2, src_x1:src_x2, 3:4] / 255.0
            rgb = resized[src_y1:src_y2, src_x1:src_x2, :3]
            frame[y1:y2, x1:x2] = (
                frame[y1:y2, x1:x2] * (1 - alpha) + rgb * alpha
            ).astype(np.uint8)
        else:
            frame[y1:y2, x1:x2] = resized[src_y1:src_y2, src_x1:src_x2]

        return (x1, y1, x2 - x1, y2 - y1)

    def _apply_augmentation(
        self,
        frame: NDArray[np.uint8],
        config: DatasetConfig,
    ) -> NDArray[np.uint8]:
        """apply data augmentation to a frame.

        Args:
            frame: input frame
            config: augmentation configuration

        Returns:
            augmented frame
        """
        result = frame.astype(np.float32)

        # brightness adjustment
        brightness = random.uniform(*config.brightness_range)
        result *= brightness

        # contrast adjustment
        contrast = random.uniform(*config.contrast_range)
        mean = np.mean(result)
        result = (result - mean) * contrast + mean

        # add noise
        if config.noise_level > 0:
            noise = np.random.normal(0, config.noise_level * 255, result.shape)
            result += noise

        # clip and convert back
        result = np.clip(result, 0, 255).astype(np.uint8)
        return result

    def generate_frame(
        self,
        config: DatasetConfig,
        available_positions: List[GridPosition] | None = None,
    ) -> SyntheticFrame:
        """generate a single synthetic training frame.

        Args:
            config: generation configuration
            available_positions: optional list of positions to use

        Returns:
            SyntheticFrame with image and annotations
        """
        # determine frame dimensions from grid config
        width, height = self.grid_config.resolution
        frame = self._create_background(width, height)

        # get available positions
        if available_positions is None:
            available_positions = []
            for row in range(4):
                for col in range(7):
                    available_positions.append(GridPosition(row, col))
            if config.include_bench:
                for col in range(9):
                    available_positions.append(GridPosition(4, col))

        # determine number of champions to place
        num_champions = random.randint(config.min_champions, config.max_champions)
        num_champions = min(num_champions, len(available_positions))

        # select positions
        selected_positions = random.sample(available_positions, num_champions)

        # get available champions (those with assets)
        available_champs = self.get_available_champions()
        if not available_champs:
            # return empty frame if no assets
            return SyntheticFrame(
                image=frame, annotations=[], champions_placed=[]
            )

        annotations = []
        champions_placed = []

        for pos in selected_positions:
            # select random champion
            shorthand = random.choice(available_champs)

            # select star level based on weights
            star_level = random.choices([1, 2, 3], weights=config.star_weights)[0]

            # find matching asset
            matching_assets = [
                a for a in self.assets[shorthand] if a.star_level == star_level
            ]
            if not matching_assets:
                matching_assets = self.assets[shorthand]
            if not matching_assets:
                continue

            asset = random.choice(matching_assets)

            # get pixel position
            px, py = self.grid_config.get_pixel_position(pos)

            # add some position jitter for realism
            jitter = 5
            px += random.uniform(-jitter, jitter)
            py += random.uniform(-jitter, jitter)

            # scale based on star level (3-star are bigger)
            scale = 1.0 + (star_level - 1) * 0.15

            # place asset
            bbox_pixels = self._place_asset(frame, asset, (px, py), scale)

            # create normalized bounding box
            x, y, w, h = bbox_pixels
            bbox = BoundingBox(
                class_id=self.class_mapping[shorthand],
                x_center=(x + w / 2) / width,
                y_center=(y + h / 2) / height,
                width=w / width,
                height=h / height,
            )
            annotations.append(bbox)
            champions_placed.append((pos, shorthand, star_level))

        # apply augmentation if enabled
        if config.augment:
            frame = self._apply_augmentation(frame, config)

        return SyntheticFrame(
            image=frame,
            annotations=annotations,
            champions_placed=champions_placed,
        )

    def generate_dataset(
        self,
        config: DatasetConfig,
    ) -> None:
        """generate a complete synthetic dataset.

        creates:
        - images/ directory with frame images
        - labels/ directory with YOLO format annotations
        - classes.txt with class names
        - data.yaml for YOLO training

        Args:
            config: dataset generation configuration
        """
        output_dir = Path(config.output_dir)
        images_dir = output_dir / "images"
        labels_dir = output_dir / "labels"

        images_dir.mkdir(parents=True, exist_ok=True)
        labels_dir.mkdir(parents=True, exist_ok=True)

        # generate frames
        for i in range(config.num_frames):
            frame = self.generate_frame(config)

            # save image
            image_path = images_dir / f"frame_{i:06d}.png"
            cv2.imwrite(str(image_path), frame.image)

            # save annotations
            label_path = labels_dir / f"frame_{i:06d}.txt"
            with label_path.open("w") as f:
                for bbox in frame.annotations:
                    f.write(bbox.to_yolo_string() + "\n")

        # create classes.txt
        classes_path = output_dir / "classes.txt"
        shorthands = self.champion_data.get_all_shorthands()
        with classes_path.open("w") as f:
            for shorthand in shorthands:
                f.write(shorthand + "\n")

        # create data.yaml for YOLO
        data_yaml = {
            "path": str(output_dir.absolute()),
            "train": "images",
            "val": "images",  # would split in practice
            "names": {i: s for i, s in enumerate(shorthands)},
        }
        yaml_path = output_dir / "data.yaml"
        with yaml_path.open("w") as f:
            # simple yaml output without pyyaml dependency
            f.write(f"path: {data_yaml['path']}\n")
            f.write(f"train: {data_yaml['train']}\n")
            f.write(f"val: {data_yaml['val']}\n")
            f.write("names:\n")
            for class_id, name in data_yaml["names"].items():
                f.write(f"  {class_id}: {name}\n")

        # create metadata
        metadata = {
            "num_frames": config.num_frames,
            "num_classes": len(shorthands),
            "resolution": list(self.grid_config.resolution),
            "augmented": config.augment,
        }
        metadata_path = output_dir / "metadata.json"
        with metadata_path.open("w") as f:
            json.dump(metadata, f, indent=2)


def create_cell_classification_dataset(
    frames_dir: Path | str,
    output_dir: Path | str,
    grid_config: GridConfig,
    champion_data: ChampionData,
) -> None:
    """create a cell-based classification dataset from annotated frames.

    crops each grid cell from frames and organizes by class for
    image classification training.

    Args:
        frames_dir: directory containing source frames
        output_dir: directory to save cropped cells
        grid_config: grid configuration for cell positions
        champion_data: champion data for class names

    Creates structure:
        output_dir/
            train/
                empty/
                ANI/
                BLI/
                ...
            val/
                ...
    """
    frames_dir = Path(frames_dir)
    output_dir = Path(output_dir)

    # create class directories
    classes = ["empty"] + champion_data.get_all_shorthands()
    for split in ["train", "val"]:
        for class_name in classes:
            (output_dir / split / class_name).mkdir(parents=True, exist_ok=True)

    # this is a stub - actual implementation would:
    # 1. load frames and their annotations
    # 2. for each grid position, crop the cell
    # 3. determine class from annotation (or "empty")
    # 4. save to appropriate directory
    pass
