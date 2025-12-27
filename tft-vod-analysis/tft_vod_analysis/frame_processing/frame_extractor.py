"""video frame extraction utilities for TFT gameplay analysis."""

from dataclasses import dataclass
from pathlib import Path
from typing import Generator, Iterator

import cv2
import numpy as np
from numpy.typing import NDArray


@dataclass
class FrameInfo:
    """metadata about an extracted frame."""

    frame_number: int
    timestamp_ms: float
    frame: NDArray[np.uint8]


class VideoFrameExtractor:
    """extracts frames from video files for analysis.

    supports various extraction modes: all frames, fixed interval,
    or keyframe detection.
    """

    def __init__(self, video_path: Path | str) -> None:
        """initialize the frame extractor.

        Args:
            video_path: path to the video file

        Raises:
            FileNotFoundError: if video file doesn't exist
            ValueError: if video cannot be opened
        """
        self.video_path = Path(video_path)
        if not self.video_path.exists():
            raise FileNotFoundError(f"video not found: {self.video_path}")

        self.cap = cv2.VideoCapture(str(self.video_path))
        if not self.cap.isOpened():
            raise ValueError(f"failed to open video: {self.video_path}")

        # cache video properties
        self._fps = self.cap.get(cv2.CAP_PROP_FPS)
        self._frame_count = int(self.cap.get(cv2.CAP_PROP_FRAME_COUNT))
        self._width = int(self.cap.get(cv2.CAP_PROP_FRAME_WIDTH))
        self._height = int(self.cap.get(cv2.CAP_PROP_FRAME_HEIGHT))
        self._duration_ms = (self._frame_count / self._fps) * 1000 if self._fps > 0 else 0

    @property
    def fps(self) -> float:
        """video frames per second."""
        return self._fps

    @property
    def frame_count(self) -> int:
        """total number of frames in video."""
        return self._frame_count

    @property
    def resolution(self) -> tuple[int, int]:
        """video resolution as (width, height)."""
        return (self._width, self._height)

    @property
    def duration_ms(self) -> float:
        """video duration in milliseconds."""
        return self._duration_ms

    def get_frame(self, frame_number: int) -> FrameInfo:
        """get a specific frame by frame number.

        Args:
            frame_number: zero-indexed frame number

        Returns:
            FrameInfo with frame data and metadata

        Raises:
            ValueError: if frame number is out of range or frame cannot be read
        """
        if frame_number < 0 or frame_number >= self._frame_count:
            raise ValueError(
                f"frame {frame_number} out of range [0, {self._frame_count})"
            )

        self.cap.set(cv2.CAP_PROP_POS_FRAMES, frame_number)
        ret, frame = self.cap.read()

        if not ret or frame is None:
            raise ValueError(f"failed to read frame {frame_number}")

        timestamp_ms = (frame_number / self._fps) * 1000 if self._fps > 0 else 0

        return FrameInfo(
            frame_number=frame_number,
            timestamp_ms=timestamp_ms,
            frame=frame,
        )

    def get_frame_at_time(self, timestamp_ms: float) -> FrameInfo:
        """get frame at a specific timestamp.

        Args:
            timestamp_ms: timestamp in milliseconds

        Returns:
            FrameInfo with frame data and metadata

        Raises:
            ValueError: if timestamp is out of range
        """
        if timestamp_ms < 0 or timestamp_ms > self._duration_ms:
            raise ValueError(
                f"timestamp {timestamp_ms}ms out of range [0, {self._duration_ms}]"
            )

        frame_number = int((timestamp_ms / 1000) * self._fps)
        return self.get_frame(frame_number)

    def extract_frames_interval(
        self,
        interval_ms: float = 1000,
        start_ms: float = 0,
        end_ms: float | None = None,
    ) -> Generator[FrameInfo, None, None]:
        """extract frames at fixed time intervals.

        Args:
            interval_ms: time between frames in milliseconds
            start_ms: start timestamp in milliseconds
            end_ms: end timestamp in milliseconds (None = end of video)

        Yields:
            FrameInfo for each extracted frame
        """
        if end_ms is None:
            end_ms = self._duration_ms

        current_ms = start_ms
        while current_ms <= end_ms:
            try:
                yield self.get_frame_at_time(current_ms)
            except ValueError:
                break
            current_ms += interval_ms

    def extract_frames_fps(
        self,
        target_fps: float = 1.0,
        start_ms: float = 0,
        end_ms: float | None = None,
    ) -> Generator[FrameInfo, None, None]:
        """extract frames at a target frame rate.

        Args:
            target_fps: target frames per second (e.g., 1.0 = 1 frame/sec)
            start_ms: start timestamp in milliseconds
            end_ms: end timestamp in milliseconds (None = end of video)

        Yields:
            FrameInfo for each extracted frame
        """
        interval_ms = 1000.0 / target_fps
        yield from self.extract_frames_interval(interval_ms, start_ms, end_ms)

    def extract_all_frames(self) -> Generator[FrameInfo, None, None]:
        """extract all frames from the video.

        warning: this can be memory intensive for long videos.
        consider using extract_frames_interval or extract_frames_fps instead.

        Yields:
            FrameInfo for each frame in the video
        """
        self.cap.set(cv2.CAP_PROP_POS_FRAMES, 0)

        frame_number = 0
        while True:
            ret, frame = self.cap.read()
            if not ret or frame is None:
                break

            timestamp_ms = (frame_number / self._fps) * 1000 if self._fps > 0 else 0

            yield FrameInfo(
                frame_number=frame_number,
                timestamp_ms=timestamp_ms,
                frame=frame,
            )
            frame_number += 1

    def detect_scene_changes(
        self,
        threshold: float = 30.0,
        min_interval_ms: float = 1000,
    ) -> Generator[FrameInfo, None, None]:
        """detect frames where significant scene changes occur.

        useful for finding planning phase transitions in TFT.
        uses frame differencing to detect changes.

        Args:
            threshold: pixel difference threshold (higher = less sensitive)
            min_interval_ms: minimum time between detected changes

        Yields:
            FrameInfo for frames with detected scene changes
        """
        self.cap.set(cv2.CAP_PROP_POS_FRAMES, 0)

        prev_frame = None
        prev_timestamp_ms = -min_interval_ms  # allow first frame
        frame_number = 0

        while True:
            ret, frame = self.cap.read()
            if not ret or frame is None:
                break

            timestamp_ms = (frame_number / self._fps) * 1000 if self._fps > 0 else 0

            if prev_frame is not None:
                # calculate frame difference
                gray_curr = cv2.cvtColor(frame, cv2.COLOR_BGR2GRAY)
                gray_prev = cv2.cvtColor(prev_frame, cv2.COLOR_BGR2GRAY)
                diff = cv2.absdiff(gray_curr, gray_prev)
                mean_diff = np.mean(diff)

                # check if change exceeds threshold and min interval
                if (
                    mean_diff > threshold
                    and (timestamp_ms - prev_timestamp_ms) >= min_interval_ms
                ):
                    yield FrameInfo(
                        frame_number=frame_number,
                        timestamp_ms=timestamp_ms,
                        frame=frame.copy(),
                    )
                    prev_timestamp_ms = timestamp_ms

            prev_frame = frame.copy()
            frame_number += 1

    def close(self) -> None:
        """release video resources."""
        if self.cap is not None:
            self.cap.release()

    def __enter__(self) -> "VideoFrameExtractor":
        """context manager entry."""
        return self

    def __exit__(self, exc_type: type, exc_val: Exception, exc_tb: object) -> None:
        """context manager exit."""
        self.close()

    def __del__(self) -> None:
        """destructor to ensure resources are released."""
        self.close()


def extract_and_save_frames(
    video_path: Path | str,
    output_dir: Path | str,
    interval_ms: float = 1000,
    prefix: str = "frame",
    image_format: str = "png",
) -> list[Path]:
    """convenience function to extract frames and save to disk.

    Args:
        video_path: path to input video
        output_dir: directory to save extracted frames
        interval_ms: time between frames in milliseconds
        prefix: filename prefix for saved frames
        image_format: image format (png, jpg, etc.)

    Returns:
        list of paths to saved frame files

    Raises:
        FileNotFoundError: if video doesn't exist
        ValueError: if video cannot be opened
    """
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    saved_paths = []

    with VideoFrameExtractor(video_path) as extractor:
        for frame_info in extractor.extract_frames_interval(interval_ms):
            filename = f"{prefix}_{frame_info.frame_number:06d}.{image_format}"
            output_path = output_dir / filename

            cv2.imwrite(str(output_path), frame_info.frame)
            saved_paths.append(output_path)

    return saved_paths


def extract_keyframes_for_annotation(
    video_path: Path | str,
    output_dir: Path | str,
    frames_per_minute: float = 2.0,
    scene_detection: bool = True,
    scene_threshold: float = 25.0,
) -> list[Path]:
    """extract keyframes suitable for annotation.

    combines time-based extraction with optional scene change detection
    to capture diverse frames for training data.

    Args:
        video_path: path to input video
        output_dir: directory to save extracted frames
        frames_per_minute: baseline extraction rate
        scene_detection: whether to also extract scene change frames
        scene_threshold: sensitivity for scene detection

    Returns:
        list of paths to saved keyframes
    """
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    saved_paths = []
    seen_frames = set()

    with VideoFrameExtractor(video_path) as extractor:
        # extract at fixed interval
        interval_ms = 60000 / frames_per_minute  # convert to ms
        for frame_info in extractor.extract_frames_interval(interval_ms):
            if frame_info.frame_number not in seen_frames:
                filename = f"keyframe_{frame_info.frame_number:06d}.png"
                output_path = output_dir / filename
                cv2.imwrite(str(output_path), frame_info.frame)
                saved_paths.append(output_path)
                seen_frames.add(frame_info.frame_number)

        # optionally add scene change frames
        if scene_detection:
            for frame_info in extractor.detect_scene_changes(
                threshold=scene_threshold
            ):
                if frame_info.frame_number not in seen_frames:
                    filename = f"scene_{frame_info.frame_number:06d}.png"
                    output_path = output_dir / filename
                    cv2.imwrite(str(output_path), frame_info.frame)
                    saved_paths.append(output_path)
                    seen_frames.add(frame_info.frame_number)

    return sorted(saved_paths)
