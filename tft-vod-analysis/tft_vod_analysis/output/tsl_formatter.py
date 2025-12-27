"""TSL (TFT Session Language) formatter for detection output.

converts champion detections to TSL FEN notation following the schema
defined in /Users/jasonshipp/code/tft-tools/tsl/SCHEMA.md
"""

from dataclasses import dataclass
from typing import Dict, List, Tuple

from tft_vod_analysis.data.grid_config import GridPosition
from tft_vod_analysis.detection.grid_snapper import Detection


@dataclass
class GameState:
    """represents the complete game state for TSL output."""

    level: int = 1
    stage: int = 1
    round: int = 1
    gold: int = 0
    health: int = 100
    board: Dict[GridPosition, Tuple[str, int]] = None  # pos -> (shorthand, star)
    bench: Dict[int, Tuple[str, int]] = None  # col -> (shorthand, star)
    items: List[str] = None
    augments: List[str] = None

    def __post_init__(self) -> None:
        """initialize default empty collections."""
        if self.board is None:
            self.board = {}
        if self.bench is None:
            self.bench = {}
        if self.items is None:
            self.items = []
        if self.augments is None:
            self.augments = []


class TSLFormatter:
    """formats game state to TSL FEN notation.

    TSL format:
    <level> <stage>-<round> <gold>g <health>h [<board>] [<items>] [<augments>]

    board FEN format:
    [row0/row1/row2/row3/bench]
    - champion cell: <stars><SHORTHAND> (e.g., 2ANI for 2-star Anivia)
    - empty cells: number indicating consecutive empty cells
    """

    def format_champion(self, shorthand: str, star_level: int) -> str:
        """format a single champion entry.

        Args:
            shorthand: champion shorthand code (e.g., "ANI")
            star_level: star level (1, 2, or 3)

        Returns:
            formatted champion string (e.g., "2ANI")
        """
        return f"{star_level}{shorthand}"

    def format_row(
        self,
        champions: Dict[int, Tuple[str, int]],
        row_length: int,
    ) -> str:
        """format a single row (board row or bench) in FEN notation.

        Args:
            champions: dict mapping column to (shorthand, star_level)
            row_length: number of cells in the row

        Returns:
            FEN-formatted row string
        """
        result = []
        empty_count = 0

        for col in range(row_length):
            if col in champions:
                # flush empty count if any
                if empty_count > 0:
                    result.append(str(empty_count))
                    empty_count = 0

                shorthand, star_level = champions[col]
                result.append(self.format_champion(shorthand, star_level))
            else:
                empty_count += 1

        # flush remaining empty count
        if empty_count > 0:
            result.append(str(empty_count))

        return "|".join(result) if result else str(row_length)

    def format_board_fen(self, state: GameState) -> str:
        """format board and bench as FEN notation.

        Args:
            state: game state with board and bench

        Returns:
            FEN string like "[2ANI|6/7/7/7/9]"
        """
        rows = []

        # format board rows (0-3)
        for row in range(4):
            row_champions = {
                pos.col: champ
                for pos, champ in state.board.items()
                if pos.row == row
            }
            rows.append(self.format_row(row_champions, 7))

        # format bench (row 4)
        rows.append(self.format_row(state.bench, 9))

        return f"[{'/'.join(rows)}]"

    def format_items(self, items: List[str]) -> str:
        """format items list.

        Args:
            items: list of item shorthands

        Returns:
            formatted items string like "[i=IE,i=RB]" or "[]" if empty
        """
        if not items:
            return "[]"
        return f"[{','.join(f'i={item}' for item in items)}]"

    def format_augments(self, augments: List[str]) -> str:
        """format augments list.

        Args:
            augments: list of augment shorthands

        Returns:
            formatted augments string like "[a=AUG1,a=AUG2]" or "[]" if empty
        """
        if not augments:
            return "[]"
        return f"[{','.join(f'a={aug}' for aug in augments)}]"

    def format_full(self, state: GameState) -> str:
        """format complete game state to TSL notation.

        Args:
            state: complete game state

        Returns:
            full TSL string like:
            "7 3-5 32g 67h [2ANI|6/7/7/7/9] [] []"
        """
        parts = [
            str(state.level),
            f"{state.stage}-{state.round}",
            f"{state.gold}g",
            f"{state.health}h",
            self.format_board_fen(state),
            self.format_items(state.items),
            self.format_augments(state.augments),
        ]
        return " ".join(parts)

    def format_board_only(self, state: GameState) -> str:
        """format only the board FEN (without game metadata).

        useful when you only have champion detections without UI parsing.

        Args:
            state: game state (only board/bench used)

        Returns:
            board FEN string like "[2ANI|6/7/7/7/9]"
        """
        return self.format_board_fen(state)


def detections_to_game_state(
    board_detections: Dict[GridPosition, Detection],
    level: int = 1,
    stage: int = 1,
    round_num: int = 1,
    gold: int = 0,
    health: int = 100,
) -> GameState:
    """convert detection results to GameState.

    Args:
        board_detections: dict mapping grid positions to detections
        level: player level
        stage: current stage
        round_num: current round
        gold: gold amount
        health: health amount

    Returns:
        GameState ready for TSL formatting
    """
    board = {}
    bench = {}

    for pos, detection in board_detections.items():
        champ_data = (detection.champion_shorthand, detection.star_level)
        if pos.is_board():
            board[pos] = champ_data
        else:
            bench[pos.col] = champ_data

    return GameState(
        level=level,
        stage=stage,
        round=round_num,
        gold=gold,
        health=health,
        board=board,
        bench=bench,
    )


def format_detections_to_tsl(
    board_detections: Dict[GridPosition, Detection],
    level: int = 1,
    stage: int = 1,
    round_num: int = 1,
    gold: int = 0,
    health: int = 100,
    include_metadata: bool = True,
) -> str:
    """convenience function to format detections directly to TSL.

    Args:
        board_detections: dict mapping grid positions to detections
        level: player level
        stage: current stage
        round_num: current round
        gold: gold amount
        health: health amount
        include_metadata: if True, include full TSL; if False, board only

    Returns:
        TSL-formatted string
    """
    state = detections_to_game_state(
        board_detections, level, stage, round_num, gold, health
    )

    formatter = TSLFormatter()
    if include_metadata:
        return formatter.format_full(state)
    return formatter.format_board_only(state)
