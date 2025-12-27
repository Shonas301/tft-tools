"""champion data loader and mapping utilities."""

from pathlib import Path
from typing import Dict

import pandas as pd


class ChampionData:
    """loads and provides access to TFT champion data from CSV files.

    this class handles loading champion information from the set_16_champions.csv
    file and provides convenient mappings between champion names and shorthand codes.
    """

    def __init__(self, csv_path: Path | str) -> None:
        """initialize the champion data loader.

        Args:
            csv_path: path to the champion CSV file

        Raises:
            FileNotFoundError: if the CSV file doesn't exist
            ValueError: if the CSV file is malformed or missing required columns
        """
        self.csv_path = Path(csv_path)
        if not self.csv_path.exists():
            raise FileNotFoundError(f"champion CSV not found: {self.csv_path}")

        self.df = pd.read_csv(self.csv_path)

        # validate required columns
        required_columns = {"name", "shorthand", "cost", "traits"}
        missing_columns = required_columns - set(self.df.columns)
        if missing_columns:
            raise ValueError(
                f"CSV missing required columns: {missing_columns}"
            )

        # create mappings
        self._name_to_shorthand: Dict[str, str] = {}
        self._shorthand_to_name: Dict[str, str] = {}
        self._shorthand_to_cost: Dict[str, int] = {}
        self._build_mappings()

    def _build_mappings(self) -> None:
        """build internal mappings from the dataframe."""
        for _, row in self.df.iterrows():
            name = row["name"]
            shorthand = row["shorthand"]
            cost = row["cost"]

            self._name_to_shorthand[name] = shorthand
            self._shorthand_to_name[shorthand] = name
            self._shorthand_to_cost[shorthand] = cost

    def get_shorthand(self, champion_name: str) -> str:
        """get shorthand code for a champion name.

        Args:
            champion_name: full champion name (e.g., "Anivia")

        Returns:
            shorthand code (e.g., "ANI")

        Raises:
            KeyError: if champion name not found
        """
        if champion_name not in self._name_to_shorthand:
            raise KeyError(f"unknown champion: {champion_name}")
        return self._name_to_shorthand[champion_name]

    def get_name(self, shorthand: str) -> str:
        """get champion name from shorthand code.

        Args:
            shorthand: shorthand code (e.g., "ANI")

        Returns:
            full champion name (e.g., "Anivia")

        Raises:
            KeyError: if shorthand not found
        """
        if shorthand not in self._shorthand_to_name:
            raise KeyError(f"unknown shorthand: {shorthand}")
        return self._shorthand_to_name[shorthand]

    def get_cost(self, shorthand: str) -> int:
        """get champion cost from shorthand code.

        Args:
            shorthand: shorthand code (e.g., "ANI")

        Returns:
            champion cost (1-5)

        Raises:
            KeyError: if shorthand not found
        """
        if shorthand not in self._shorthand_to_cost:
            raise KeyError(f"unknown shorthand: {shorthand}")
        return self._shorthand_to_cost[shorthand]

    def get_all_shorthands(self) -> list[str]:
        """get list of all champion shorthand codes.

        Returns:
            list of all shorthand codes
        """
        return list(self._shorthand_to_name.keys())

    def get_all_names(self) -> list[str]:
        """get list of all champion names.

        Returns:
            list of all champion names
        """
        return list(self._name_to_shorthand.keys())

    def __len__(self) -> int:
        """return number of champions loaded.

        Returns:
            count of champions
        """
        return len(self._name_to_shorthand)


# default path to champion CSV in tft-tools repo
DEFAULT_CHAMPION_CSV = Path(__file__).parents[4] / "tft-data" / "set_16_champions.csv"


def load_champions(csv_path: Path | str | None = None) -> ChampionData:
    """load champion data from CSV file.

    convenience function to load champion data. if no path is provided,
    uses the default location in the tft-tools repo.

    Args:
        csv_path: optional path to champion CSV file. if None, uses default.

    Returns:
        loaded ChampionData instance

    Raises:
        FileNotFoundError: if the CSV file doesn't exist
        ValueError: if the CSV file is malformed
    """
    if csv_path is None:
        csv_path = DEFAULT_CHAMPION_CSV
    return ChampionData(csv_path)
