"""unit tests for champion data loader."""

from pathlib import Path
from tempfile import NamedTemporaryFile

import pytest

from tft_vod_analysis.data.champion_loader import ChampionData, load_champions


@pytest.fixture
def sample_csv() -> Path:
    """create a temporary CSV file with sample champion data."""
    csv_content = """name,url,cost,metaCost,traits,shorthand
Anivia,https://tftactics.gg/champions/anivia/,1,1,Freljord; Invoker,ANI
Blitzcrank,https://tftactics.gg/champions/blitzcrank/,1,1,Zaun; Juggernaut,BLI
Ashe,https://tftactics.gg/champions/ashe/,2,2,Freljord; Quickstriker,ASH
"""
    with NamedTemporaryFile(mode="w", suffix=".csv", delete=False) as f:
        f.write(csv_content)
        temp_path = Path(f.name)
    return temp_path


@pytest.fixture
def champion_data(sample_csv: Path) -> ChampionData:
    """create a ChampionData instance with sample data."""
    return ChampionData(sample_csv)


def test_load_csv_success(sample_csv: Path) -> None:
    """test successful CSV loading."""
    data = ChampionData(sample_csv)
    assert len(data) == 3
    assert "ANI" in data.get_all_shorthands()
    assert "Anivia" in data.get_all_names()


def test_load_csv_file_not_found() -> None:
    """test loading non-existent CSV raises FileNotFoundError."""
    with pytest.raises(FileNotFoundError):
        ChampionData("/nonexistent/path/champions.csv")


def test_load_csv_missing_columns() -> None:
    """test loading CSV with missing required columns raises ValueError."""
    csv_content = """name,shorthand
Anivia,ANI
"""
    with NamedTemporaryFile(mode="w", suffix=".csv", delete=False) as f:
        f.write(csv_content)
        temp_path = Path(f.name)

    with pytest.raises(ValueError, match="missing required columns"):
        ChampionData(temp_path)

    # cleanup
    temp_path.unlink()


def test_get_shorthand_success(champion_data: ChampionData) -> None:
    """test getting shorthand from champion name."""
    assert champion_data.get_shorthand("Anivia") == "ANI"
    assert champion_data.get_shorthand("Blitzcrank") == "BLI"
    assert champion_data.get_shorthand("Ashe") == "ASH"


def test_get_shorthand_unknown_champion(champion_data: ChampionData) -> None:
    """test getting shorthand for unknown champion raises KeyError."""
    with pytest.raises(KeyError, match="unknown champion"):
        champion_data.get_shorthand("UnknownChampion")


def test_get_name_success(champion_data: ChampionData) -> None:
    """test getting champion name from shorthand."""
    assert champion_data.get_name("ANI") == "Anivia"
    assert champion_data.get_name("BLI") == "Blitzcrank"
    assert champion_data.get_name("ASH") == "Ashe"


def test_get_name_unknown_shorthand(champion_data: ChampionData) -> None:
    """test getting name for unknown shorthand raises KeyError."""
    with pytest.raises(KeyError, match="unknown shorthand"):
        champion_data.get_name("XYZ")


def test_get_cost_success(champion_data: ChampionData) -> None:
    """test getting champion cost from shorthand."""
    assert champion_data.get_cost("ANI") == 1
    assert champion_data.get_cost("BLI") == 1
    assert champion_data.get_cost("ASH") == 2


def test_get_cost_unknown_shorthand(champion_data: ChampionData) -> None:
    """test getting cost for unknown shorthand raises KeyError."""
    with pytest.raises(KeyError, match="unknown shorthand"):
        champion_data.get_cost("XYZ")


def test_get_all_shorthands(champion_data: ChampionData) -> None:
    """test getting all shorthand codes."""
    shorthands = champion_data.get_all_shorthands()
    assert len(shorthands) == 3
    assert set(shorthands) == {"ANI", "BLI", "ASH"}


def test_get_all_names(champion_data: ChampionData) -> None:
    """test getting all champion names."""
    names = champion_data.get_all_names()
    assert len(names) == 3
    assert set(names) == {"Anivia", "Blitzcrank", "Ashe"}


def test_len(champion_data: ChampionData) -> None:
    """test __len__ returns correct count."""
    assert len(champion_data) == 3


def test_load_champions_with_path(sample_csv: Path) -> None:
    """test load_champions convenience function with explicit path."""
    data = load_champions(sample_csv)
    assert len(data) == 3
    assert data.get_shorthand("Anivia") == "ANI"


def test_load_champions_default_path() -> None:
    """test load_champions with default path (if it exists)."""
    # this test will only pass if the default CSV exists
    # in the expected location
    default_path = Path(__file__).parents[3] / "tft-data" / "set_16_champions.csv"

    if not default_path.exists():
        pytest.skip(f"default champion CSV not found at {default_path}")

    data = load_champions()
    assert len(data) > 0
    # verify it has expected champions from set 16
    assert "ANI" in data.get_all_shorthands()
