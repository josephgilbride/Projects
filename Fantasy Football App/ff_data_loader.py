import nflreadpy as nfl
import pandas as pd
import streamlit as st
import os

SKILL_POSITIONS = {"QB", "RB", "WR", "TE", "FB"}
# FB counts as RB for fantasy purposes
_POSITION_REMAP = {"FB": "RB"}
MIN_GAMES_DEFAULT = 4

# Path to the manually-maintained 2025 PFR export
EXCEL_2025_PATH = "Fantasy Football/2025_stats.xlsx"

_SCORING_COLUMN = {
    "PPR":      "fantasy_points_ppr",
    "Half-PPR": None,   # computed below
    "Standard": "fantasy_points",
}

_REPLACEMENT_RANKS = {"QB": 12, "RB": 24, "WR": 30, "TE": 12}


@st.cache_data(ttl=3600 * 24, show_spinner=False)
def fetch_weekly_stats(year: int = 2024, scoring: str = "PPR") -> pd.DataFrame:
    """Download weekly player stats for the given season and scoring format."""
    warnings_filter = __import__("warnings")
    with warnings_filter.catch_warnings():
        warnings_filter.simplefilter("ignore")
        raw =  nfl.load_player_stats(seasons=[year]).to_pandas()

    raw = raw[raw["position"].isin(SKILL_POSITIONS)].copy()
    raw["position"] = raw["position"].replace(_POSITION_REMAP)
    raw = raw[raw["season_type"] == "REG"].copy()

    if scoring == "PPR":
        raw["fp"] = pd.to_numeric(raw["fantasy_points_ppr"], errors="coerce").fillna(0)
    elif scoring == "Half-PPR":
        std = pd.to_numeric(raw["fantasy_points"], errors="coerce").fillna(0)
        ppr = pd.to_numeric(raw["fantasy_points_ppr"], errors="coerce").fillna(0)
        raw["fp"] = (std + ppr) / 2
    else:
        raw["fp"] = pd.to_numeric(raw["fantasy_points"], errors="coerce").fillna(0)

    keep = [
        "player_display_name", "position", "team",
        "season", "week", "fp",
        "passing_yards", "passing_tds", "interceptions",
        "rushing_yards", "rushing_tds", "carries",
        "receptions", "targets", "receiving_yards", "receiving_tds",
    ]
    return raw[[c for c in keep if c in raw.columns]].copy()


def build_player_summary(
    weekly: pd.DataFrame,
    min_games: int = MIN_GAMES_DEFAULT,
) -> pd.DataFrame:
    """Aggregate weekly rows into per-player season summary statistics."""
    grp = weekly.groupby(["player_display_name", "position"])

    summary = grp.agg(
        games=("week", "count"),
        total=("fp", "sum"),
        avg=("fp", "mean"),
        std=("fp", "std"),
        floor=("fp", lambda x: x.quantile(0.10)),
        ceiling=("fp", lambda x: x.quantile(0.90)),
        team=("team", "last"),
    ).reset_index()

    summary["std"]     = summary["std"].fillna(0).round(2)
    summary["floor"]   = summary["floor"].round(2)
    summary["ceiling"] = summary["ceiling"].round(2)
    summary["avg"]     = summary["avg"].round(2)
    summary["total"]   = summary["total"].round(1)
    # Coefficient of variation: lower = more consistent
    summary["cv"] = (summary["std"] / summary["avg"].clip(lower=0.1)).round(3)

    summary = summary[summary["games"] >= min_games].copy()
    return summary.sort_values("avg", ascending=False).reset_index(drop=True)


def load_pfr_excel(path: str = EXCEL_2025_PATH, scoring: str = "PPR", min_games: int = MIN_GAMES_DEFAULT) -> pd.DataFrame:
    """
    Load a Pro Football Reference fantasy export (two-row header) and return
    a summary DataFrame in the same shape as build_player_summary().
    Std/floor/ceiling are NaN because PFR only provides season totals.
    """
    raw = pd.read_excel(path, header=None)

    # Row 1 (index 1) holds the real column names; row 0 is the category grouping
    cols = raw.iloc[1].tolist()
    # Disambiguate duplicate names by prepending the category from row 0
    categories = raw.iloc[0].tolist()
    seen: dict[str, int] = {}
    final_cols: list[str] = []
    for cat, col in zip(categories, cols):
        key = str(col)
        prefix = "" if pd.isna(cat) else f"{cat}_"
        full = f"{prefix}{key}"
        count = seen.get(full, 0)
        seen[full] = count + 1
        final_cols.append(full if count == 0 else f"{full}.{count}")

    df = raw.iloc[2:].copy()
    df.columns = final_cols
    df = df.reset_index(drop=True)

    # Drop PFR's repeated-header separator rows
    df = df[df["Rk"] != "Rk"].dropna(subset=["Player"]).copy()

    # Strip Pro Bowl (*) and All-Pro (+) markers from player names
    df["player_display_name"] = df["Player"].astype(str).str.replace(r"[*+]", "", regex=True).str.strip()
    df["position"] = df["FantPos"].astype(str).str.strip().replace(_POSITION_REMAP)
    df["team"] = df["Tm"].astype(str).str.strip()

    df["games"] = pd.to_numeric(df["Games_G"], errors="coerce")

    # PFR column names after disambiguation:
    #   FantPt = standard, PPR = ppr
    #   Half-PPR isn't stored directly — average the two
    if scoring == "PPR":
        df["total"] = pd.to_numeric(df["Fantasy_PPR"], errors="coerce")
    elif scoring == "Half-PPR":
        std_pts = pd.to_numeric(df["Fantasy_FantPt"], errors="coerce")
        ppr_pts = pd.to_numeric(df["Fantasy_PPR"],    errors="coerce")
        df["total"] = (std_pts + ppr_pts) / 2
    else:
        df["total"] = pd.to_numeric(df["Fantasy_FantPt"], errors="coerce")

    df = df.dropna(subset=["total", "games"])
    df["avg"] = (df["total"] / df["games"]).round(2)
    df["total"] = df["total"].round(1)

    # No weekly data available — leave variability metrics as NaN
    df["std"]     = float("nan")
    df["floor"]   = float("nan")
    df["ceiling"] = float("nan")
    df["cv"]      = float("nan")

    df = df[df["position"].isin(SKILL_POSITIONS - {"FB"} | {"RB"})]
    df = df[df["games"] >= min_games]

    cols_out = ["player_display_name", "position", "team", "games", "total", "avg", "std", "floor", "ceiling", "cv"]
    return df[cols_out].sort_values("avg", ascending=False).reset_index(drop=True)


def build_weighted_summary(
    scoring: str = "PPR",
    min_games: int = MIN_GAMES_DEFAULT,
    w_recent: float = 0.8,
    w_prior: float = 0.2,
) -> tuple[pd.DataFrame, str]:
    """
    Combine the two most recent available seasons into a weighted summary.
    Returns (summary_df, label) where label describes the years used.

    Weighted avg is normalized per player: players who only appear in one
    season get that season's avg unscaled (not penalized for the missing year).

    Variability metrics (std/floor/ceiling) come from whichever season has
    weekly data; NaN when neither does.
    """
    has_2025 = os.path.exists(EXCEL_2025_PATH)

    if has_2025:
        s_recent = load_pfr_excel(scoring=scoring, min_games=1)
        weekly_prior = fetch_weekly_stats(year=2024, scoring=scoring)
        s_prior = build_player_summary(weekly_prior, min_games=1)
        recent_year, prior_year = 2025, 2024
    else:
        weekly_recent = fetch_weekly_stats(year=2024, scoring=scoring)
        s_recent = build_player_summary(weekly_recent, min_games=1)
        weekly_prior = fetch_weekly_stats(year=2023, scoring=scoring)
        s_prior = build_player_summary(weekly_prior, min_games=1)
        recent_year, prior_year = 2024, 2023

    label = f"{recent_year}×{w_recent} + {prior_year}×{w_prior}"

    r = s_recent[["player_display_name", "position", "team", "games",
                   "avg", "std", "floor", "ceiling", "cv"]].copy()
    r.columns = ["player_display_name", "position", "team_r", "games_r",
                  "avg_r", "std_r", "floor_r", "ceiling_r", "cv_r"]

    p = s_prior[["player_display_name", "position", "team", "games",
                  "avg", "std", "floor", "ceiling", "cv"]].copy()
    p.columns = ["player_display_name", "position", "team_p", "games_p",
                  "avg_p", "std_p", "floor_p", "ceiling_p", "cv_p"]

    merged = r.merge(p, on=["player_display_name", "position"], how="outer")

    def _wavg(row):
        a, b = row["avg_r"], row["avg_p"]
        if pd.notna(a) and pd.notna(b):
            return round(w_recent * a + w_prior * b, 2)
        return round(a, 2) if pd.notna(a) else round(b, 2)

    merged["avg"]     = merged.apply(_wavg, axis=1)
    merged["team"]    = merged["team_r"].combine_first(merged["team_p"])
    merged["games"]   = merged["games_r"].combine_first(merged["games_p"])
    # Prefer whichever season has weekly-derived variability metrics
    merged["std"]     = merged["std_r"].combine_first(merged["std_p"])
    merged["floor"]   = merged["floor_r"].combine_first(merged["floor_p"])
    merged["ceiling"] = merged["ceiling_r"].combine_first(merged["ceiling_p"])
    merged["cv"]      = merged["cv_r"].combine_first(merged["cv_p"])
    merged["total"]   = float("nan")  # not meaningful across seasons

    merged = merged[merged["games"] >= min_games].copy()
    out = merged[["player_display_name", "position", "team", "games",
                   "total", "avg", "std", "floor", "ceiling", "cv"]]
    return out.sort_values("avg", ascending=False).reset_index(drop=True), label


def compute_vor(
    summary: pd.DataFrame,
    replacement_ranks: dict | None = None,
) -> pd.DataFrame:
    """Append VOR (Value Over Replacement) column to the summary DataFrame."""
    if replacement_ranks is None:
        replacement_ranks = _REPLACEMENT_RANKS

    df = summary.copy()
    df["VOR"] = 0.0

    for pos, rep_rank in replacement_ranks.items():
        mask    = df["position"] == pos
        pos_df  = df[mask].sort_values("avg", ascending=False).reset_index(drop=True)
        if pos_df.empty:
            continue
        idx     = min(rep_rank - 1, len(pos_df) - 1)
        rep_avg = float(pos_df.iloc[idx]["avg"])
        df.loc[mask, "VOR"] = (df.loc[mask, "avg"] - rep_avg).round(2)

    return df
