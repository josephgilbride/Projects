import streamlit as st
import pandas as pd
import numpy as np
import plotly.graph_objects as go

from ff_data_loader import (
    fetch_weekly_stats, build_player_summary, compute_vor,
    load_pfr_excel, build_weighted_summary, EXCEL_2025_PATH,
)
import os

st.set_page_config(page_title="Fantasy Football Draft Prep", layout="wide", page_icon="🏈")
st.markdown(
    "<style>.stTabs [data-baseweb='tab'] { font-size: 1.05rem; padding: 10px 16px; }</style>",
    unsafe_allow_html=True,
)
st.title("🏈 Fantasy Football Draft Prep")

# ── Sidebar ──────────────────────────────────────────────────────────────────
with st.sidebar:
    st.header("Settings")

    _has_2025 = os.path.exists(EXCEL_2025_PATH)
    _weighted_label = "Weighted (2yr)" if _has_2025 else "Weighted (2yr)"
    _seasons  = ([2025] if _has_2025 else []) + [_weighted_label, 2024, 2023, 2022]
    season = st.selectbox("Season", _seasons, index=0)
    if season == 2025:
        st.caption("2025 data: Pro Football Reference season totals (no week-by-week).")
    elif season == _weighted_label:
        _wt_desc = "2025×0.8 + 2024×0.2" if _has_2025 else "2024×0.8 + 2023×0.2"
        st.caption(f"Weighted blend: {_wt_desc}. Week-by-week from prior season where available.")
    else:
        st.caption("Data via nflverse (last published: 2024 season).")
    scoring = st.selectbox("Scoring Format", ["PPR", "Half-PPR", "Standard"], index=0)
    min_games = st.slider("Min Games Played", 1, 10, 4)

    st.divider()
    st.subheader("VOR Replacement Ranks")
    rep_qb = st.number_input("QB replacement rank", 1, 20, 12)
    rep_rb = st.number_input("RB replacement rank", 1, 40, 24)
    rep_wr = st.number_input("WR replacement rank", 1, 50, 36)
    rep_te = st.number_input("TE replacement rank", 1, 20, 12)
    replacement_ranks = {"QB": rep_qb, "RB": rep_rb, "WR": rep_wr, "TE": rep_te}

    st.divider()
    if st.button("Clear Draft Board", use_container_width=True):
        for k in ("my_team", "drafted_others"):
            st.session_state.pop(k, None)

# ── Session state defaults ───────────────────────────────────────────────────
if "my_team" not in st.session_state:
    st.session_state["my_team"] = set()
if "drafted_others" not in st.session_state:
    st.session_state["drafted_others"] = set()

# ── Fetch & process data ─────────────────────────────────────────────────────
_weighted_label = "Weighted (2yr)"
with st.spinner(f"Loading {season} season stats…"):
    if season == 2025:
        weekly  = None
        summary = load_pfr_excel(scoring=scoring, min_games=min_games)
        _weighted_desc = ""
    elif season == _weighted_label:
        weekly  = None   # combined view has no single weekly dataset
        summary, _weighted_desc = build_weighted_summary(
            scoring=scoring, min_games=min_games
        )
    else:
        weekly  = fetch_weekly_stats(year=season, scoring=scoring)
        summary = build_player_summary(weekly, min_games=min_games)
        _weighted_desc = ""
    summary = compute_vor(summary, replacement_ranks)

if _weighted_desc:
    st.info(f"Weighted blend: **{_weighted_desc}** — avg = 0.8 × recent season + 0.2 × prior season. Players appearing in only one season use that season's avg unscaled.")

all_players = summary["player_display_name"].tolist()
positions   = sorted(summary["position"].unique())

# ── Tabs ─────────────────────────────────────────────────────────────────────
tab1, tab2, tab3, tab4 = st.tabs([
    "📋 Draft Board",
    "📊 Player Compare",
    "🎯 Lineup Builder",
    "📉 Position Scarcity",
])

# ═══════════════════════════════════════════════════════════════════════════
# TAB 1: Draft Board
# ═══════════════════════════════════════════════════════════════════════════
with tab1:
    st.subheader("Draft Board")
    st.caption(
        "Mark players as **My Team** (drafted by you) or **Gone** (drafted by others). "
        "The board updates live to show who's still available."
    )

    # ── Filters ──────────────────────────────────────────────────────────────
    f_col1, f_col2, f_col3, f_col4 = st.columns([2, 1, 1, 1])
    with f_col1:
        pos_filter = st.multiselect(
            "Position", positions, default=positions, key="db_pos"
        )
    with f_col2:
        sort_by = st.selectbox(
            "Sort by", ["VOR", "avg", "floor", "ceiling", "cv", "total"], index=0
        )
    with f_col3:
        show_gone = st.checkbox("Show Drafted (Others)", value=False)
    with f_col4:
        show_mine = st.checkbox("Show My Team only", value=False)

    # ── Build display table ───────────────────────────────────────────────────
    df = summary[summary["position"].isin(pos_filter)].copy()
    df = df.sort_values(sort_by, ascending=(sort_by == "cv")).reset_index(drop=True)
    df.insert(0, "Rank", df.index + 1)

    # Status column
    def _status(name: str) -> str:
        if name in st.session_state["my_team"]:
            return "✅ Mine"
        if name in st.session_state["drafted_others"]:
            return "❌ Gone"
        return "Available"

    df["Status"] = df["player_display_name"].map(_status)

    if show_mine:
        df = df[df["Status"] == "✅ Mine"]
    elif not show_gone:
        df = df[df["Status"] != "❌ Gone"]

    # ── Metrics summary ───────────────────────────────────────────────────────
    n_mine = len(st.session_state["my_team"])
    n_gone = len(st.session_state["drafted_others"])
    n_avail = len(df[df["Status"] == "Available"]) if not show_mine else 0

    m1, m2, m3 = st.columns(3)
    m1.metric("Available", n_avail)
    m2.metric("My Team", n_mine)
    m3.metric("Drafted (Others)", n_gone)

    # ── Colour coding ──────────────────────────────────────────────────────────
    def _row_style(row):
        if row["Status"] == "✅ Mine":
            return ["background-color: rgba(46,204,113,0.15)"] * len(row)
        if row["Status"] == "❌ Gone":
            return ["color: #666; text-decoration: line-through"] * len(row)
        return [""] * len(row)

    display_cols = ["Rank", "player_display_name", "position", "team",
                    "avg", "std", "floor", "ceiling", "cv", "VOR", "games", "Status"]
    display_cols = [c for c in display_cols if c in df.columns]
    display_df = df[display_cols].rename(columns={
        "player_display_name": "Player",
        "position": "Pos",
        "team": "Team",
        "avg": "Avg PPR",
        "std": "Std Dev",
        "floor": "Floor (P10)",
        "ceiling": "Ceiling (P90)",
        "cv": "CV (lower=consistent)",
        "games": "Games",
    })

    styled = display_df.style.apply(_row_style, axis=1).format({
        "Avg PPR":          "{:.1f}",
        "Std Dev":          "{:.1f}",
        "Floor (P10)":      "{:.1f}",
        "Ceiling (P90)":    "{:.1f}",
        "CV (lower=consistent)": "{:.3f}",
        "VOR":              "{:+.1f}",
    })

    st.dataframe(styled, hide_index=True, use_container_width=True, height=520)

    # ── Mark players ──────────────────────────────────────────────────────────
    st.divider()
    mark_col1, mark_col2, mark_col3 = st.columns(3)

    with mark_col1:
        pick_mine = st.selectbox(
            "Add to My Team", ["— select —"] + all_players, key="pick_mine"
        )
        if st.button("✅ Mark as Mine", use_container_width=True):
            if pick_mine != "— select —":
                st.session_state["my_team"].add(pick_mine)
                st.session_state["drafted_others"].discard(pick_mine)
                st.rerun()

    with mark_col2:
        pick_gone = st.selectbox(
            "Mark as Drafted (Others)", ["— select —"] + all_players, key="pick_gone"
        )
        if st.button("❌ Mark as Gone", use_container_width=True):
            if pick_gone != "— select —":
                st.session_state["drafted_others"].add(pick_gone)
                st.session_state["my_team"].discard(pick_gone)
                st.rerun()

    with mark_col3:
        pick_undo = st.selectbox(
            "Remove status from player",
            ["— select —"] + list(
                st.session_state["my_team"] | st.session_state["drafted_others"]
            ),
            key="pick_undo",
        )
        if st.button("↩ Clear Status", use_container_width=True):
            if pick_undo != "— select —":
                st.session_state["my_team"].discard(pick_undo)
                st.session_state["drafted_others"].discard(pick_undo)
                st.rerun()

# ═══════════════════════════════════════════════════════════════════════════
# TAB 2: Player Compare
# ═══════════════════════════════════════════════════════════════════════════
with tab2:
    st.subheader("Player Compare")
    st.caption("Compare up to 4 players side-by-side using their week-by-week point history.")

    selected_players = st.multiselect(
        "Select players (up to 4)",
        all_players,
        default=all_players[:2] if len(all_players) >= 2 else all_players,
        max_selections=4,
        key="compare_players",
    )

    if not selected_players:
        st.info("Select at least one player above.")
    else:
        _colors = ["royalblue", "tomato", "mediumseagreen", "darkorange"]

        if weekly is None:
            st.info(
                "Week-by-week charts are not available for 2025 — the data source "
                "(Pro Football Reference) only provides season totals, not game logs."
            )
        else:
            player_weekly = weekly[weekly["player_display_name"].isin(selected_players)].copy()

            # ── Line chart: weekly points ────────────────────────────────────────
            fig_line = go.Figure()
            for i, player in enumerate(selected_players):
                pw = player_weekly[player_weekly["player_display_name"] == player].sort_values("week")
                fig_line.add_trace(go.Scatter(
                    x=pw["week"], y=pw["fp"],
                    mode="lines+markers",
                    name=player,
                    line=dict(color=_colors[i % 4], width=2),
                    marker=dict(size=7),
                    hovertemplate=f"<b>{player}</b><br>Week %{{x}}: %{{y:.1f}} pts<extra></extra>",
                ))

            fig_line.update_layout(
                title=f"{season} Weekly {scoring} Points",
                xaxis=dict(title="Week", dtick=1),
                yaxis=dict(title="Fantasy Points"),
                height=400,
                legend=dict(orientation="h", y=-0.2),
                margin=dict(t=50, b=10, l=50, r=20),
            )
            st.plotly_chart(fig_line, use_container_width=True)

            # ── Box plot: point distribution ────────────────────────────────────
            fig_box = go.Figure()
            for i, player in enumerate(selected_players):
                pw = player_weekly[player_weekly["player_display_name"] == player]
                fig_box.add_trace(go.Box(
                    y=pw["fp"],
                    name=player,
                    marker_color=_colors[i % 4],
                    boxpoints="all",
                    jitter=0.35,
                    pointpos=-1.8,
                    hovertemplate="%{y:.1f} pts<extra></extra>",
                ))

            fig_box.update_layout(
                title="Point Distribution (Box Plot)",
                yaxis=dict(title="Fantasy Points"),
                height=380,
                margin=dict(t=50, b=10, l=50, r=20),
            )
            st.plotly_chart(fig_box, use_container_width=True)

        # ── Stats table ──────────────────────────────────────────────────────
        st.divider()
        st.subheader("Stats Comparison")
        stats_rows = []
        for player in selected_players:
            row_sum = summary[summary["player_display_name"] == player]
            if row_sum.empty:
                continue
            r = row_sum.iloc[0]
            stats_rows.append({
                "Player":   player,
                "Pos":      r["position"],
                "Team":     r["team"],
                "Games":    int(r["games"]),
                "Avg PPR":  r["avg"],
                "Std Dev":  r["std"],
                "Floor":    r["floor"],
                "Ceiling":  r["ceiling"],
                "CV":       r["cv"],
                "VOR":      r["VOR"],
                "Total":    r["total"],
            })
        if stats_rows:
            stats_df = pd.DataFrame(stats_rows)
            st.dataframe(
                stats_df.style.format({
                    "Avg PPR": "{:.2f}", "Std Dev": "{:.2f}",
                    "Floor":   "{:.2f}", "Ceiling": "{:.2f}",
                    "CV":      "{:.3f}", "VOR":     "{:+.2f}",
                    "Total":   "{:.1f}",
                }),
                hide_index=True, use_container_width=True,
            )

        # ── Week-by-week detail table ────────────────────────────────────────
        if weekly is not None:
            with st.expander("Week-by-week breakdown"):
                player_weekly_all = weekly[weekly["player_display_name"].isin(selected_players)].copy()
                pivot = (
                    player_weekly_all
                    .pivot_table(
                        index="week",
                        columns="player_display_name",
                        values="fp",
                        aggfunc="first",
                    )
                    .sort_index()
                )
                pivot = pivot[[p for p in selected_players if p in pivot.columns]]
                st.dataframe(
                    pivot.style.format("{:.1f}", na_rep="—")
                         .background_gradient(axis=None, cmap="YlGn"),
                    use_container_width=True,
                )

# ═══════════════════════════════════════════════════════════════════════════
# TAB 3: Lineup Builder
# ═══════════════════════════════════════════════════════════════════════════
with tab3:
    st.subheader("Lineup Builder")
    st.caption(
        "Build your optimal starting lineup. "
        "Use **Auto-fill Best Lineup** to fill each slot with the highest-VOR player. "
        "Toggle **My Team Only** to restrict choices to players you've drafted."
    )

    lb_col1, lb_col2 = st.columns([1, 2])

    with lb_col1:
        my_team_only = st.toggle("My Team Only", value=bool(st.session_state["my_team"]))

    # Player pool for selection
    if my_team_only and st.session_state["my_team"]:
        pool = summary[summary["player_display_name"].isin(st.session_state["my_team"])].copy()
    else:
        pool = summary[~summary["player_display_name"].isin(
            st.session_state["drafted_others"]
        )].copy()

    pool = pool.sort_values("avg", ascending=False)

    def _players_for(pos: str) -> list[str]:
        return pool[pool["position"] == pos]["player_display_name"].tolist()

    flex_positions = {"RB", "WR", "TE"}

    def _flex_players() -> list[str]:
        return pool[pool["position"].isin(flex_positions)]["player_display_name"].tolist()

    SLOTS = [
        ("QB",   "QB"),
        ("RB1",  "RB"),
        ("RB2",  "RB"),
        ("WR1",  "WR"),
        ("WR2",  "WR"),
        ("WR3",  "WR"),
        ("TE",   "TE"),
        ("FLEX", "FLEX"),
    ]
    MANUAL_SLOTS = ["K", "D/ST"]   # no statistical data available for these

    def _autofill():
        used: set[str] = set()
        for slot, pos in SLOTS:
            candidates = _players_for(pos) if pos != "FLEX" else _flex_players()
            candidates = [p for p in candidates if p not in used]
            if candidates:
                st.session_state[f"lb_{slot}"] = candidates[0]
                used.add(candidates[0])
            else:
                if candidates:
                    st.session_state[f"lb_{slot}"] = candidates[0]

    with lb_col2:
        st.button("⚡ Auto-fill Best Lineup", on_click=_autofill, use_container_width=True)

    st.divider()

    lineup_rows = []
    selected_in_lineup: set[str] = set()

    for slot, pos in SLOTS:
        candidates = _players_for(pos) if pos != "FLEX" else _flex_players()
        candidates = [p for p in candidates if p not in selected_in_lineup]
        options = ["— empty —"] + candidates

        default_key = f"lb_{slot}"
        stored = st.session_state.get(default_key, "— empty —")
        if stored not in options:
            stored = "— empty —"

        c1, c2, c3, c4, c5 = st.columns([1, 3, 1, 1, 1])
        c1.markdown(f"**{slot}**")

        chosen = c2.selectbox(
            slot, options, index=options.index(stored),
            key=default_key, label_visibility="collapsed",
        )

        if chosen != "— empty —":
            selected_in_lineup.add(chosen)
            row = summary[summary["player_display_name"] == chosen]
            if not row.empty:
                r = row.iloc[0]
                _floor   = r["floor"]   if pd.notna(r["floor"])   else None
                _ceiling = r["ceiling"] if pd.notna(r["ceiling"]) else None
                c3.metric("Avg", f"{r['avg']:.1f}")
                c4.metric("Floor",   f"{_floor:.1f}"   if _floor   is not None else "—")
                c5.metric("Ceiling", f"{_ceiling:.1f}" if _ceiling is not None else "—")
                lineup_rows.append({
                    "Slot": slot, "Player": chosen,
                    "Pos": r["position"], "Team": r["team"],
                    "Avg":     float(r["avg"]),
                    "Floor":   float(_floor)   if _floor   is not None else float("nan"),
                    "Ceiling": float(_ceiling) if _ceiling is not None else float("nan"),
                    "VOR":     float(r["VOR"]),
                })
        else:
            c3.write("—")
            c4.write("—")
            c5.write("—")

    # ── Manual slots: K and D/ST ──────────────────────────────────────────────
    st.divider()
    st.caption("K and D/ST — enter manually (no statistical data available for these positions).")
    for slot in MANUAL_SLOTS:
        mc1, mc2 = st.columns([1, 6])
        mc1.markdown(f"**{slot}**")
        mc2.text_input(slot, placeholder=f"e.g. {'Chiefs' if slot == 'D/ST' else 'Tucker'}", key=f"lb_{slot}", label_visibility="collapsed")

    # ── Lineup totals ─────────────────────────────────────────────────────────
    if lineup_rows:
        st.divider()
        lineup_df = pd.DataFrame(lineup_rows)

        t1, t2, t3, t4 = st.columns(4)
        _floor_sum   = lineup_df["Floor"].sum(skipna=True)
        _ceiling_sum = lineup_df["Ceiling"].sum(skipna=True)
        _has_variability = lineup_df["Floor"].notna().any()
        t1.metric("Projected Total (Avg)", f"{lineup_df['Avg'].sum():.1f}")
        t2.metric("Floor (P10 sum)",   f"{_floor_sum:.1f}"   if _has_variability else "—")
        t3.metric("Ceiling (P90 sum)", f"{_ceiling_sum:.1f}" if _has_variability else "—")
        t4.metric("Players Set", len(lineup_rows))

        # Per-player bar chart
        fig_lb = go.Figure()
        fig_lb.add_trace(go.Bar(
            x=lineup_df["Player"],
            y=lineup_df["Avg"],
            name="Avg",
            marker_color="steelblue",
            error_y=dict(
                type="data",
                symmetric=False,
                array=(lineup_df["Ceiling"] - lineup_df["Avg"]).fillna(0),
                arrayminus=(lineup_df["Avg"] - lineup_df["Floor"]).fillna(0),
                visible=_has_variability,
            ),
            hovertemplate=(
                "<b>%{x}</b><br>Avg: %{y:.1f}<extra></extra>"
            ),
        ))
        fig_lb.update_layout(
            title="Lineup Projected Points (bars = avg, error bars = floor/ceiling)",
            xaxis=dict(tickangle=-20),
            yaxis=dict(title="PPR Points"),
            height=380,
            margin=dict(t=50, b=60, l=50, r=20),
        )
        st.plotly_chart(fig_lb, use_container_width=True)

        st.dataframe(
            lineup_df.style.format({
                "Avg": "{:.1f}", "Floor": "{:.1f}",
                "Ceiling": "{:.1f}", "VOR": "{:+.1f}",
            }),
            hide_index=True, use_container_width=True,
        )

# ═══════════════════════════════════════════════════════════════════════════
# TAB 4: Position Scarcity
# ═══════════════════════════════════════════════════════════════════════════
with tab4:
    st.subheader("Position Scarcity & VOR")
    st.caption(
        "Value Over Replacement (VOR) shows how much a player is worth above the "
        "last startable player at their position. The cliff in each curve tells you "
        "when a position becomes scarce — draft before the drop."
    )

    pos_colors = {"QB": "royalblue", "RB": "mediumseagreen", "WR": "tomato", "TE": "darkorange"}

    # ── VOR by rank within each position ─────────────────────────────────────
    fig_scarcity = go.Figure()

    for pos in ["QB", "RB", "WR", "TE"]:
        pos_df = summary[summary["position"] == pos].sort_values("avg", ascending=False).reset_index(drop=True)
        if pos_df.empty:
            continue
        pos_df["pos_rank"] = pos_df.index + 1
        rep_rank = replacement_ranks.get(pos, 12)
        rep_idx  = min(rep_rank - 1, len(pos_df) - 1)
        rep_avg  = float(pos_df.iloc[rep_idx]["avg"])

        vor_vals = pos_df["avg"] - rep_avg

        fig_scarcity.add_trace(go.Scatter(
            x=pos_df["pos_rank"],
            y=vor_vals,
            mode="lines+markers",
            name=pos,
            line=dict(color=pos_colors.get(pos, "gray"), width=2),
            marker=dict(size=5),
            hovertemplate=(
                f"<b>{{{{text}}}}</b> ({pos})<br>"
                "Rank: %{x}<br>VOR: %{y:+.1f}<extra></extra>"
            ),
            text=pos_df["player_display_name"],
        ))

        # Mark the replacement level
        fig_scarcity.add_vline(
            x=rep_rank,
            line_dash="dot",
            line_color=pos_colors.get(pos, "gray"),
            line_width=1,
            opacity=0.5,
        )

    fig_scarcity.add_hline(y=0, line_dash="dash", line_color="white", line_width=1, opacity=0.4)
    fig_scarcity.update_layout(
        title="VOR by Position Rank (dotted lines = replacement level)",
        xaxis=dict(title="Position Rank", dtick=2),
        yaxis=dict(title="VOR (avg pts above replacement)"),
        height=480,
        legend=dict(orientation="h", y=-0.15),
        margin=dict(t=50, b=10, l=60, r=20),
    )
    st.plotly_chart(fig_scarcity, use_container_width=True)

    # ── Average points by rank ────────────────────────────────────────────────
    st.divider()
    st.subheader("Average PPR Points by Position Rank")

    fig_avg = go.Figure()
    for pos in ["QB", "RB", "WR", "TE"]:
        pos_df = summary[summary["position"] == pos].sort_values("avg", ascending=False).reset_index(drop=True)
        if pos_df.empty:
            continue
        pos_df["pos_rank"] = pos_df.index + 1
        fig_avg.add_trace(go.Scatter(
            x=pos_df["pos_rank"],
            y=pos_df["avg"],
            mode="lines+markers",
            name=pos,
            line=dict(color=pos_colors.get(pos, "gray"), width=2),
            marker=dict(size=5),
            text=pos_df["player_display_name"],
            hovertemplate=(
                f"<b>{{{{text}}}}</b> ({pos})<br>"
                "Rank: %{x}<br>Avg PPR: %{y:.1f}<extra></extra>"
            ),
        ))

    fig_avg.update_layout(
        xaxis=dict(title="Position Rank", dtick=2),
        yaxis=dict(title="Avg PPR Points / Game"),
        height=400,
        legend=dict(orientation="h", y=-0.2),
        margin=dict(t=20, b=10, l=60, r=20),
    )
    st.plotly_chart(fig_avg, use_container_width=True)

    # ── Per-position top players table ────────────────────────────────────────
    st.divider()
    st.subheader("Top Players by Position")

    for pos in ["QB", "RB", "WR", "TE"]:
        pos_df = summary[summary["position"] == pos].sort_values("avg", ascending=False).head(20).copy()
        pos_df = pos_df.reset_index(drop=True)
        pos_df.insert(0, "Pos Rank", pos_df.index + 1)

        with st.expander(f"{pos} — Top {len(pos_df)}"):
            st.dataframe(
                pos_df[["Pos Rank", "player_display_name", "team",
                         "avg", "std", "floor", "ceiling", "cv", "VOR", "games"]]
                .rename(columns={
                    "player_display_name": "Player",
                    "team": "Team",
                    "avg": "Avg PPR",
                    "std": "Std",
                    "floor": "Floor",
                    "ceiling": "Ceiling",
                    "cv": "CV",
                    "games": "Games",
                })
                .style.format({
                    "Avg PPR": "{:.1f}", "Std": "{:.1f}",
                    "Floor": "{:.1f}", "Ceiling": "{:.1f}",
                    "CV": "{:.3f}", "VOR": "{:+.1f}",
                }),
                hide_index=True, use_container_width=True,
            )
