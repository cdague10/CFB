"""Bears Tech Assessment in Python.

Data file expected: bird_tracking_1.csv
"""

import matplotlib.pyplot as plt
import pandas as pd


def main() -> None:
    # Load bird tracking data
    bird_data = pd.read_csv("bird_tracking_1.csv")

    # Top 5 speeds by species
    top_5_speeds_by_species = (
        bird_data.sort_values(["Species", "Speed_kmph"], ascending=[True, False])
        .groupby("Species", group_keys=False)
        .head(5)
        .loc[
            :,
            [
                "Bird_ID",
                "Species",
                "Speed_kmph",
                "Region",
                "Habitat",
                "Flight_Distance_km",
                "Flight_Duration_hours",
            ],
        ]
    )
    print(top_5_speeds_by_species)

    species_summary = (
        bird_data.groupby("Species", dropna=False)
        .agg(
            max_speed=("Speed_kmph", "max"),
            mean_speed=("Speed_kmph", "mean"),
            median_speed=("Speed_kmph", "median"),
            count=("Species", "size"),
        )
        .reset_index()
        .sort_values("mean_speed", ascending=False)
    )

    print("\nSpecies Speed Summary:")
    print(species_summary)

    # Match the R bar chart output for top speeds.
    plot_df = top_5_speeds_by_species.copy()
    plot_df["Bird_Species"] = plot_df["Bird_ID"].astype(str) + " - " + plot_df["Species"].astype(str)
    plot_df = plot_df.sort_values("Speed_kmph", ascending=True)

    plt.figure(figsize=(10, 8))
    plt.barh(plot_df["Bird_Species"], plot_df["Speed_kmph"])
    plt.title("Top 5 Speeds by Bird Species")
    plt.xlabel("Speed (km/h)")
    plt.ylabel("Bird ID - Species")
    plt.tight_layout()
    plt.show()

    # Crane migration success rate
    crane_data = bird_data[bird_data["Species"] == "Crane"]
    total_cranes = len(crane_data)
    successful_migrations = (crane_data["Migration_Success"] == "Successful").sum()
    failed_migrations = (crane_data["Migration_Success"] == "Failed").sum()
    success_rate = (successful_migrations / total_cranes) * 100 if total_cranes else float("nan")

    crane_success = pd.DataFrame(
        {
            "total_cranes": [total_cranes],
            "successful_migrations": [successful_migrations],
            "failed_migrations": [failed_migrations],
            "success_rate": [success_rate],
        }
    )

    print("\n=== CRANE MIGRATION SUCCESS RATE ===")
    print(crane_success)

    # Fastest-bird travel time for 2100 miles
    distance_km = 2100 * 1.6
    max_speed_kmph = bird_data["Speed_kmph"].max(skipna=True)
    time_minutes = (distance_km / max_speed_kmph) * 60
    time_minutes_rounded = round(time_minutes, 1)
    print(time_minutes_rounded)

    # Storks with low observation quality
    stork_data = bird_data[bird_data["Species"] == "Stork"]
    total_storks = len(stork_data)
    low_quality_storks = (stork_data["Observation_Quality"] == "Low").sum()
    low_quality_pct = (low_quality_storks / total_storks) * 100 if total_storks else float("nan")

    stork_quality_summary = pd.DataFrame(
        {
            "total_storks": [total_storks],
            "low_quality_storks": [low_quality_storks],
            "low_quality_pct": [low_quality_pct],
        }
    )

    print("\n=== STORK OBSERVATION QUALITY (LOW) ===")
    print(stork_quality_summary)

    # Habitat + weather combination with most "Lost Signal"
    cleaned = bird_data.copy()
    for col in ["Interrupted_Reason", "Habitat", "Weather_Condition", "Tracking_Quality", "Migration_Success"]:
        cleaned[col] = cleaned[col].astype(str).str.strip()

    lost_signal_top = (
        cleaned[cleaned["Interrupted_Reason"] == "Lost Signal"]
        .groupby(["Habitat", "Weather_Condition"], dropna=False)
        .size()
        .reset_index(name="n")
        .sort_values("n", ascending=False)
        .head(1)
    )

    print(lost_signal_top)

    # Highest predator sightings per rest stop (Rest_Stops >= 1)
    most_predator_sightings = (
        bird_data[
            (bird_data["Rest_Stops"] >= 1)
            & bird_data["Region"].notna()
            & bird_data["Predator_Sightings"].notna()
            & bird_data["Rest_Stops"].notna()
        ]
        .groupby("Region", dropna=False)
        .agg(
            sightings_per_rest_stop=(
                "Predator_Sightings",
                lambda x: x.sum() / bird_data.loc[x.index, "Rest_Stops"].sum(),
            )
        )
        .reset_index()
        .sort_values("sightings_per_rest_stop", ascending=False)
    )

    print(most_predator_sightings)

    # Migration success rate: stormy weather + poor tracking quality
    stormy_poor = cleaned[
        (cleaned["Weather_Condition"] == "Stormy") & (cleaned["Tracking_Quality"] == "Poor")
    ]
    pct_success = round((stormy_poor["Migration_Success"] == "Successful").mean(skipna=True) * 100, 2)
    print(f"{pct_success:.2f}%")


if __name__ == "__main__":
    main()
