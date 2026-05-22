-- Bears Tech Assessment in SQL
-- Data file expected: bird_tracking_1.csv
--
-- This script is written for DuckDB syntax so it can run directly from CSV.
-- If using another SQL engine, replace the CSV load step and keep the query blocks.

CREATE OR REPLACE VIEW bird_data AS
SELECT *
FROM read_csv_auto('bird_tracking_1.csv');

-- =============================================================
-- Top 5 Speeds By Species
-- =============================================================
WITH ranked AS (
    SELECT
        Bird_ID,
        Species,
        Speed_kmph,
        Region,
        Habitat,
        Flight_Distance_km,
        Flight_Duration_hours,
        ROW_NUMBER() OVER (PARTITION BY Species ORDER BY Speed_kmph DESC) AS rn
    FROM bird_data
)
SELECT
    Bird_ID,
    Species,
    Speed_kmph,
    Region,
    Habitat,
    Flight_Distance_km,
    Flight_Duration_hours
FROM ranked
WHERE rn <= 5
ORDER BY Species, Speed_kmph DESC;

SELECT
    Species,
    MAX(Speed_kmph) AS max_speed,
    AVG(Speed_kmph) AS mean_speed,
    MEDIAN(Speed_kmph) AS median_speed,
    COUNT(*) AS count
FROM bird_data
GROUP BY Species
ORDER BY mean_speed DESC;

-- =============================================================
-- Crane Migration Success Rate
-- =============================================================
SELECT
    COUNT(*) AS total_cranes,
    SUM(CASE WHEN Migration_Success = 'Successful' THEN 1 ELSE 0 END) AS successful_migrations,
    SUM(CASE WHEN Migration_Success = 'Failed' THEN 1 ELSE 0 END) AS failed_migrations,
    (SUM(CASE WHEN Migration_Success = 'Successful' THEN 1 ELSE 0 END) * 100.0) / NULLIF(COUNT(*), 0) AS success_rate
FROM bird_data
WHERE Species = 'Crane';

-- =============================================================
-- Fastest-Bird Travel Time For 2100 Miles
-- =============================================================
SELECT
    ROUND(((2100 * 1.6) / MAX(Speed_kmph)) * 60, 1) AS time_minutes_rounded
FROM bird_data;

-- =============================================================
-- Storks With Low Observation Quality
-- =============================================================
SELECT
    COUNT(*) AS total_storks,
    SUM(CASE WHEN Observation_Quality = 'Low' THEN 1 ELSE 0 END) AS low_quality_storks,
    (SUM(CASE WHEN Observation_Quality = 'Low' THEN 1 ELSE 0 END) * 100.0) / NULLIF(COUNT(*), 0) AS low_quality_pct
FROM bird_data
WHERE Species = 'Stork';

-- =============================================================
-- Habitat + Weather Combination With Most "Lost Signal"
-- =============================================================
SELECT
    TRIM(Habitat) AS Habitat,
    TRIM(Weather_Condition) AS Weather_Condition,
    COUNT(*) AS n
FROM bird_data
WHERE TRIM(Interrupted_Reason) = 'Lost Signal'
GROUP BY 1, 2
ORDER BY n DESC
LIMIT 1;

-- =============================================================
-- Highest Predator Sightings Per Rest Stop (Rest_Stops >= 1)
-- =============================================================
SELECT
    Region,
    SUM(Predator_Sightings) / NULLIF(SUM(Rest_Stops), 0) AS sightings_per_rest_stop
FROM bird_data
WHERE Rest_Stops >= 1
  AND Region IS NOT NULL
  AND Predator_Sightings IS NOT NULL
  AND Rest_Stops IS NOT NULL
GROUP BY Region
ORDER BY sightings_per_rest_stop DESC;

-- =============================================================
-- Migration Success Rate: Stormy Weather + Poor Tracking Quality
-- =============================================================
SELECT
    ROUND(AVG(CASE WHEN TRIM(Migration_Success) = 'Successful' THEN 1.0 ELSE 0.0 END) * 100, 2) AS pct_success
FROM bird_data
WHERE TRIM(Weather_Condition) = 'Stormy'
  AND TRIM(Tracking_Quality) = 'Poor';
