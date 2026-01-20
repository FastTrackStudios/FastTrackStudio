//! Measure layout utilities for chart rendering.
//!
//! This module provides functions for measure width distribution
//! and system grouping.

/// Distribute available width among measures using spring physics.
///
/// This implements MuseScore-style proportional spacing where measures with
/// more content receive proportionally more width.
///
/// # Arguments
/// * `weights` - Content weights for each regular measure
/// * `count_in_measures` - Number of count-in measures (fixed width)
/// * `total_width` - Total available width for all measures
/// * `compact_scale` - Scale factor for count-in measures (typically 0.5)
/// * `base_measure_width` - Base width for a single measure
///
/// # Returns
/// Vector of widths for each measure (count-in measures first, then regular)
#[must_use]
pub fn distribute_measure_widths(
    weights: &[f64],
    count_in_measures: usize,
    total_width: f64,
    compact_scale: f64,
    base_measure_width: f64,
) -> Vec<f64> {
    if weights.is_empty() {
        return Vec::new();
    }

    // Calculate count-in width (fixed, compact)
    let count_in_width = base_measure_width * compact_scale;
    let count_in_total = count_in_measures as f64 * count_in_width;

    // Remaining width for regular measures
    let regular_width = total_width - count_in_total;

    // Sum of weights for spring calculation
    let weight_sum: f64 = weights.iter().sum();

    // Distribute width proportionally to weights
    let mut widths = Vec::with_capacity(count_in_measures + weights.len());

    // Add count-in widths
    for _ in 0..count_in_measures {
        widths.push(count_in_width);
    }

    // Add regular measure widths (proportional to weight)
    if weight_sum > 0.0 {
        for &weight in weights {
            let proportion = weight / weight_sum;
            let measure_width = regular_width * proportion;
            widths.push(measure_width);
        }
    } else {
        // Fallback: equal distribution
        let equal_width = regular_width / weights.len() as f64;
        for _ in weights {
            widths.push(equal_width);
        }
    }

    widths
}

/// Group measures into systems based on maximum measures per system.
///
/// # Arguments
/// * `measure_count` - Total number of measures to group
/// * `max_measures_per_system` - Maximum measures allowed per system
///
/// # Returns
/// Vector of systems, each containing measure indices
#[must_use]
pub fn group_measures_into_systems(
    measure_count: usize,
    max_measures_per_system: usize,
) -> Vec<Vec<usize>> {
    let mut systems = Vec::new();
    let mut current_system = Vec::new();

    for i in 0..measure_count {
        current_system.push(i);
        if current_system.len() >= max_measures_per_system {
            systems.push(std::mem::take(&mut current_system));
        }
    }

    if !current_system.is_empty() {
        systems.push(current_system);
    }

    systems
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_distribute_measure_widths_equal_weights() {
        let weights = vec![1.0, 1.0, 1.0, 1.0];
        let widths = distribute_measure_widths(&weights, 0, 400.0, 0.5, 100.0);

        assert_eq!(widths.len(), 4);
        // All measures should have equal width
        for &w in &widths {
            assert!((w - 100.0).abs() < 0.001);
        }
    }

    #[test]
    fn test_distribute_measure_widths_with_count_in() {
        let weights = vec![1.0, 1.0];
        let widths = distribute_measure_widths(&weights, 2, 400.0, 0.5, 100.0);

        // 2 count-in + 2 regular = 4 measures
        assert_eq!(widths.len(), 4);

        // Count-in measures should be 50.0 each (100 * 0.5)
        assert!((widths[0] - 50.0).abs() < 0.001);
        assert!((widths[1] - 50.0).abs() < 0.001);

        // Regular measures share remaining 300.0 equally
        assert!((widths[2] - 150.0).abs() < 0.001);
        assert!((widths[3] - 150.0).abs() < 0.001);
    }

    #[test]
    fn test_distribute_measure_widths_proportional() {
        // Different weights should give different widths
        let weights = vec![1.0, 2.0, 1.0];
        let widths = distribute_measure_widths(&weights, 0, 400.0, 0.5, 100.0);

        assert_eq!(widths.len(), 3);

        // Weight 1.0 should get 1/4 of width
        assert!((widths[0] - 100.0).abs() < 0.001);
        // Weight 2.0 should get 2/4 of width
        assert!((widths[1] - 200.0).abs() < 0.001);
        // Weight 1.0 should get 1/4 of width
        assert!((widths[2] - 100.0).abs() < 0.001);
    }

    #[test]
    fn test_distribute_measure_widths_empty() {
        let weights: Vec<f64> = vec![];
        let widths = distribute_measure_widths(&weights, 0, 400.0, 0.5, 100.0);

        assert!(widths.is_empty());
    }

    #[test]
    fn test_group_measures_into_systems_basic() {
        let systems = group_measures_into_systems(8, 4);

        assert_eq!(systems.len(), 2);
        assert_eq!(systems[0], vec![0, 1, 2, 3]);
        assert_eq!(systems[1], vec![4, 5, 6, 7]);
    }

    #[test]
    fn test_group_measures_into_systems_partial_last() {
        let systems = group_measures_into_systems(10, 4);

        assert_eq!(systems.len(), 3);
        assert_eq!(systems[0], vec![0, 1, 2, 3]);
        assert_eq!(systems[1], vec![4, 5, 6, 7]);
        assert_eq!(systems[2], vec![8, 9]);
    }

    #[test]
    fn test_group_measures_into_systems_fewer_than_max() {
        let systems = group_measures_into_systems(3, 4);

        assert_eq!(systems.len(), 1);
        assert_eq!(systems[0], vec![0, 1, 2]);
    }

    #[test]
    fn test_group_measures_into_systems_empty() {
        let systems = group_measures_into_systems(0, 4);

        assert!(systems.is_empty());
    }
}
