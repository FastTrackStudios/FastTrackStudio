//! Live graph clock control via `pw-metadata` (same approach as
//! daw-audio-io's `pw` module: express intent, shell out, best-effort —
//! absent tools mean no-ops, no cfg needed).

use std::process::{Command, Stdio};

use patchbay_proto::ClockInfo;

fn quiet(program: &str) -> Command {
    let mut c = Command::new(program);
    c.stdout(Stdio::null()).stderr(Stdio::null());
    c
}

/// Force the graph quantum (frames, live); `0` clears the force.
pub fn force_quantum(frames: u32) {
    let _ = quiet("pw-metadata")
        .args(["-n", "settings", "0", "clock.force-quantum", &frames.to_string()])
        .status();
}

/// Read the live clock settings; zeroed when `pw-metadata` is missing.
pub fn clock_info() -> ClockInfo {
    let out = Command::new("pw-metadata")
        .args(["-n", "settings"])
        .output()
        .map(|o| String::from_utf8_lossy(&o.stdout).into_owned())
        .unwrap_or_default();
    let mut c = ClockInfo::default();
    // Lines look like: update: id:0 key:'clock.rate' value:'48000' type:''
    for line in out.lines() {
        let Some((key, value)) = parse_metadata_line(line) else {
            continue;
        };
        let num = || value.parse::<u32>().unwrap_or(0);
        match key {
            "clock.rate" => c.rate = num(),
            "clock.quantum" => c.quantum = num(),
            "clock.force-quantum" => c.force_quantum = num(),
            "clock.force-rate" => c.force_rate = num(),
            "clock.min-quantum" => c.min_quantum = num(),
            "clock.max-quantum" => c.max_quantum = num(),
            _ => {}
        }
    }
    c
}

/// Pull `key:'…' value:'…'` out of a pw-metadata line.
fn parse_metadata_line(line: &str) -> Option<(&str, &str)> {
    let key_start = line.find("key:'")? + 5;
    let key_end = key_start + line[key_start..].find('\'')?;
    let val_start = line.find("value:'")? + 7;
    let val_end = val_start + line[val_start..].find('\'')?;
    Some((&line[key_start..key_end], &line[val_start..val_end]))
}
