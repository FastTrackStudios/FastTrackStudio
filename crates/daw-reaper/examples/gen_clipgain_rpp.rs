use dawfile_reaper::RppSerialize;
use dawfile_reaper::builder::ReaperProjectBuilder;
fn main() {
    let out = std::env::args().nth(1).unwrap();
    let p=ReaperProjectBuilder::new().tempo_with_time_sig(120.0,4,4)
    .track("A",|t|t.item(0.0,5.0,|it|it.name("ca").source_wave("/Users/codywright/Downloads/PNG WORSHIP COLLECTIVE SESSION FILES/10 REASON WHY/Audio Files/10 REASON WHY demo (Bass)_1.1.wav").gain(0.123456)))
    .track("B",|t|t.item(0.0,5.0,|it|it.name("cb").source_wave("/Users/codywright/Downloads/PNG WORSHIP COLLECTIVE SESSION FILES/10 REASON WHY/Audio Files/10 REASON WHY demo (Bass)_1.wav").gain(0.654321)))
    .track("C",|t|t.item(0.0,5.0,|it|it.name("cc").source_wave("/Users/codywright/Downloads/PNG WORSHIP COLLECTIVE SESSION FILES/10 REASON WHY/Audio Files/10 REASON WHY demo (Drums)_1.1.wav").gain(0.222333)))
    .build();
    std::fs::write(&out, p.to_rpp_string()).unwrap();
}
