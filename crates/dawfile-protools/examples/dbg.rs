use dawfile_protools::raw_block::RawBlock;
fn fa<'a>(bs: &'a [RawBlock], ct: u16, o: &mut Vec<&'a RawBlock>) {
    for b in bs {
        if b.content_type_raw == ct {
            o.push(b)
        }
        fa(&b.children, ct, o);
    }
}
fn main() {
    let raw = std::fs::read(std::env::args().nth(1).unwrap()).unwrap();
    let s = dawfile_protools::parse_raw(raw).unwrap();
    let d = s.cursor().data();
    let mut v = Vec::new();
    fa(&s.blocks, 0x2633, &mut v);
    println!("=== {} MidiRegionNew (0x2633) blocks ===", v.len());
    let b = v[0];
    let lo = b.start;
    let hi = (b.start + 11 + b.block_size as usize + 8).min(d.len());
    println!(
        "0x2633[0] start={} block_size={} children={}",
        b.start,
        b.block_size,
        b.children.len()
    );
    let hx: String = d[lo..hi]
        .iter()
        .map(|x| format!("{:02x}", x))
        .collect::<Vec<_>>()
        .join(" ");
    println!("{}", hx);
    println!(
        "(ascii) {}",
        d[lo..hi]
            .iter()
            .map(|&x| if x >= 0x20 && x < 0x7f {
                x as char
            } else {
                '.'
            })
            .collect::<String>()
    );
    // 0x2634 map header
    let mut m = Vec::new();
    fa(&s.blocks, 0x2634, &mut m);
    let mb = m[0];
    println!(
        "\n0x2634 map start={} block_size={} children={}",
        mb.start,
        mb.block_size,
        mb.children.len()
    );
    let mhx: String = d[mb.start..(mb.start + 30)]
        .iter()
        .map(|x| format!("{:02x}", x))
        .collect::<Vec<_>>()
        .join(" ");
    println!("{}", mhx);
    // 0x1057 + 0x1058
    let mut g = Vec::new();
    fa(&s.blocks, 0x1057, &mut g);
    let gb = g[0];
    println!(
        "\n0x1057[0] start={} block_size={} children={}",
        gb.start,
        gb.block_size,
        gb.children.len()
    );
    let ghx: String = d[gb.start..(gb.start + 24)]
        .iter()
        .map(|x| format!("{:02x}", x))
        .collect::<Vec<_>>()
        .join(" ");
    println!("{}", ghx);
}
