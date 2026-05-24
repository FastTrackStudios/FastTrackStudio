use dawfile_protools::raw_block::RawBlock;
fn main() {
    let raw = std::fs::read(std::env::args().nth(1).unwrap()).unwrap();
    let s = dawfile_protools::parse_raw(raw).unwrap();
    let d = s.cursor().data();
    // top-level block list (offset, ct) for correlation
    println!("=== first 12 top-level blocks (offset, ct) ===");
    for b in s.blocks.iter().take(12) {
        println!(
            "  off={} ct={:#06x} size={}",
            b.start, b.content_type_raw, b.block_size
        );
    }
    let mut reg = Vec::new();
    fn fa<'a>(bs: &'a [RawBlock], ct: u16, o: &mut Vec<&'a RawBlock>) {
        for b in bs {
            if b.content_type_raw == ct {
                o.push(b)
            }
            fa(&b.children, ct, o);
        }
    }
    fa(&s.blocks, 0x0002, &mut reg);
    let r = reg[0];
    let pl = r.start + 9;
    println!("=== registry payload first 220 bytes (16/row, +off) ===");
    for row in 0..14 {
        let o = pl + row * 16;
        let line: String = d[o..o + 16]
            .iter()
            .map(|x| format!("{:02x}", x))
            .collect::<Vec<_>>()
            .join(" ");
        println!("  +{:<4} {}", row * 16, line);
    }
}
