use keyflow::chart;

fn main() {
    let result = chart! {"
Well - Jacob Collier
120bpm 4/4 #E

Intro 4
1_2 2maj_2 | 4 | x2

VS 16
1_2 2maj_2 | 4 | x^

[Hits]
'1_2 '2maj_2 | '4 |

VS

[Hits]

INST 8

[SOLO Keys] 8

Br 16

br

Outro 16

"};

    // Display the chart
    match result {
        Ok(chart) => {
            println!("{}", chart);
        }
        Err(e) => {
            eprintln!("Error parsing chart: {}", e);
            std::process::exit(1);
        }
    }
}

