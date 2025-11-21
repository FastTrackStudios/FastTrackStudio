use keyflow::chart;

fn main() {
    let result = chart! {"
All Along The Watchtower - Jimi Hendrix
117bpm 4/4 #E

/SMART_REPEATS=true

; (Auto 2 measures of Count In hits on & 4 &)

Count In 2
S4 s4. 5_8 _8 6_8  

Intro 4
6_8 6 6 s4 5 5 5 | 
4 _8 _8 s4 5 _8 6 

6_8 6 6 s4 5 5 5 | 
4 _8 _8 s4 5-> _8 6 
; Hit on & of 4

^Band-In 4
6_8 6 6 s4 s4 5 5 4 | 
4 _8 _8  s4 s4 5 _8 6 x2

VS 16
6_2 5 4 5 x^

INST 8
6 5 4 5 x^

VS 16
6 5 4 5 x^

INST 8
6 5 4 5 x^

Down 8
6 5 4 5 x^

GTR SOLO 16
6 5 4 5 x^

VS 16
6 5 4 5 x^

INST 16 
6 5 4 5 x^

END 1
6 /fermata

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
