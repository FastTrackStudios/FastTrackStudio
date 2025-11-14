use keyflow::chart;

fn main() {
    let result = chart! {"
Reckless Love - Cory Asbury
68bpm 6/8 #G

in
6 5 4 1

vs
6 5 4 4 x4

ch
6 5 4 1 x4

Instrumental
6 5 4 1 x2

vs
ch
Instrumental

br
6 6 6 6 x2

br 
6 5 4 1 x2

ch

br

outro
6 5 4 1 x2
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
