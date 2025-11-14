use keyflow::chart;

fn main() {
    let result = chart! {"
    Build My Life - Housefires
    70bpm 4/4 #C

    in
    1 4 1/3 4

    vs
    1 4 1/3 4 x2
    vs

    ch
    4 2 1 6 x2

    instrumental
    4 //// //// 

    vs 
    ch

    instrumental
    4 5 6 1/3

    br
    4 5 6 1/3 x4

    ch

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