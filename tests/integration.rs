#[cfg(test)]
mod integration {
    #[test]
    fn parse_file_test_a() {
        let input = include_str!("TestD.fds");
        let t1 = std::time::Instant::now();
        let parser = namelist::NmlParser::new(std::io::Cursor::new(input));
        let _: Vec<_> = parser.collect();
        let t2 = std::time::Instant::now();
        println!("Time Taken: {} s", (t2 - t1).as_secs_f64());
    }
}
