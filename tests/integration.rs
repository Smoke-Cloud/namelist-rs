#[cfg(test)]
mod integration {
    // This test is for benchmarking on large files.
    #[ignore]
    #[test]
    fn parse_file_test_a() {
        let input = include_str!("TestD.fds");
        let t1 = std::time::Instant::now();
        for _ in namelist::NmlParser::new(std::io::Cursor::new(input)) {}
        let t2 = std::time::Instant::now();
        println!("Time Taken: {} s", (t2 - t1).as_secs_f64());
    }
}
