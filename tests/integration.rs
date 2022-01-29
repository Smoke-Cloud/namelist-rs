#[cfg(test)]
mod integration {
    // This test is for benchmarking on large files.
    #[ignore]
    #[test]
    fn parse_file_test_old_a() {
        let input = include_str!("TestD.fds");
        let t1 = std::time::Instant::now();
        for _ in namelist::NmlParser::new(std::io::Cursor::new(input)) {}
        let t2 = std::time::Instant::now();
        println!("Time Taken: {} s", (t2 - t1).as_secs_f64());
    }

    // This test is for benchmarking on large files.
    #[ignore]
    #[test]
    fn parse_file_test_fmt_a() {
        let file = include_str!("TestD.fds");
        let input = file.to_string();
        let t1 = std::time::Instant::now();
        let result = namelist::lst::tokenize_nml(input);
        // for element in result.elements.iter() {
        //     let ss = &result.content[element.span.start..(element.span.start + element.span.len)];
        //     println!("{:?}: {:?}", element, ss);
        // }
        let t2 = std::time::Instant::now();
        println!("Time Taken: {} s", (t2 - t1).as_secs_f64());
        assert_eq!(file, result.to_string());
    }
}
