#[cfg(test)]
mod integration {
    use namelist::Namelist;

    #[test]
    fn parse_file_test_a() {
        let input = include_str!("TestA.fds");
        let t1 = std::time::Instant::now();
        let parser = namelist::NmlParser::new(std::io::Cursor::new(input));
        let nmls: Vec<_> = parser
            .collect::<Result<Vec<Namelist>, _>>()
            .expect("test parse failed");
        let mut new = String::new();
        for nml in nmls {
            new.push_str(&nml.to_string());
        }
        assert_eq!(input, new);
        let t2 = std::time::Instant::now();
        println!("Time Taken: {} s", (t2 - t1).as_secs_f64());
    }

    #[test]
    fn parse_file_test_d() {
        let input = include_str!("TestD.fds");
        let t1 = std::time::Instant::now();
        let parser = namelist::NmlParser::new(std::io::Cursor::new(input));
        let nmls: Vec<_> = parser
            .collect::<Result<Vec<Namelist>, _>>()
            .expect("test parse failed");
        let mut new = String::new();
        for nml in nmls {
            new.push_str(&nml.to_string());
        }
        assert_eq!(input, new);
        let t2 = std::time::Instant::now();
        println!("Time Taken: {} s", (t2 - t1).as_secs_f64());
    }

    #[test]
    fn parse_file_test_e() {
        let input = include_str!("TestE.fds");
        let t1 = std::time::Instant::now();
        let parser = namelist::NmlParser::new(std::io::Cursor::new(input));
        let nmls: Vec<_> = parser
            .collect::<Result<Vec<Namelist>, _>>()
            .expect("test parse failed");
        let mut new = String::new();
        for nml in nmls {
            new.push_str(&nml.to_string());
        }
        assert_eq!(input, new);
        let t2 = std::time::Instant::now();
        println!("Time Taken: {} s", (t2 - t1).as_secs_f64());
    }

    #[test]
    fn parse_file_test_f() {
        let input = include_str!("TestF.fds");
        let t1 = std::time::Instant::now();
        let parser = namelist::NmlParser::new(std::io::Cursor::new(input));
        let nmls: Vec<_> = parser
            .collect::<Result<Vec<Namelist>, _>>()
            .expect("test parse failed");
        let mut new = String::new();
        for nml in nmls {
            new.push_str(&nml.to_string());
        }
        assert_eq!(input, new);
        let t2 = std::time::Instant::now();
        println!("Time Taken: {} s", (t2 - t1).as_secs_f64());
    }

    #[test]
    fn parse_file_test_g() {
        let input = include_str!("TestG.fds");
        let t1 = std::time::Instant::now();
        let parser = namelist::NmlParser::new(std::io::Cursor::new(input));
        let nmls: Vec<_> = parser
            .collect::<Result<Vec<Namelist>, _>>()
            .expect("test parse failed");
        let mut new = String::new();
        for nml in nmls {
            new.push_str(&nml.to_string());
        }
        assert_eq!(input, new);
        let t2 = std::time::Instant::now();
        println!("Time Taken: {} s", (t2 - t1).as_secs_f64());
    }

    #[test]
    fn parse_file_test_room_fire() {
        let input = include_str!("room_fire.fds");
        let t1 = std::time::Instant::now();
        let parser = namelist::NmlParser::new(std::io::Cursor::new(input));
        let nmls: Vec<_> = parser
            .collect::<Result<Vec<Namelist>, _>>()
            .expect("test parse failed");
        let mut new = String::new();
        for nml in nmls {
            new.push_str(&nml.to_string());
        }
        assert_eq!(input, new);
        let t2 = std::time::Instant::now();
        println!("Time Taken: {} s", (t2 - t1).as_secs_f64());
    }
}
