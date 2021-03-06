let configs = [
    {
        "sid": "A1-learn-01",
        "group": "A1",
        "phase": "learn",
        "trial": 1,
        "agent": 42,
        "recipient": 31,
        "indicator": 1,
        "result": 52
    },
    {
        "sid": "A1-learn-02",
        "group": "A1",
        "phase": "learn",
        "trial": 2,
        "agent": 42,
        "recipient": 33,
        "indicator": 1,
        "result": 54
    },
    {
        "sid": "A1-learn-03",
        "group": "A1",
        "phase": "learn",
        "trial": 3,
        "agent": 42,
        "recipient": 52,
        "indicator": 1,
        "result": 53
    },
    {
        "sid": "A1-learn-04",
        "group": "A1",
        "phase": "learn",
        "trial": 4,
        "agent": 42,
        "recipient": 53,
        "indicator": 1,
        "result": 54
    },
    {
        "sid": "A1-learn-05",
        "group": "A1",
        "phase": "learn",
        "trial": 5,
        "agent": 42,
        "recipient": 61,
        "indicator": 1,
        "result": 52
    },
    {
        "sid": "A1-learn-06",
        "group": "A1",
        "phase": "learn",
        "trial": 6,
        "agent": 42,
        "recipient": 62,
        "indicator": 1,
        "result": 53
    },
    {
        "sid": "A1-gen-01",
        "group": "A1",
        "phase": "gen",
        "trial": 1,
        "agent": 32,
        "recipient": 32,
        "indicator": 1,
        "result": 43
    },
    {
        "sid": "A1-gen-02",
        "group": "A1",
        "phase": "gen",
        "trial": 2,
        "agent": 32,
        "recipient": 41,
        "indicator": 1,
        "result": 42
    },
    {
        "sid": "A1-gen-03",
        "group": "A1",
        "phase": "gen",
        "trial": 3,
        "agent": 32,
        "recipient": 51,
        "indicator": 1,
        "result": 42
    },
    {
        "sid": "A1-gen-04",
        "group": "A1",
        "phase": "gen",
        "trial": 4,
        "agent": 32,
        "recipient": 63,
        "indicator": 1,
        "result": 44
    },
    {
        "sid": "A1-gen-05",
        "group": "A1",
        "phase": "gen",
        "trial": 5,
        "agent": 42,
        "recipient": 32,
        "indicator": 1,
        "result": 53
    },
    {
        "sid": "A1-gen-06",
        "group": "A1",
        "phase": "gen",
        "trial": 6,
        "agent": 42,
        "recipient": 41,
        "indicator": 1,
        "result": 52
    },
    {
        "sid": "A1-gen-07",
        "group": "A1",
        "phase": "gen",
        "trial": 7,
        "agent": 42,
        "recipient": 51,
        "indicator": 1,
        "result": 52
    },
    {
        "sid": "A1-gen-08",
        "group": "A1",
        "phase": "gen",
        "trial": 8,
        "agent": 42,
        "recipient": 63,
        "indicator": 1,
        "result": 54
    },
    {
        "sid": "A1-gen-09",
        "group": "A1",
        "phase": "gen",
        "trial": 9,
        "agent": 43,
        "recipient": 32,
        "indicator": 1,
        "result": 53
    },
    {
        "sid": "A1-gen-10",
        "group": "A1",
        "phase": "gen",
        "trial": 10,
        "agent": 43,
        "recipient": 41,
        "indicator": 1,
        "result": 52
    },
    {
        "sid": "A1-gen-11",
        "group": "A1",
        "phase": "gen",
        "trial": 11,
        "agent": 43,
        "recipient": 51,
        "indicator": 1,
        "result": 52
    },
    {
        "sid": "A1-gen-12",
        "group": "A1",
        "phase": "gen",
        "trial": 12,
        "agent": 43,
        "recipient": 63,
        "indicator": 1,
        "result": 54
    },
    {
        "sid": "A1-gen-13",
        "group": "A1",
        "phase": "gen",
        "trial": 13,
        "agent": 61,
        "recipient": 32,
        "indicator": 1,
        "result": 73
    },
    {
        "sid": "A1-gen-14",
        "group": "A1",
        "phase": "gen",
        "trial": 14,
        "agent": 61,
        "recipient": 41,
        "indicator": 1,
        "result": 72
    },
    {
        "sid": "A1-gen-15",
        "group": "A1",
        "phase": "gen",
        "trial": 15,
        "agent": 61,
        "recipient": 51,
        "indicator": 1,
        "result": 72
    },
    {
        "sid": "A1-gen-16",
        "group": "A1",
        "phase": "gen",
        "trial": 16,
        "agent": 61,
        "recipient": 63,
        "indicator": 1,
        "result": 74
    },
    {
        "sid": "A2-learn-01",
        "group": "A2",
        "phase": "learn",
        "trial": 1,
        "agent": 42,
        "recipient": 31,
        "indicator": 1,
        "result": 52
    },
    {
        "sid": "A2-learn-02",
        "group": "A2",
        "phase": "learn",
        "trial": 2,
        "agent": 42,
        "recipient": 33,
        "indicator": 1,
        "result": 54
    },
    {
        "sid": "A2-learn-03",
        "group": "A2",
        "phase": "learn",
        "trial": 3,
        "agent": 42,
        "recipient": 52,
        "indicator": 1,
        "result": 53
    },
    {
        "sid": "A2-learn-04",
        "group": "A2",
        "phase": "learn",
        "trial": 4,
        "agent": 42,
        "recipient": 53,
        "indicator": 1,
        "result": 54
    },
    {
        "sid": "A2-learn-05",
        "group": "A2",
        "phase": "learn",
        "trial": 5,
        "agent": 42,
        "recipient": 61,
        "indicator": 1,
        "result": 52
    },
    {
        "sid": "A2-learn-06",
        "group": "A2",
        "phase": "learn",
        "trial": 6,
        "agent": 42,
        "recipient": 62,
        "indicator": 1,
        "result": 53
    },
    {
        "sid": "A2-gen-01",
        "group": "A2",
        "phase": "gen",
        "trial": 1,
        "agent": 32,
        "recipient": 32,
        "indicator": 1,
        "result": 43
    },
    {
        "sid": "A2-gen-02",
        "group": "A2",
        "phase": "gen",
        "trial": 2,
        "agent": 32,
        "recipient": 41,
        "indicator": 1,
        "result": 42
    },
    {
        "sid": "A2-gen-03",
        "group": "A2",
        "phase": "gen",
        "trial": 3,
        "agent": 32,
        "recipient": 51,
        "indicator": 1,
        "result": 42
    },
    {
        "sid": "A2-gen-04",
        "group": "A2",
        "phase": "gen",
        "trial": 4,
        "agent": 32,
        "recipient": 63,
        "indicator": 1,
        "result": 44
    },
    {
        "sid": "A2-gen-05",
        "group": "A2",
        "phase": "gen",
        "trial": 5,
        "agent": 42,
        "recipient": 32,
        "indicator": 1,
        "result": 53
    },
    {
        "sid": "A2-gen-06",
        "group": "A2",
        "phase": "gen",
        "trial": 6,
        "agent": 42,
        "recipient": 41,
        "indicator": 1,
        "result": 52
    },
    {
        "sid": "A2-gen-07",
        "group": "A2",
        "phase": "gen",
        "trial": 7,
        "agent": 42,
        "recipient": 51,
        "indicator": 1,
        "result": 52
    },
    {
        "sid": "A2-gen-08",
        "group": "A2",
        "phase": "gen",
        "trial": 8,
        "agent": 42,
        "recipient": 63,
        "indicator": 1,
        "result": 54
    },
    {
        "sid": "A2-gen-09",
        "group": "A2",
        "phase": "gen",
        "trial": 9,
        "agent": 43,
        "recipient": 32,
        "indicator": 1,
        "result": 53
    },
    {
        "sid": "A2-gen-10",
        "group": "A2",
        "phase": "gen",
        "trial": 10,
        "agent": 43,
        "recipient": 41,
        "indicator": 1,
        "result": 52
    },
    {
        "sid": "A2-gen-11",
        "group": "A2",
        "phase": "gen",
        "trial": 11,
        "agent": 43,
        "recipient": 51,
        "indicator": 1,
        "result": 52
    },
    {
        "sid": "A2-gen-12",
        "group": "A2",
        "phase": "gen",
        "trial": 12,
        "agent": 43,
        "recipient": 63,
        "indicator": 1,
        "result": 54
    },
    {
        "sid": "A2-gen-13",
        "group": "A2",
        "phase": "gen",
        "trial": 13,
        "agent": 61,
        "recipient": 32,
        "indicator": 1,
        "result": 73
    },
    {
        "sid": "A2-gen-14",
        "group": "A2",
        "phase": "gen",
        "trial": 14,
        "agent": 61,
        "recipient": 41,
        "indicator": 1,
        "result": 72
    },
    {
        "sid": "A2-gen-15",
        "group": "A2",
        "phase": "gen",
        "trial": 15,
        "agent": 61,
        "recipient": 51,
        "indicator": 1,
        "result": 72
    },
    {
        "sid": "A2-gen-16",
        "group": "A2",
        "phase": "gen",
        "trial": 16,
        "agent": 61,
        "recipient": 63,
        "indicator": 1,
        "result": 74
    },
    {
        "sid": "A3-learn-01",
        "group": "A3",
        "phase": "learn",
        "trial": 1,
        "agent": 42,
        "recipient": 31,
        "indicator": 0,
        "result": 52
    },
    {
        "sid": "A3-learn-02",
        "group": "A3",
        "phase": "learn",
        "trial": 2,
        "agent": 42,
        "recipient": 33,
        "indicator": 0,
        "result": 54
    },
    {
        "sid": "A3-learn-03",
        "group": "A3",
        "phase": "learn",
        "trial": 3,
        "agent": 42,
        "recipient": 52,
        "indicator": 0,
        "result": 53
    },
    {
        "sid": "A3-learn-04",
        "group": "A3",
        "phase": "learn",
        "trial": 4,
        "agent": 42,
        "recipient": 53,
        "indicator": 0,
        "result": 54
    },
    {
        "sid": "A3-learn-05",
        "group": "A3",
        "phase": "learn",
        "trial": 5,
        "agent": 42,
        "recipient": 61,
        "indicator": 0,
        "result": 52
    },
    {
        "sid": "A3-learn-06",
        "group": "A3",
        "phase": "learn",
        "trial": 6,
        "agent": 42,
        "recipient": 62,
        "indicator": 0,
        "result": 53
    },
    {
        "sid": "A3-gen-01",
        "group": "A3",
        "phase": "gen",
        "trial": 1,
        "agent": 32,
        "recipient": 32,
        "indicator": 0,
        "result": 43
    },
    {
        "sid": "A3-gen-02",
        "group": "A3",
        "phase": "gen",
        "trial": 2,
        "agent": 32,
        "recipient": 41,
        "indicator": 0,
        "result": 42
    },
    {
        "sid": "A3-gen-03",
        "group": "A3",
        "phase": "gen",
        "trial": 3,
        "agent": 32,
        "recipient": 51,
        "indicator": 0,
        "result": 42
    },
    {
        "sid": "A3-gen-04",
        "group": "A3",
        "phase": "gen",
        "trial": 4,
        "agent": 32,
        "recipient": 63,
        "indicator": 0,
        "result": 44
    },
    {
        "sid": "A3-gen-05",
        "group": "A3",
        "phase": "gen",
        "trial": 5,
        "agent": 42,
        "recipient": 32,
        "indicator": 0,
        "result": 53
    },
    {
        "sid": "A3-gen-06",
        "group": "A3",
        "phase": "gen",
        "trial": 6,
        "agent": 42,
        "recipient": 41,
        "indicator": 0,
        "result": 52
    },
    {
        "sid": "A3-gen-07",
        "group": "A3",
        "phase": "gen",
        "trial": 7,
        "agent": 42,
        "recipient": 51,
        "indicator": 0,
        "result": 52
    },
    {
        "sid": "A3-gen-08",
        "group": "A3",
        "phase": "gen",
        "trial": 8,
        "agent": 42,
        "recipient": 63,
        "indicator": 0,
        "result": 54
    },
    {
        "sid": "A3-gen-09",
        "group": "A3",
        "phase": "gen",
        "trial": 9,
        "agent": 43,
        "recipient": 32,
        "indicator": 0,
        "result": 53
    },
    {
        "sid": "A3-gen-10",
        "group": "A3",
        "phase": "gen",
        "trial": 10,
        "agent": 43,
        "recipient": 41,
        "indicator": 0,
        "result": 52
    },
    {
        "sid": "A3-gen-11",
        "group": "A3",
        "phase": "gen",
        "trial": 11,
        "agent": 43,
        "recipient": 51,
        "indicator": 0,
        "result": 52
    },
    {
        "sid": "A3-gen-12",
        "group": "A3",
        "phase": "gen",
        "trial": 12,
        "agent": 43,
        "recipient": 63,
        "indicator": 0,
        "result": 54
    },
    {
        "sid": "A3-gen-13",
        "group": "A3",
        "phase": "gen",
        "trial": 13,
        "agent": 61,
        "recipient": 32,
        "indicator": 0,
        "result": 73
    },
    {
        "sid": "A3-gen-14",
        "group": "A3",
        "phase": "gen",
        "trial": 14,
        "agent": 61,
        "recipient": 41,
        "indicator": 0,
        "result": 72
    },
    {
        "sid": "A3-gen-15",
        "group": "A3",
        "phase": "gen",
        "trial": 15,
        "agent": 61,
        "recipient": 51,
        "indicator": 0,
        "result": 72
    },
    {
        "sid": "A3-gen-16",
        "group": "A3",
        "phase": "gen",
        "trial": 16,
        "agent": 61,
        "recipient": 63,
        "indicator": 0,
        "result": 74
    },
    {
        "sid": "A4-learn-01",
        "group": "A4",
        "phase": "learn",
        "trial": 1,
        "agent": 42,
        "recipient": 31,
        "indicator": 1,
        "result": 52
    },
    {
        "sid": "A4-learn-02",
        "group": "A4",
        "phase": "learn",
        "trial": 2,
        "agent": 42,
        "recipient": 33,
        "indicator": 1,
        "result": 54
    },
    {
        "sid": "A4-learn-03",
        "group": "A4",
        "phase": "learn",
        "trial": 3,
        "agent": 42,
        "recipient": 52,
        "indicator": 1,
        "result": 53
    },
    {
        "sid": "A4-learn-04",
        "group": "A4",
        "phase": "learn",
        "trial": 4,
        "agent": 42,
        "recipient": 53,
        "indicator": 1,
        "result": 54
    },
    {
        "sid": "A4-learn-05",
        "group": "A4",
        "phase": "learn",
        "trial": 5,
        "agent": 42,
        "recipient": 61,
        "indicator": 1,
        "result": 52
    },
    {
        "sid": "A4-learn-06",
        "group": "A4",
        "phase": "learn",
        "trial": 6,
        "agent": 42,
        "recipient": 62,
        "indicator": 1,
        "result": 53
    },
    {
        "sid": "A4-gen-01",
        "group": "A4",
        "phase": "gen",
        "trial": 1,
        "agent": 32,
        "recipient": 32,
        "indicator": 1,
        "result": 43
    },
    {
        "sid": "A4-gen-02",
        "group": "A4",
        "phase": "gen",
        "trial": 2,
        "agent": 32,
        "recipient": 41,
        "indicator": 1,
        "result": 42
    },
    {
        "sid": "A4-gen-03",
        "group": "A4",
        "phase": "gen",
        "trial": 3,
        "agent": 32,
        "recipient": 51,
        "indicator": 1,
        "result": 42
    },
    {
        "sid": "A4-gen-04",
        "group": "A4",
        "phase": "gen",
        "trial": 4,
        "agent": 32,
        "recipient": 63,
        "indicator": 1,
        "result": 44
    },
    {
        "sid": "A4-gen-05",
        "group": "A4",
        "phase": "gen",
        "trial": 5,
        "agent": 42,
        "recipient": 32,
        "indicator": 1,
        "result": 53
    },
    {
        "sid": "A4-gen-06",
        "group": "A4",
        "phase": "gen",
        "trial": 6,
        "agent": 42,
        "recipient": 41,
        "indicator": 1,
        "result": 52
    },
    {
        "sid": "A4-gen-07",
        "group": "A4",
        "phase": "gen",
        "trial": 7,
        "agent": 42,
        "recipient": 51,
        "indicator": 1,
        "result": 52
    },
    {
        "sid": "A4-gen-08",
        "group": "A4",
        "phase": "gen",
        "trial": 8,
        "agent": 42,
        "recipient": 63,
        "indicator": 1,
        "result": 54
    },
    {
        "sid": "A4-gen-09",
        "group": "A4",
        "phase": "gen",
        "trial": 9,
        "agent": 43,
        "recipient": 32,
        "indicator": 1,
        "result": 53
    },
    {
        "sid": "A4-gen-10",
        "group": "A4",
        "phase": "gen",
        "trial": 10,
        "agent": 43,
        "recipient": 41,
        "indicator": 1,
        "result": 52
    },
    {
        "sid": "A4-gen-11",
        "group": "A4",
        "phase": "gen",
        "trial": 11,
        "agent": 43,
        "recipient": 51,
        "indicator": 1,
        "result": 52
    },
    {
        "sid": "A4-gen-12",
        "group": "A4",
        "phase": "gen",
        "trial": 12,
        "agent": 43,
        "recipient": 63,
        "indicator": 1,
        "result": 54
    },
    {
        "sid": "A4-gen-13",
        "group": "A4",
        "phase": "gen",
        "trial": 13,
        "agent": 61,
        "recipient": 32,
        "indicator": 1,
        "result": 73
    },
    {
        "sid": "A4-gen-14",
        "group": "A4",
        "phase": "gen",
        "trial": 14,
        "agent": 61,
        "recipient": 41,
        "indicator": 1,
        "result": 72
    },
    {
        "sid": "A4-gen-15",
        "group": "A4",
        "phase": "gen",
        "trial": 15,
        "agent": 61,
        "recipient": 51,
        "indicator": 1,
        "result": 72
    },
    {
        "sid": "A4-gen-16",
        "group": "A4",
        "phase": "gen",
        "trial": 16,
        "agent": 61,
        "recipient": 63,
        "indicator": 1,
        "result": 74
    },
    {
        "sid": "A5-learn-01",
        "group": "A5",
        "phase": "learn",
        "trial": 1,
        "agent": 31,
        "recipient": 42,
        "indicator": 1,
        "result": 43
    },
    {
        "sid": "A5-learn-02",
        "group": "A5",
        "phase": "learn",
        "trial": 2,
        "agent": 33,
        "recipient": 42,
        "indicator": 1,
        "result": 43
    },
    {
        "sid": "A5-learn-03",
        "group": "A5",
        "phase": "learn",
        "trial": 3,
        "agent": 52,
        "recipient": 42,
        "indicator": 1,
        "result": 63
    },
    {
        "sid": "A5-learn-04",
        "group": "A5",
        "phase": "learn",
        "trial": 4,
        "agent": 53,
        "recipient": 42,
        "indicator": 1,
        "result": 63
    },
    {
        "sid": "A5-learn-05",
        "group": "A5",
        "phase": "learn",
        "trial": 5,
        "agent": 61,
        "recipient": 42,
        "indicator": 1,
        "result": 73
    },
    {
        "sid": "A5-learn-06",
        "group": "A5",
        "phase": "learn",
        "trial": 6,
        "agent": 62,
        "recipient": 42,
        "indicator": 1,
        "result": 73
    },
    {
        "sid": "A5-gen-01",
        "group": "A5",
        "phase": "gen",
        "trial": 1,
        "agent": 32,
        "recipient": 32,
        "indicator": 1,
        "result": 43
    },
    {
        "sid": "A5-gen-02",
        "group": "A5",
        "phase": "gen",
        "trial": 2,
        "agent": 32,
        "recipient": 42,
        "indicator": 1,
        "result": 43
    },
    {
        "sid": "A5-gen-03",
        "group": "A5",
        "phase": "gen",
        "trial": 3,
        "agent": 32,
        "recipient": 43,
        "indicator": 1,
        "result": 44
    },
    {
        "sid": "A5-gen-04",
        "group": "A5",
        "phase": "gen",
        "trial": 4,
        "agent": 32,
        "recipient": 61,
        "indicator": 1,
        "result": 42
    },
    {
        "sid": "A5-gen-05",
        "group": "A5",
        "phase": "gen",
        "trial": 5,
        "agent": 41,
        "recipient": 32,
        "indicator": 1,
        "result": 53
    },
    {
        "sid": "A5-gen-06",
        "group": "A5",
        "phase": "gen",
        "trial": 6,
        "agent": 41,
        "recipient": 42,
        "indicator": 1,
        "result": 53
    },
    {
        "sid": "A5-gen-07",
        "group": "A5",
        "phase": "gen",
        "trial": 7,
        "agent": 41,
        "recipient": 43,
        "indicator": 1,
        "result": 54
    },
    {
        "sid": "A5-gen-08",
        "group": "A5",
        "phase": "gen",
        "trial": 8,
        "agent": 41,
        "recipient": 61,
        "indicator": 1,
        "result": 52
    },
    {
        "sid": "A5-gen-09",
        "group": "A5",
        "phase": "gen",
        "trial": 9,
        "agent": 51,
        "recipient": 32,
        "indicator": 1,
        "result": 63
    },
    {
        "sid": "A5-gen-10",
        "group": "A5",
        "phase": "gen",
        "trial": 10,
        "agent": 51,
        "recipient": 42,
        "indicator": 1,
        "result": 63
    },
    {
        "sid": "A5-gen-11",
        "group": "A5",
        "phase": "gen",
        "trial": 11,
        "agent": 51,
        "recipient": 43,
        "indicator": 1,
        "result": 64
    },
    {
        "sid": "A5-gen-12",
        "group": "A5",
        "phase": "gen",
        "trial": 12,
        "agent": 51,
        "recipient": 61,
        "indicator": 1,
        "result": 62
    },
    {
        "sid": "A5-gen-13",
        "group": "A5",
        "phase": "gen",
        "trial": 13,
        "agent": 63,
        "recipient": 32,
        "indicator": 1,
        "result": 73
    },
    {
        "sid": "A5-gen-14",
        "group": "A5",
        "phase": "gen",
        "trial": 14,
        "agent": 63,
        "recipient": 42,
        "indicator": 1,
        "result": 73
    },
    {
        "sid": "A5-gen-15",
        "group": "A5",
        "phase": "gen",
        "trial": 15,
        "agent": 63,
        "recipient": 43,
        "indicator": 1,
        "result": 74
    },
    {
        "sid": "A5-gen-16",
        "group": "A5",
        "phase": "gen",
        "trial": 16,
        "agent": 63,
        "recipient": 61,
        "indicator": 1,
        "result": 72
    },
    {
        "sid": "A6-learn-01",
        "group": "A6",
        "phase": "learn",
        "trial": 1,
        "agent": 31,
        "recipient": 42,
        "indicator": 1,
        "result": 43
    },
    {
        "sid": "A6-learn-02",
        "group": "A6",
        "phase": "learn",
        "trial": 2,
        "agent": 33,
        "recipient": 42,
        "indicator": 1,
        "result": 43
    },
    {
        "sid": "A6-learn-03",
        "group": "A6",
        "phase": "learn",
        "trial": 3,
        "agent": 52,
        "recipient": 42,
        "indicator": 1,
        "result": 63
    },
    {
        "sid": "A6-learn-04",
        "group": "A6",
        "phase": "learn",
        "trial": 4,
        "agent": 53,
        "recipient": 42,
        "indicator": 1,
        "result": 63
    },
    {
        "sid": "A6-learn-05",
        "group": "A6",
        "phase": "learn",
        "trial": 5,
        "agent": 61,
        "recipient": 42,
        "indicator": 1,
        "result": 73
    },
    {
        "sid": "A6-learn-06",
        "group": "A6",
        "phase": "learn",
        "trial": 6,
        "agent": 62,
        "recipient": 42,
        "indicator": 1,
        "result": 73
    },
    {
        "sid": "A6-gen-01",
        "group": "A6",
        "phase": "gen",
        "trial": 1,
        "agent": 32,
        "recipient": 32,
        "indicator": 1,
        "result": 43
    },
    {
        "sid": "A6-gen-02",
        "group": "A6",
        "phase": "gen",
        "trial": 2,
        "agent": 32,
        "recipient": 42,
        "indicator": 1,
        "result": 43
    },
    {
        "sid": "A6-gen-03",
        "group": "A6",
        "phase": "gen",
        "trial": 3,
        "agent": 32,
        "recipient": 43,
        "indicator": 1,
        "result": 44
    },
    {
        "sid": "A6-gen-04",
        "group": "A6",
        "phase": "gen",
        "trial": 4,
        "agent": 32,
        "recipient": 61,
        "indicator": 1,
        "result": 42
    },
    {
        "sid": "A6-gen-05",
        "group": "A6",
        "phase": "gen",
        "trial": 5,
        "agent": 41,
        "recipient": 32,
        "indicator": 1,
        "result": 53
    },
    {
        "sid": "A6-gen-06",
        "group": "A6",
        "phase": "gen",
        "trial": 6,
        "agent": 41,
        "recipient": 42,
        "indicator": 1,
        "result": 53
    },
    {
        "sid": "A6-gen-07",
        "group": "A6",
        "phase": "gen",
        "trial": 7,
        "agent": 41,
        "recipient": 43,
        "indicator": 1,
        "result": 54
    },
    {
        "sid": "A6-gen-08",
        "group": "A6",
        "phase": "gen",
        "trial": 8,
        "agent": 41,
        "recipient": 61,
        "indicator": 1,
        "result": 52
    },
    {
        "sid": "A6-gen-09",
        "group": "A6",
        "phase": "gen",
        "trial": 9,
        "agent": 51,
        "recipient": 32,
        "indicator": 1,
        "result": 63
    },
    {
        "sid": "A6-gen-10",
        "group": "A6",
        "phase": "gen",
        "trial": 10,
        "agent": 51,
        "recipient": 42,
        "indicator": 1,
        "result": 63
    },
    {
        "sid": "A6-gen-11",
        "group": "A6",
        "phase": "gen",
        "trial": 11,
        "agent": 51,
        "recipient": 43,
        "indicator": 1,
        "result": 64
    },
    {
        "sid": "A6-gen-12",
        "group": "A6",
        "phase": "gen",
        "trial": 12,
        "agent": 51,
        "recipient": 61,
        "indicator": 1,
        "result": 62
    },
    {
        "sid": "A6-gen-13",
        "group": "A6",
        "phase": "gen",
        "trial": 13,
        "agent": 63,
        "recipient": 32,
        "indicator": 1,
        "result": 73
    },
    {
        "sid": "A6-gen-14",
        "group": "A6",
        "phase": "gen",
        "trial": 14,
        "agent": 63,
        "recipient": 42,
        "indicator": 1,
        "result": 73
    },
    {
        "sid": "A6-gen-15",
        "group": "A6",
        "phase": "gen",
        "trial": 15,
        "agent": 63,
        "recipient": 43,
        "indicator": 1,
        "result": 74
    },
    {
        "sid": "A6-gen-16",
        "group": "A6",
        "phase": "gen",
        "trial": 16,
        "agent": 63,
        "recipient": 61,
        "indicator": 1,
        "result": 72
    },
    {
        "sid": "A7-learn-01",
        "group": "A7",
        "phase": "learn",
        "trial": 1,
        "agent": 31,
        "recipient": 42,
        "indicator": 0,
        "result": 43
    },
    {
        "sid": "A7-learn-02",
        "group": "A7",
        "phase": "learn",
        "trial": 2,
        "agent": 33,
        "recipient": 42,
        "indicator": 0,
        "result": 43
    },
    {
        "sid": "A7-learn-03",
        "group": "A7",
        "phase": "learn",
        "trial": 3,
        "agent": 52,
        "recipient": 42,
        "indicator": 0,
        "result": 63
    },
    {
        "sid": "A7-learn-04",
        "group": "A7",
        "phase": "learn",
        "trial": 4,
        "agent": 53,
        "recipient": 42,
        "indicator": 0,
        "result": 63
    },
    {
        "sid": "A7-learn-05",
        "group": "A7",
        "phase": "learn",
        "trial": 5,
        "agent": 61,
        "recipient": 42,
        "indicator": 0,
        "result": 73
    },
    {
        "sid": "A7-learn-06",
        "group": "A7",
        "phase": "learn",
        "trial": 6,
        "agent": 62,
        "recipient": 42,
        "indicator": 0,
        "result": 73
    },
    {
        "sid": "A7-gen-01",
        "group": "A7",
        "phase": "gen",
        "trial": 1,
        "agent": 32,
        "recipient": 32,
        "indicator": 0,
        "result": 43
    },
    {
        "sid": "A7-gen-02",
        "group": "A7",
        "phase": "gen",
        "trial": 2,
        "agent": 32,
        "recipient": 42,
        "indicator": 0,
        "result": 43
    },
    {
        "sid": "A7-gen-03",
        "group": "A7",
        "phase": "gen",
        "trial": 3,
        "agent": 32,
        "recipient": 43,
        "indicator": 0,
        "result": 44
    },
    {
        "sid": "A7-gen-04",
        "group": "A7",
        "phase": "gen",
        "trial": 4,
        "agent": 32,
        "recipient": 61,
        "indicator": 0,
        "result": 42
    },
    {
        "sid": "A7-gen-05",
        "group": "A7",
        "phase": "gen",
        "trial": 5,
        "agent": 41,
        "recipient": 32,
        "indicator": 0,
        "result": 53
    },
    {
        "sid": "A7-gen-06",
        "group": "A7",
        "phase": "gen",
        "trial": 6,
        "agent": 41,
        "recipient": 42,
        "indicator": 0,
        "result": 53
    },
    {
        "sid": "A7-gen-07",
        "group": "A7",
        "phase": "gen",
        "trial": 7,
        "agent": 41,
        "recipient": 43,
        "indicator": 0,
        "result": 54
    },
    {
        "sid": "A7-gen-08",
        "group": "A7",
        "phase": "gen",
        "trial": 8,
        "agent": 41,
        "recipient": 61,
        "indicator": 0,
        "result": 52
    },
    {
        "sid": "A7-gen-09",
        "group": "A7",
        "phase": "gen",
        "trial": 9,
        "agent": 51,
        "recipient": 32,
        "indicator": 0,
        "result": 63
    },
    {
        "sid": "A7-gen-10",
        "group": "A7",
        "phase": "gen",
        "trial": 10,
        "agent": 51,
        "recipient": 42,
        "indicator": 0,
        "result": 63
    },
    {
        "sid": "A7-gen-11",
        "group": "A7",
        "phase": "gen",
        "trial": 11,
        "agent": 51,
        "recipient": 43,
        "indicator": 0,
        "result": 64
    },
    {
        "sid": "A7-gen-12",
        "group": "A7",
        "phase": "gen",
        "trial": 12,
        "agent": 51,
        "recipient": 61,
        "indicator": 0,
        "result": 62
    },
    {
        "sid": "A7-gen-13",
        "group": "A7",
        "phase": "gen",
        "trial": 13,
        "agent": 63,
        "recipient": 32,
        "indicator": 0,
        "result": 73
    },
    {
        "sid": "A7-gen-14",
        "group": "A7",
        "phase": "gen",
        "trial": 14,
        "agent": 63,
        "recipient": 42,
        "indicator": 0,
        "result": 73
    },
    {
        "sid": "A7-gen-15",
        "group": "A7",
        "phase": "gen",
        "trial": 15,
        "agent": 63,
        "recipient": 43,
        "indicator": 0,
        "result": 74
    },
    {
        "sid": "A7-gen-16",
        "group": "A7",
        "phase": "gen",
        "trial": 16,
        "agent": 63,
        "recipient": 61,
        "indicator": 0,
        "result": 72
    },
    {
        "sid": "A8-learn-01",
        "group": "A8",
        "phase": "learn",
        "trial": 1,
        "agent": 31,
        "recipient": 42,
        "indicator": 1,
        "result": 43
    },
    {
        "sid": "A8-learn-02",
        "group": "A8",
        "phase": "learn",
        "trial": 2,
        "agent": 33,
        "recipient": 42,
        "indicator": 1,
        "result": 43
    },
    {
        "sid": "A8-learn-03",
        "group": "A8",
        "phase": "learn",
        "trial": 3,
        "agent": 52,
        "recipient": 42,
        "indicator": 1,
        "result": 63
    },
    {
        "sid": "A8-learn-04",
        "group": "A8",
        "phase": "learn",
        "trial": 4,
        "agent": 53,
        "recipient": 42,
        "indicator": 1,
        "result": 63
    },
    {
        "sid": "A8-learn-05",
        "group": "A8",
        "phase": "learn",
        "trial": 5,
        "agent": 61,
        "recipient": 42,
        "indicator": 1,
        "result": 73
    },
    {
        "sid": "A8-learn-06",
        "group": "A8",
        "phase": "learn",
        "trial": 6,
        "agent": 62,
        "recipient": 42,
        "indicator": 1,
        "result": 73
    },
    {
        "sid": "A8-gen-01",
        "group": "A8",
        "phase": "gen",
        "trial": 1,
        "agent": 32,
        "recipient": 32,
        "indicator": 1,
        "result": 43
    },
    {
        "sid": "A8-gen-02",
        "group": "A8",
        "phase": "gen",
        "trial": 2,
        "agent": 32,
        "recipient": 42,
        "indicator": 1,
        "result": 43
    },
    {
        "sid": "A8-gen-03",
        "group": "A8",
        "phase": "gen",
        "trial": 3,
        "agent": 32,
        "recipient": 43,
        "indicator": 1,
        "result": 44
    },
    {
        "sid": "A8-gen-04",
        "group": "A8",
        "phase": "gen",
        "trial": 4,
        "agent": 32,
        "recipient": 61,
        "indicator": 1,
        "result": 42
    },
    {
        "sid": "A8-gen-05",
        "group": "A8",
        "phase": "gen",
        "trial": 5,
        "agent": 41,
        "recipient": 32,
        "indicator": 1,
        "result": 53
    },
    {
        "sid": "A8-gen-06",
        "group": "A8",
        "phase": "gen",
        "trial": 6,
        "agent": 41,
        "recipient": 42,
        "indicator": 1,
        "result": 53
    },
    {
        "sid": "A8-gen-07",
        "group": "A8",
        "phase": "gen",
        "trial": 7,
        "agent": 41,
        "recipient": 43,
        "indicator": 1,
        "result": 54
    },
    {
        "sid": "A8-gen-08",
        "group": "A8",
        "phase": "gen",
        "trial": 8,
        "agent": 41,
        "recipient": 61,
        "indicator": 1,
        "result": 52
    },
    {
        "sid": "A8-gen-09",
        "group": "A8",
        "phase": "gen",
        "trial": 9,
        "agent": 51,
        "recipient": 32,
        "indicator": 1,
        "result": 63
    },
    {
        "sid": "A8-gen-10",
        "group": "A8",
        "phase": "gen",
        "trial": 10,
        "agent": 51,
        "recipient": 42,
        "indicator": 1,
        "result": 63
    },
    {
        "sid": "A8-gen-11",
        "group": "A8",
        "phase": "gen",
        "trial": 11,
        "agent": 51,
        "recipient": 43,
        "indicator": 1,
        "result": 64
    },
    {
        "sid": "A8-gen-12",
        "group": "A8",
        "phase": "gen",
        "trial": 12,
        "agent": 51,
        "recipient": 61,
        "indicator": 1,
        "result": 62
    },
    {
        "sid": "A8-gen-13",
        "group": "A8",
        "phase": "gen",
        "trial": 13,
        "agent": 63,
        "recipient": 32,
        "indicator": 1,
        "result": 73
    },
    {
        "sid": "A8-gen-14",
        "group": "A8",
        "phase": "gen",
        "trial": 14,
        "agent": 63,
        "recipient": 42,
        "indicator": 1,
        "result": 73
    },
    {
        "sid": "A8-gen-15",
        "group": "A8",
        "phase": "gen",
        "trial": 15,
        "agent": 63,
        "recipient": 43,
        "indicator": 1,
        "result": 74
    },
    {
        "sid": "A8-gen-16",
        "group": "A8",
        "phase": "gen",
        "trial": 16,
        "agent": 63,
        "recipient": 61,
        "indicator": 1,
        "result": 72
    }
]