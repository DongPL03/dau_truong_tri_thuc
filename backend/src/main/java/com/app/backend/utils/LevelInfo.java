package com.app.backend.utils;

import lombok.AllArgsConstructor;
import lombok.Getter;

@AllArgsConstructor
@Getter
public class LevelInfo {
    private int level;
    private long xpInLevel;
    private long xpToNext;
    private double progressPercent;
}
