package com.app.backend.responses.websocket;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class ScoreUpdateEvent {
    private String type;       // "SCORE_UPDATE"
    private Long tranDauId;
    private Long userId;
    private String hoTen;
    private boolean correct;
    private int gainedPoints;
    private int totalPoints;
    private int questionIndex;
}

