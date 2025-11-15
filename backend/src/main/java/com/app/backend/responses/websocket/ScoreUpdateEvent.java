package com.app.backend.responses.websocket;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;

@Data
@Builder
public class ScoreUpdateEvent {
    @JsonProperty("type")
    private String type;       // "SCORE_UPDATE"
    @JsonProperty("tran_dau_id")
    private Long tranDauId;
    @JsonProperty("user_id")
    private Long userId;
    @JsonProperty("ho_ten")
    private String hoTen;
    @JsonProperty("correct")
    private boolean correct;
    @JsonProperty("timestamp")
    private LocalDateTime timestamp;
    @JsonProperty("gained_points")
    private int gainedPoints;
    @JsonProperty("total_points")
    private int totalPoints;
    @JsonProperty("question_index")
    private int questionIndex;
}