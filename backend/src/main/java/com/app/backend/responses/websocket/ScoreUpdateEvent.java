package com.app.backend.responses.websocket;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;

import java.time.Instant;

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
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant timestamp;

    @JsonProperty("gained_points")
    private int gainedPoints;

    @JsonProperty("total_points")
    private int totalPoints;

    @JsonProperty("question_index")
    private int questionIndex;

    @JsonProperty("combo_streak")
    private int comboStreak;

    @JsonProperty("combo_bonus")
    private int comboBonus;

    @JsonProperty("combo_multiplier")
    private double comboMultiplier;

}