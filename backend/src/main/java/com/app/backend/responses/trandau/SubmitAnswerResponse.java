package com.app.backend.responses.trandau;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;
import lombok.Data;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Builder
public class SubmitAnswerResponse {

    @JsonProperty("correct")
    private boolean correct;

    @JsonProperty("gained_points")
    private int gainedPoints;       // điểm nhận được ở câu này

    @JsonProperty("total_points")
    private int totalPoints;        // tổng điểm mới của người chơi

    @JsonProperty("question_index")
    private int questionIndex;      // index hiện tại (0-based)
}

