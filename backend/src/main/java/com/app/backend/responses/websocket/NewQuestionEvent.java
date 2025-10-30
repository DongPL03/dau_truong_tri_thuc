package com.app.backend.responses.websocket;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class NewQuestionEvent {
    private String type;        // "NEW_QUESTION"
    private Long tranDauId;
    private int questionIndex;  // 0-based
    private QuestionView question;
    private int thoiGianCauGiay;

    @Data
    @Builder
    public static class QuestionView {
        private Long id;
        private String noiDung;
        private String luaChonA;
        private String luaChonB;
        private String luaChonC;
        private String luaChonD;
    }
}
