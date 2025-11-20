package com.app.backend.responses.websocket;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;

import java.time.Instant;

@Data
@Builder
public class NewQuestionEvent {
    @JsonProperty("type")
    private String type;        // "NEW_QUESTION"

    @JsonProperty("tran_dau_id")
    private Long tranDauId;

    @JsonProperty("question_index")
    private int questionIndex;  // 0-based

    @JsonProperty("question")
    private QuestionView question;

    @JsonProperty("timestamp")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant timestamp;

    @JsonProperty("thoi_gian_cau_giay")
    private int thoiGianCauGiay;

    @Data
    @Builder
    public static class QuestionView {
        @JsonProperty("id")
        private Long id;
        @JsonProperty("noi_dung")
        private String noiDung;
        @JsonProperty("loai_noi_dung")
        private String loaiNoiDung;
        @JsonProperty("duong_dan_tep")
        private String duongDanTep;
        @JsonProperty("lua_chon_a")
        private String luaChonA;
        @JsonProperty("lua_chon_b")
        private String luaChonB;
        @JsonProperty("lua_chon_c")
        private String luaChonC;
        @JsonProperty("lua_chon_d")
        private String luaChonD;
    }
}
