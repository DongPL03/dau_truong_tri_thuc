package com.app.backend.responses.admin;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

// QuestionAnswersAdminResponse.java
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class QuestionAnswersAdminResponse {
    @JsonProperty("tran_dau_id")
    private Long tranDauId;
    @JsonProperty("cau_hoi_id")
    private Long cauHoiId;

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
    @JsonProperty("dap_an_dung")
    private Character dapAnDung;

    @JsonProperty("nguoi_choi")
    private List<PlayerAnswerRow> nguoiChoi;

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class PlayerAnswerRow {
        @JsonProperty("user_id")
        private Long userId;
        @JsonProperty("ho_ten")
        private String hoTen;
        @JsonProperty("lua_chon")
        private Character luaChon;
        @JsonProperty("dung_hay_sai")
        private Boolean dungHaySai;
        @JsonProperty("thoi_gian_ms")
        private Integer thoiGianMs;
    }
}
