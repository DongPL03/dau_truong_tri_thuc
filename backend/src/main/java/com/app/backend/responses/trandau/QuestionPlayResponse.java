package com.app.backend.responses.trandau;

import com.app.backend.models.CauHoi;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;
import lombok.*;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Builder
public class QuestionPlayResponse {

    @JsonProperty("cau_hoi_id")
    private Long cauHoiId;

    @JsonProperty("noi_dung")
    private String noiDung;

    @JsonProperty("lua_chon_a")
    private String luaChonA;

    @JsonProperty("lua_chon_b")
    private String luaChonB;

    @JsonProperty("lua_chon_c")
    private String luaChonC;

    @JsonProperty("lua_chon_d")
    private String luaChonD;

    /**
     * tổng thời gian cho câu (giây) để client hiển thị đếm ngược
     */
    @JsonProperty("thoi_gian_cau_giay")
    private int thoiGianCauGiay;

    public static QuestionPlayResponse from(CauHoi q, int seconds) {
        return QuestionPlayResponse.builder()
                .cauHoiId(q.getId())
                .noiDung(q.getNoiDung())
                .luaChonA(q.getLuaChonA())
                .luaChonB(q.getLuaChonB())
                .luaChonC(q.getLuaChonC())
                .luaChonD(q.getLuaChonD())
                .thoiGianCauGiay(seconds)
                .build();
    }
}