package com.app.backend.responses.trandau;

import com.app.backend.models.CauHoi;
import com.app.backend.models.TranDau;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.List;

@Data
@Builder
public class BattleStartResponse {
    @JsonProperty("tran_dau_id")
    private Long tranDauId;
    @JsonProperty("ten_phong")
    private String tenPhong;
    @JsonProperty("ma_phong")
    private String maPhong;
    @JsonProperty("bat_dau_luc")
    private LocalDateTime batDauLuc;
    @JsonProperty("tong_cau_hoi")
    private int tongCauHoi;
    @JsonProperty("cau_hoi_list")
    private List<QuestionView> cauHoiList;

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

    public static BattleStartResponse from(TranDau td, List<CauHoi> questions) {
        return BattleStartResponse.builder()
                .tranDauId(td.getId())
                .tenPhong(td.getTenPhong())
                .maPhong(td.getMaPhong())
                .batDauLuc(td.getBatDauLuc())
                .tongCauHoi(questions.size())
                .cauHoiList(
                        questions.stream()
                                .map(q -> QuestionView.builder()
                                        .id(q.getId())
                                        .noiDung(q.getNoiDung())
                                        .luaChonA(q.getLuaChonA())
                                        .luaChonB(q.getLuaChonB())
                                        .luaChonC(q.getLuaChonC())
                                        .luaChonD(q.getLuaChonD())
                                        .build())
                                .toList()
                )
                .build();
    }
}
