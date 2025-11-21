package com.app.backend.responses.practice;

import com.app.backend.models.CauHoi;
import com.app.backend.models.PhienLuyenTap;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.util.List;
import java.util.stream.Collectors;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class BatDauLuyenTapResponse {
    @JsonProperty("phien_id")
    private Long phienId;

    @JsonProperty("bo_cau_hoi")
    private String boCauHoi;

    @JsonProperty("tong_cau_hoi")
    private Integer tongCauHoi;

    @JsonProperty("cau_hoi_list")
    private List<CauHoiItem> cauHoiList;

    @Getter
    @Setter
    @AllArgsConstructor
    @NoArgsConstructor
    @Builder
    public static class CauHoiItem {
        @JsonProperty("id")
        private Long id;
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
    }

    public static BatDauLuyenTapResponse from(PhienLuyenTap phien, List<CauHoi> selected) {
        return BatDauLuyenTapResponse.builder()
                .phienId(phien.getId())
                .boCauHoi(phien.getBoCauHoi().getTieuDe())
                .tongCauHoi(selected.size())
                .cauHoiList(selected.stream().map(c ->
                        CauHoiItem.builder()
                                .id(c.getId())
                                .noiDung(c.getNoiDung())
                                .luaChonA(c.getLuaChonA())
                                .luaChonB(c.getLuaChonB())
                                .luaChonC(c.getLuaChonC())
                                .luaChonD(c.getLuaChonD())
                                .build()
                ).collect(Collectors.toList()))
                .build();
    }

}