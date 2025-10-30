package com.app.backend.responses.practice;

import com.app.backend.models.CauHoi;
import com.app.backend.models.PhienLuyenTap;
import lombok.*;

import java.util.List;
import java.util.stream.Collectors;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class BatDauLuyenTapResponse {
    private Long phienId;
    private String boCauHoi;
    private Integer tongCauHoi;
    private List<CauHoiItem> cauHoiList;

    @Getter
    @Setter
    @AllArgsConstructor
    @NoArgsConstructor
    @Builder
    public static class CauHoiItem {
        private Long id;
        private String noiDung;
        private String luaChonA;
        private String luaChonB;
        private String luaChonC;
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