package com.app.backend.responses.practice;

import com.app.backend.models.PhienLuyenTap;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class LichSuLuyenTapItem {
    private Long phienId;
    private String boCauHoi;
    private Integer tongCauHoi;
    private Integer soCauDung;
    private Integer diemSo;
    private BigDecimal doChinhXac;
    private Integer thoiGianTbMs;
    private LocalDateTime ngayTao;
    private UserInfo nguoiDung;

    @Getter
    @Setter
    @AllArgsConstructor
    @NoArgsConstructor
    @Builder
    public static class UserInfo {
        private Long id;
        private String ten;
    }

    public static LichSuLuyenTapItem from(PhienLuyenTap p) {
        return LichSuLuyenTapItem.builder()
                .phienId(p.getId())
                .boCauHoi(p.getBoCauHoi().getTieuDe())
                .tongCauHoi(p.getTongCauHoi())
                .soCauDung(p.getSoCauDung())
                .diemSo(p.getDiemSo())
                .doChinhXac(p.getDoChinhXac())
                .thoiGianTbMs(p.getThoiGianTbMs())
                .ngayTao(p.getTaoLuc())
                .nguoiDung(UserInfo.builder()
                        .id(p.getNguoiDung().getId())
                        .ten(p.getNguoiDung().getHoTen())
                        .build())
                .build();
    }

}
