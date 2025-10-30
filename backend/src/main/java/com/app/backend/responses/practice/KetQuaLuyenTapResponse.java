package com.app.backend.responses.practice;

import com.app.backend.models.PhienLuyenTap;
import com.app.backend.models.TraLoiLuyenTap;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.stream.Collectors;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class KetQuaLuyenTapResponse {
    private Long phienId;
    private String boCauHoi;
    private Integer tongCauHoi;
    private Integer soCauDung;
    private Integer diemSo;
    private BigDecimal doChinhXac;
    private Integer thoiGianTbMs;
    private LocalDateTime taoLuc;
    private List<AnswerDetail> chiTiet;

    @Getter
    @Setter
    @AllArgsConstructor
    @NoArgsConstructor
    @Builder
    public static class AnswerDetail {
        private Long cauHoiId;
        private String noiDung;
        private Character dapAnDung;
        private Character luaChon;
        private Boolean dungHaySai;
        private Integer thoiGianMs;
    }

    public static KetQuaLuyenTapResponse from(PhienLuyenTap phien, List<TraLoiLuyenTap> traLois) {
        return KetQuaLuyenTapResponse.builder()
                .phienId(phien.getId())
                .boCauHoi(phien.getBoCauHoi().getTieuDe())
                .tongCauHoi(phien.getTongCauHoi())
                .soCauDung(phien.getSoCauDung())
                .diemSo(phien.getDiemSo())
                .doChinhXac(phien.getDoChinhXac())
                .thoiGianTbMs(phien.getThoiGianTbMs())
                .taoLuc(phien.getTaoLuc())
                .chiTiet(traLois.stream().map(t ->
                        AnswerDetail.builder()
                                .cauHoiId(t.getCauHoi().getId())
                                .noiDung(t.getCauHoi().getNoiDung())
                                .dapAnDung(t.getCauHoi().getDapAnDung())
                                .luaChon(t.getLuaChon())
                                .dungHaySai(t.getDungHaySai())
                                .thoiGianMs(t.getThoiGianMs())
                                .build()
                ).collect(Collectors.toList()))
                .build();
    }

}
