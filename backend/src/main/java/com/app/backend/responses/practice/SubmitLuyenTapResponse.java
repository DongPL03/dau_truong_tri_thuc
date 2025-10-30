package com.app.backend.responses.practice;

import com.app.backend.models.PhienLuyenTap;
import com.app.backend.models.TraLoiLuyenTap;
import lombok.*;

import java.math.BigDecimal;
import java.util.List;
import java.util.stream.Collectors;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class SubmitLuyenTapResponse {
    private Long phienId;
    private Integer tongSoCau;
    private Integer soCauDung;
    private Integer diemSo;
    private BigDecimal doChinhXac;
    private Integer thoiGianTbMs;
    private List<AnswerResult> chiTiet;

    @Getter @Setter @AllArgsConstructor @NoArgsConstructor @Builder
    public static class AnswerResult {
        private Long cauHoiId;
        private Character luaChon;
        private Boolean dungHaySai;
        private Integer thoiGianMs;
    }

    public static SubmitLuyenTapResponse from(PhienLuyenTap phien, List<TraLoiLuyenTap> traLois) {
        return SubmitLuyenTapResponse.builder()
                .phienId(phien.getId())
                .tongSoCau(phien.getTongCauHoi())
                .soCauDung(phien.getSoCauDung())
                .diemSo(phien.getDiemSo())
                .doChinhXac(phien.getDoChinhXac())
                .thoiGianTbMs(phien.getThoiGianTbMs())
                .chiTiet(traLois.stream().map(t ->
                        AnswerResult.builder()
                                .cauHoiId(t.getCauHoi().getId())
                                .luaChon(t.getLuaChon())
                                .dungHaySai(t.getDungHaySai())
                                .thoiGianMs(t.getThoiGianMs())
                                .build()
                ).collect(Collectors.toList()))
                .build();
    }
}
